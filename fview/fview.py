# -*- coding: utf-8 -*-

from __future__ import print_function

import re
import os
import traceback
from collections import OrderedDict


class FileStack(object):
    def __init__(self):
        self._stack = [[]]

    def step(self):
        self._stack.append([s for s in self._stack[-1]])

    def increment(self, context):
        self._stack[-1].append(context)

    def decrement(self):
        return self._stack[-1].pop(-1)

    def __getitem__(self, key):
        return self._stack[key]


class FileManager(object):
    def __init__(
            self,
            program_manager,
            lines,
            basedir,
            filepath,
            include_paths):

        self.program_manager = program_manager

        self._lines = tuple(lines)
        self.line_cursor = 0
        self.stack = FileStack()

        self.basedir = basedir
        self.filepath = filepath
        self.include_paths = include_paths

    @property
    def lines(self):
        return self._lines

    def step(self):
        if not self.has_next():
            raise StopIteration

        line = self._lines[self.line_cursor]
        self.line_cursor += 1
        self.stack.step()
        return line

    def peek(self):
        try:
            return self._lines[self.line_cursor]
        except IndexError:
            raise IndexError(
                'Error reading line {} of file {}:\n'.format(
                    self.line_cursor, self.filepath, traceback.format_exc()))

    def __iter__(self):
        while True:
            yield self.step()
        raise StopIteration

    def has_next(self):
        return self.line_cursor < len(self._lines)

    def __len__(self):
        return len(self._lines)

    def __getitem__(self, key):
        return self._lines.__getitem__(key)


class ProgramManager(object):
    def __init__(self, basedir, include_paths, root_files):

        self.basedir = basedir
        self.include_paths = include_paths
        self.root_files = root_files

        self.files = {}

    def parse(self):
        for root in self.root_files:
            if root in self.files:
                continue

            parser = FortranFile(
                self,
                self.basedir,
                root,
                self.include_paths)

            parser.read()
            parser.parse()

            self.files[root] = parser

    def include(self, filepath):
        if filepath in self.files:
            return self.files[filepath]

        include = FortranFile(
            program_manager=self,
            basedir=self.basedir,
            filepath=filepath,
            include_paths=self.include_paths)

        include.read()
        include.parse()

        self.files[filepath] = include

        return include


class FortranSyntaxHandler(object):

    TYPES = [
        'INTEGER', 'INTEGER(1)', 'INTEGER(2)', 'INTEGER(4)', 'INTEGER(8)',
        'REAL', 'REAL(4)', 'DOUBLE', 'DOUBLE PRECISION', 'REAL(8)',
        'CHARACTER', 'LOGICAL', 'COMPLEX', 'COMPLEX(4)', 'DOUBLE COMPLEX',
        'COMPLEX(8)']

    @staticmethod
    def is_comment(line):
        return len(line.lstrip()) > 0 and (
            line.lstrip()[0] == '!' or line.lstrip().upper()[:2] == 'C ')

    @staticmethod
    def is_whitespace(line):
        return len(line.strip()) == 0

    @staticmethod
    def is_subroutine_def(line):
        return len(line) > 0 and line.lstrip().upper()[:11] == 'SUBROUTINE '

    @staticmethod
    def is_module_def(line):
        return len(line) > 0 and line.lstrip().upper()[:7] == 'MODULE '

    @classmethod
    def is_function_def(cls, line):
        return len(line) > 0 and re.match(
            '({})\s+FUNCTION\s+'.format('|'.join(map(re.escape, cls.TYPES))),
            line.lstrip(),
            re.I)

    @staticmethod
    def is_include(line):
        return len(line) > 0 and line.lstrip().upper()[:8] == 'INCLUDE '

    @staticmethod
    def is_exit_context(line, context_type, context_name):
        return (
            (line.strip().upper() == 'END') or
            (re.match(
                r'END\s*{}(\s*{})?\s*(\![^\n]*)?$'.format(
                    re.escape(context_type),
                    re.escape(context_name)),
                line.strip(), re.I)))


class FortranParser(FortranSyntaxHandler):
    def __init__(self, file_manager):
        super(FortranParser, self).__init__()
        self._file_manager = file_manager

    @property
    def file_manager(self):
        return self._file_manager

    def source(self):
        return ''.join(self.file_manager[self.start:self.end])

    def parse(self):
        raise NotImplementedError


class CodeBlock(FortranParser):

    def __init__(self, file_manager):
        super(CodeBlock, self).__init__(file_manager)
        self.start = self.file_manager.line_cursor
        self.end = None
        self.file_manager.stack.increment(self)

    def exit(self):
        raise NotImplementedError


class HeaderBlock(CodeBlock):
    CodeType = 'Header'

    def __repr__(self):
        return '<Header>'

    def parse(self):

        while self.file_manager.has_next() and not self.exit():
            self.file_manager.step()

        self.end = self.file_manager.line_cursor
        self.file_manager.stack.decrement()

    def exit(self):
        return not self.is_comment(self.file_manager.peek())


class CommentBlock(CodeBlock):
    CodeType = 'Comment'

    def __repr__(self):
        return '<Comment>'

    def parse(self):
        while self.file_manager.has_next() and not self.exit():
            self.file_manager.step()

        self.end = self.file_manager.line_cursor
        self.file_manager.stack.decrement()

    def exit(self):
        nextline = self.file_manager.peek()
        return not (self.is_whitespace(nextline) or self.is_comment(nextline))


class Context(CodeBlock):
    CodeType = NotImplemented

    def __init__(self, file_manager, header=None, context_name=None):
        super(Context, self).__init__(file_manager)

        if header is None:
            header = HeaderBlock(file_manager)
            header.end = file_manager.line_cursor

        self.header = header
        self.context_name = context_name
        self.contents = OrderedDict()

    def handle(self):

        while self.file_manager.has_next() and not self.exit():

            comment = None

            line = self.file_manager.peek()

            if self.is_whitespace(line):
                self.file_manager.step()
                continue

            if self.is_comment(line):
                comment = CommentBlock(self.file_manager)
                comment.parse()

                if self.file_manager.has_next():
                    line = self.file_manager.peek()
                else:
                    break

            if self.exit():
                break

            if self.is_include(line):
                filepath = re.match((
                    r'\s*include\s+(?P<q>[\'\"])?(?P<fp>[\w\.\-]+)' +
                    r'(?P=q)\s*(\![^\n]*)?(\n)?$'),
                    line,
                    re.I)

                if not filepath:
                    raise ValueError((
                        'Error in file "{}" line {}: ' +
                        'Include statement "{}" not understood').format(
                        self.file_manager.filepath,
                        self.file_manager.line_cursor,
                        line))

                filepath = filepath.group('fp')

                try:
                    include = self.file_manager.program_manager.include(
                        filepath)

                except OSError:
                    raise OSError((
                        'Error in file "{}" line {}: Include file "{}" ' +
                        'not found in include_paths path').format(
                            self.file_manager.filepath,
                            self.file_manager.line_cursor,
                            filepath))

                include.calls.append((
                    self.file_manager.filepath,
                    self.file_manager.line_cursor))

                self.contents[include.context_name] = include
                self.file_manager.stack.increment(include)
                self.file_manager.step()
                self.file_manager.stack.decrement()
                continue

            if self.is_subroutine_def(line):
                subroutine = Subroutine(self.file_manager, header=comment)
                subroutine.parse()
                self.contents[subroutine.context_name] = subroutine
                continue

            if self.is_module_def(line):
                module = Module(self.file_manager, header=comment)
                module.parse()
                self.contents[module.context_name] = module
                continue

            if self.is_function_def(line):
                function = Function(self.file_manager, header=comment)
                function.parse()
                self.contents[function.context_name] = function
                continue

            self.file_manager.step()

        self.file_manager.stack.decrement()
        self.end = self.file_manager.line_cursor

    def parse(self):
        line = self.file_manager.step()
        defn_parser = re.search(
            r'\s*({})\s+(?P<name>\w+)([^\n]+)?'.format(
                re.escape(self.CodeType)),
            line,
            re.I)

        if not defn_parser:
            raise ValueError('Failed to match line {}: "{}"'.format(
                self.file_manager.line_cursor,
                line))

        self.context_name = defn_parser.group('name')

        self.file_manager.step()

        self.handle()

    def exit(self):
        return self.is_exit_context(
            self.file_manager.peek(),
            self.CodeType,
            self.context_name)

    def __getitem__(self, key):
        return self.contents[key]

    def __repr__(self):
        return '<{}:{}>'.format(self.CodeType, self.context_name)

    def display(self):
        return '{t} {n} [{f}: {s}-{e}]\n{l}\n{h}\n\n'.format(
            t=self.CodeType,
            n=self.context_name,
            f=os.path.basename(self.file_manager.filepath),
            s=self.start+1,
            e=self.end+1,
            l='='*50,
            h='' if self.header is None else self.header.source())

    def display_contents(self):

        output = ''

        output += self.display()

        for sub in self.contents.values():
            output += sub.display()

        return output


class Subroutine(Context):
    CodeType = 'Subroutine'


class Module(Context):
    CodeType = 'Module'


class Function(Context):
    CodeType = 'Function'


class FileContext(Context):
    CodeType = 'FortranFile'

    def __init__(self, file_manager, context_name='<file>'):
        super(FileContext, self).__init__(
            file_manager, context_name=context_name)

        self.end = len(self.file_manager)

    def parse(self):

        if self.file_manager.has_next():
            topline = self.file_manager.peek()
            if self.is_comment(topline) or self.is_whitespace(topline):
                self.header = HeaderBlock(self.file_manager)
                self.header.parse()

        self.handle()

    def exit(self):
        return False


class FortranFile(object):
    def __init__(self, program_manager, basedir, filepath, include_paths):
        self.program_manager = program_manager
        self.basedir = basedir
        self.filepath = filepath
        self.include_paths = include_paths
        self.file_manager = None
        self.context = None

        self.calls = []

    def _get_lines(self):
        for inc in ['.'] + self.include_paths:
            fp = os.path.join(self.basedir, inc, self.filepath)
            if os.path.exists(fp):
                with open(fp, 'r') as f:
                    return tuple(f.readlines())

            if os.path.exists(os.path.splitext(fp)[0]):
                with open(os.path.splitext(fp)[0], 'r') as f:
                    return tuple(f.readlines())

        raise OSError

    @property
    def contents(self):
        return self.context.contents

    @property
    def context_name(self):
        return self.context.context_name

    def read(self):

        self.file_manager = FileManager(
            self.program_manager,
            self._get_lines(),
            self.basedir,
            self.filepath,
            self.include_paths)

    def parse(self):
        self.context = FileContext(
            self.file_manager,
            context_name=os.path.basename(self.filepath))

        self.context.parse()

    def __getitem__(self, key):
        return self.context.contents[key]

    def __repr__(self):
        return '<FortranFile:{}>'.format(self.filepath)

    def display(self):
        return self.context.display_contents()


def read(basedir, include_paths, root_files):
    if include_paths is None:
        include_paths = []

    program_manager = ProgramManager(basedir, include_paths, root_files)
    program_manager.parse()

    return program_manager
