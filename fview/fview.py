# -*- coding: utf-8 -*-

from __future__ import print_function

import re
import os


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
    def __init__(self, lines):
        self._lines = tuple(lines)
        self.line_cursor = 0
        self.stack = FileStack()

    def step(self):
        if not self.has_next():
            raise StopIteration

        line = self._lines[self.line_cursor]
        self.line_cursor += 1
        self.stack.step()
        return line

    def peek(self):
        return self._lines[self.line_cursor]

    def __iter__(self):
        while True:
            yield self.step()
        raise StopIteration

    def has_next(self):
        return self.line_cursor < len(self._lines) - 1

    def __len__(self):
        return len(self._lines)

    def __getitem__(self, key):
        return self._lines.__getitem__(key)


class FortranSyntaxHandler(object):

    @staticmethod
    def is_comment(line):
        return len(line.lstrip()) > 0 and line.lstrip()[0] == '!'

    @staticmethod
    def is_whitespace(line):
        return len(line.strip()) == 0

    @staticmethod
    def is_subroutine_def(line):
        return len(line) > 0 and line.lstrip().upper()[:11] == 'SUBROUTINE '

    @staticmethod
    def is_module_def(line):
        return len(line) > 0 and line.lstrip().upper()[:7] == 'MODULE '

    @staticmethod
    def is_exit_context(line, context_type, context_name):
        return (
            (line.strip().upper() == 'END') or
            (re.match(
                r'END\s*{}(\s*{})?\s*$'.format(
                    re.escape(context_type),
                    re.escape(context_name)),
                line.strip(), re.I)))


class FortranParser(FortranSyntaxHandler):
    def __init__(self, manager):
        super(FortranParser, self).__init__()
        self._manager = manager

    @property
    def manager(self):
        return self._manager

    def source(self):
        return ''.join(self._manager[self.start:self.end])

    def parse(self):
        raise NotImplementedError


class CodeBlock(FortranParser):

    def __init__(self, manager):
        super(CodeBlock, self).__init__(manager)
        self.start = self.manager.line_cursor
        self.end = None
        self.manager.stack.increment(self)

    def exit(self):
        raise NotImplementedError


class HeaderBlock(CodeBlock):
    CodeType = 'Header'

    def __repr__(self):
        return '<Header>'

    def parse(self):

        while self.is_comment(self.manager.peek()):
            self.manager.step()

        self.end = self.manager.line_cursor
        self.manager.stack.decrement()


class CommentBlock(CodeBlock):
    CodeType = 'Comment'

    def __repr__(self):
        return '<Comment>'

    def parse(self):
        while (self.is_whitespace(self.manager.peek()) or
                self.is_comment(self.manager.peek())):

            self.manager.step()

        self.end = self.manager.line_cursor
        self.manager.stack.decrement()

    def exit(self):
        nextline = self.manager.peek()
        return not (self.is_whitespace(nextline) or self.is_comment(nextline))


class Context(CodeBlock):
    CodeType = NotImplemented

    def __init__(self, manager, header=None):
        super(Context, self).__init__(manager)

        if header is None:
            header = HeaderBlock(manager)
            header.end = manager.line_cursor

        self.header = header
        self.context_name = None
        self.contents = []

    def handle(self):

        while self.manager.has_next() and not self.exit():

            comment = None

            line = self.manager.peek()

            if self.is_whitespace(line):
                self.manager.step()
                continue

            if self.is_comment(line):
                comment = CommentBlock(self.manager)
                comment.parse()

                line = self.manager.peek()

            if self.is_subroutine_def(line):
                subroutine = Subroutine(self.manager, header=comment)
                self.contents.append(subroutine)
                subroutine.parse()
                continue

            if self.is_module_def(line):
                module = Module(self.manager, header=comment)
                self.contents.append(module)
                module.parse()
                continue

            self.manager.step()

        self.manager.stack.decrement()
        self.end = self.manager.line_cursor

    def parse(self):
        line = self.manager.step()
        defn_parser = re.match(
            r'\s*({})\s+(?P<name>\w+)([^\n]+)?'.format(
                re.escape(self.CodeType)),
            line,
            re.I)

        if not defn_parser:
            raise ValueError('Failed to match line {}: "{}"'.format(
                self.manager.line_cursor,
                line))

        self.context_name = defn_parser.group('name')

        self.manager.step()

        self.handle()

    def exit(self):
        return self.is_exit_context(
            self.manager.peek(),
            self.CodeType,
            self.context_name)

    def __repr__(self):
        return '<{}:{}>'.format(self.CodeType, self.context_name)

    def display(self):
        return '{t} {n}\n{l}\n{h}\n\n'.format(
            t=self.CodeType,
            n=self.context_name,
            l='='*50,
            h='' if self.header is None else self.header.source())


class Subroutine(Context):
    CodeType = 'Subroutine'


class Module(Context):
    CodeType = 'Module'


class File(Context):
    CodeType = 'FortranFile'

    def __init__(self, manager, filename='<file>'):
        super(File, self).__init__(manager, filename)
        self.end = len(self.manager)

    def parse(self):

        topline = self.manager.peek()
        if self.is_comment(topline) or self.is_whitespace(topline):
            self.header = HeaderBlock(self.manager)
            self.header.parse()

        self.handle()

    def exit(self):
        return False


def read(target):
    with open(target, 'r') as f:
        manager = FileManager(f.readlines())

    parser = File(manager, os.path.basename(target))
    parser.parse()

    print(parser.display())

    for sub in parser.contents:
        print(sub.display())

    return parser.manager.stack[8867]
