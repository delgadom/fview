# -*- coding: utf-8 -*-

from __future__ import print_function

import re
import os
import traceback
from collections import OrderedDict


class UpperOrderedDict(OrderedDict):
    def __init__(self, *args, **kwargs):
        super(UpperOrderedDict, self).__init__()
        self.update(*args, **kwargs)

    def __getitem__(self, key):
        return super(UpperOrderedDict, self).__getitem__(key.upper())

    def __setitem__(self, key, value):
        return super(UpperOrderedDict, self).__setitem__(key.upper(), value)

    def __contains__(self, key):
        return super(UpperOrderedDict, self).__contains__(key.upper())

    def update(self, *args, **kwargs):
        for k, v in dict(*args, **kwargs).iteritems():
            self[k.upper()] = v


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

        self.files = UpperOrderedDict()

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

    TYPE_REGEX = '|'.join(map(re.escape, TYPES))
    NAME_REGEX = r'[a-zA-Z][\w\$]*'

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
            r'({types})\s+FUNCTION\s+'.format(types=cls.TYPE_REGEX),
            line.lstrip(),
            re.I)

    @staticmethod
    def is_include(line):
        return len(line) > 0 and line.lstrip().upper()[:8] == 'INCLUDE '

    @classmethod
    def is_variable_def(cls, line):
        return re.match((
            r'\s*({types})(\*[0-9]+)?(\((len=)?[0-9]+\))?' +
            r'(, (DIMENSION|PARAMETER|ALLOCATABLE))?(\s+|\s*::\s*)' +
            r'(?P<paren>\()?\s*(?P<declarations>(\s*(\,)?\s*{name}\s*' +
            r'(?P<dims>\(\s*({name}(\s*\:\s*{name})*)' +
            r'(\s*,\s*{name}(\s*\:\s*{name})*)*\s*\))?)+)\s*(?(paren)\))'
            ).format(
            types=cls.TYPE_REGEX, name=cls.NAME_REGEX), line, re.I)

    @staticmethod
    def has_continuation_chr(line):
        return re.search(
            r'^(?P<pre>\s*[^&!\n]*)(?P<post>&\s*(\![^\n]*)?(\n)?)$', line)

    @staticmethod
    def is_use_line(line):
        return len(line) > 0 and line.lstrip().upper()[:4] == 'USE '

    @classmethod
    def join_continue(cls, line, nextline):
        r'''

        Examples
        --------

        :py:meth:`~FortranSyntaxHandler.join_continue` joins multi-line
        statements one line at a time:

        .. code-block:: python

            >>> FortranSyntaxHandler.join_continue(
            ...     'INTEGER  Int1, & ! This integer is the first',
            ...     '         Int2    ! This integer is the second')
            'INTEGER  Int1, Int2    ! This integer is the second'

        If the second line is a comment, it will be skipped:

        .. code-block:: python

            >>> FortranSyntaxHandler.join_continue(
            ...     'REAL FUNCTION ARGMAX&',
            ...     '  ! This function takes the following arguments:')
            'REAL FUNCTION ARGMAX&'

        join_continue is intended to be used in conjunction with
        :py:meth:`~FortranSyntaxHandler.has_continuation_chr` and
        :py:meth:`~FortranSyntaxHandler.is_comment`. For example:

        .. code-block:: python

            >>> lines = [
            ...     '! The next line is a function definition\n',
            ...     'REAL FUNCTION PRODUCT&\n',
            ...     '    ! Here are the arguments\n',
            ...     '    &(ARG1, & ! first arg \n',
            ...     '      ARG2)   ! second arg\n',
            ...     '    PRODUCT = ARG1*ARG2\n',
            ...     '    RETURN\n',
            ...     'END FUNCTION PRODUCT\n'
            ... ]
            >>> parsed_lines = []
            >>> line_no = 0
            >>>
            >>> line = lines[line_no]
            >>> while True:
            ...     if ((FortranSyntaxHandler.has_continuation_chr(line)
            ...             or FortranSyntaxHandler.is_comment(line))
            ...             and line_no + 1 < len(lines)):
            ...         line_no += 1
            ...         next = lines[line_no]
            ...         line = FortranSyntaxHandler.join_continue(line, next)
            ...         continue
            ...
            ...     parsed_lines.append(line)
            ...     line_no += 1
            ...
            ...     if line_no == len(lines):
            ...         break
            ...
            ...     else:
            ...         line = lines[line_no]
            ...
            >>> print(''.join(parsed_lines))
            REAL FUNCTION PRODUCT(ARG1, ARG2)   ! second arg
                PRODUCT = ARG1*ARG2
                RETURN
            END FUNCTION PRODUCT
            <BLANKLINE>

        '''

        line1 = re.search(
            r'^(?P<pre>\s*[^\&\!\n]*)(?P<cont>(\&)?)(\s*(\![^\n]*)?(\n)?)$',
            line)

        if cls.is_comment(nextline):
            return line1.group('pre') + line1.group('cont')

        line2 = re.sub(r'^(\s*(\&)?)', '', nextline)

        return line1.group('pre') + line2

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


class Variable(FortranParser):

    def __init__(
            self,
            file_manager,
            variable_name,
            dims,
            typedef,
            definition,
            definition_file,
            definition_line):

        super(Variable, self).__init__(file_manager)
        self.variable_name = variable_name
        self.dims = dims
        self.typedef = typedef
        self.definition = definition
        self.definition_file = definition_file
        self.definition_line = definition_line

    @classmethod
    def parse_declartion_statement(cls, line, file_manager, full_statement):
        variables = []

        parsed = re.match((
            r'\s*(?P<typedef>{types})(\*[0-9]+)?(\((len=)?[0-9]+\))?' +
            r'(, (DIMENSION|PARAMETER|ALLOCATABLE))?(\s+|\s*::\s*)' +
            r'(?P<paren>\()?\s*(?P<declarations>(\s*(\,)?\s*{name}\s*' +
            r'(?P<dims>\(\s*({name}(\s*\:\s*{name})*)(\s*,\s*{name}' +
            r'(\s*\:\s*{name})*)*\s*\))?)+)\s*(?(paren)\))').format(
            types=cls.TYPE_REGEX, name=cls.NAME_REGEX), line, re.I)

        typedef = parsed.group('typedef')
        line_number = file_manager.line_cursor

        for parser in re.finditer((
                r'(?P<declaration>((?P<name>{name})\s*' +
                r'(?P<dims>\(\s*({name}(\s*\:\s*{name})*)' +
                r'(\s*,\s*{name}(\s*\:\s*{name})*)*\s*\))?))').format(
                types=cls.TYPE_REGEX, name=cls.NAME_REGEX),
                parsed.group('declarations'), re.I):

            name = parser.group('name')
            dims = parser.group('dims')

            variables.append(cls(
                file_manager=file_manager,
                variable_name=name,
                dims=dims,
                typedef=typedef,
                definition=full_statement,
                definition_file=file_manager.filepath,
                definition_line=line_number))

        return variables


class CodeBlock(FortranParser):

    def __init__(self, file_manager):
        super(CodeBlock, self).__init__(file_manager)
        self.start = self.file_manager.line_cursor
        self.end = None
        self.file_manager.stack.increment(self)

        self.includes = {}

    def exit(self, line=None):
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

    def exit(self, line=None):
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

    def exit(self, line=None):
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
        self.contents = UpperOrderedDict()
        self.uses = UpperOrderedDict()

    def handle(self):

        while self.file_manager.has_next() and not self.exit():

            comment = None

            # Get next line, but don't increment line_cursor. Incrementation is
            # done by the context block if we increment the stack.
            line = self.file_manager.peek()

            # Skip whitespace - not given a context
            if self.is_whitespace(line):
                self.file_manager.step()
                continue

            # Handle comments as unique statement types - we assign these as
            # context headers if they preceede a context block
            if self.is_comment(line):
                comment = CommentBlock(self.file_manager)
                comment.parse()

                if self.file_manager.has_next():
                    line = self.file_manager.peek()
                else:
                    break

            # Handle continuation characters ('&' at end of statement)
            # Since we've already parsed comments, if we encounter a
            # comment in this block it must be part of a multi-line statement.
            full_statement = line
            while (
                    self.file_manager.has_next() and (
                        self.has_continuation_chr(self.file_manager.peek()) or
                        self.is_comment(self.file_manager.peek()))):

                self.file_manager.step()
                line = self.join_continue(line, self.file_manager.peek())
                full_statement = full_statement + self.file_manager.peek()

            # Exit loop if this is an end of file or context-ending statement
            # This is handled here instead of with a continue statement to
            # allow comment blocks to be preserved and assigned as headers to
            # contexts if they are in the next line
            if (not self.file_manager.has_next()) or self.exit(line):
                break

            # Handle "include" statements by parsing nested files
            if self.is_include(line):

                # Find the name of the included filepath
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

                # Catch & re-raise import errors here so we can report the
                # troublesome file & line number of the include statement
                try:
                    include = IncludeFile(
                        including_file=self.file_manager.filepath,
                        including_line=self.file_manager.line_cursor,
                        filepath=filepath,
                        program_manager=self.file_manager.program_manager)

                except OSError:
                    raise OSError((
                        'Error in file "{}" line {}: Include file "{}" ' +
                        'not found in include_paths path').format(
                            self.file_manager.filepath,
                            self.file_manager.line_cursor,
                            filepath))

                # Register current position as a caller of the included file
                include.include_file.calls.append((
                    self.file_manager.filepath,
                    self.file_manager.line_cursor))

                # Register included file in current scope's contents
                self.includes[include.include_file.context_name] = dict(
                    line=self.file_manager.line_cursor,
                    include=include)

                # Move to the next line
                self.file_manager.step()

            # Handle subroutine definitions
            elif self.is_subroutine_def(line):
                subroutine = Subroutine(self.file_manager, header=comment)
                subroutine.parse()
                self.contents[subroutine.context_name] = subroutine

            # Handle module definitions
            elif self.is_module_def(line):
                module = Module(self.file_manager, header=comment)
                module.parse()
                self.contents[module.context_name] = module

            # Handle function definitions
            elif self.is_function_def(line):
                function = Function(self.file_manager, header=comment)
                function.parse()
                self.contents[function.context_name] = function

            # Handle variable definitions
            elif self.is_variable_def(line):
                defns = Variable.parse_declartion_statement(
                    line=line,
                    file_manager=self.file_manager,
                    full_statement=full_statement)

                for var in defns:
                    self.contents[var.variable_name] = var

                self.file_manager.step()

            elif self.is_use_line(line):
                module_name = re.match(r'\s*USE\s*(?P<mod>{name})'.format(
                    name=self.NAME_REGEX), line.upper(), re.I).group('mod')

                module = self.find_in_scope(
                    module_name, self.file_manager.line_cursor)

                self.uses[module_name] = module

                self.file_manager.step()

            # Catch all other statement types
            else:
                self.file_manager.step()

        # On context exit, decrement the stack and assign the current line
        # number as the end-line of the current context
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

        self.handle()

    def exit(self, line=None):
        return self.is_exit_context(
            line if line is not None else self.file_manager.peek(),
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

    def find_in_scope(self, name, line):
        '''
        Find an object within the current context

        Parameters
        ----------

        name : str

            name of the object to find within the current context

        line : int

            line number for object's usage

        Returns
        -------

        object : object

            a :py:class:`context` or :py:class:`Variable` object corresponding
            to the object in scope at the given line

        '''

        if line is None:
            line = self.end

        for context in reversed(self.file_manager.stack[line]):
            if name in context.contents:
                return context.contents[name]

            for modname, module in context.uses.items():
                in_module = module.find_in_scope(name)

                if in_module:
                    return in_module

            for incl_name, include in module.includes.items():
                if line >= include.including_line:
                    in_file = include.find_in_scope(name)
                    if in_file:
                        return in_file


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
                self.file_manager.stack.decrement()

        self.handle()

    def exit(self, line=None):
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

    def find_in_scope(self, name, line=None):
        '''
        Find an object within the current context

        Parameters
        ----------

        name : str

            name of the object to find within the current context

        line : int

            line number for object's usage

        Returns
        -------

        object : object

            a :py:class:`context` or :py:class:`Variable` object corresponding
            to the object in scope at the given line

        '''

        return self.context.find_in_scope(name, line)


class IncludeFile(object):
    def __init__(
            self,
            including_file,
            including_line,
            filepath,
            program_manager):

        self.include_file = program_manager.include(filepath)

        self.including_file = including_file
        self.including_line = including_line


def read(basedir, include_paths, root_files):
    if include_paths is None:
        include_paths = []

    program_manager = ProgramManager(basedir, include_paths, root_files)
    program_manager.parse()

    return program_manager
