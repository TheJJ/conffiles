#!/usr/bin/env python3
#
# jj's pythonrc
#
# Copyright (c) 2012-2018 Jonas Jelten <jj@sft.mx>
#
# Licensed GPLv3 or later.

# these imports are available in interactive python shells
import asyncio
import datetime
import inspect
import os
import pathlib
import re
import sys
import time
from math import *
from pathlib import Path
from pprint import pprint, pformat
from subprocess import call, run

try:
    # alternative for dir() to view members
    from see import see
except ImportError:
    pass

if 'bpython' not in sys.modules:
    # fancy prompt. bpython doesn't like nor need this
    # the \x01 and \x02 tell readline to ignore the chars
    sys.ps1 = '\x01\x1b[36m\x02>>>\x01\x1b[m\x02 '
    sys.ps2 = '\x01\x1b[36m\x02...\x01\x1b[m\x02 '

    # bpython has its own history management
    python_histfile = ".py_history"


pager_proc = os.environ.get("PAGER", "less -R -S")

use_pygments = True
has_pygments = False


# cython on the fly compilation
if False:
    try:
        import pyximport; pyximport.install()
    except ImportError:
        pass


if use_pygments:
    try:
        from pygments import highlight
        from pygments.formatters import TerminalFormatter
        import pygments.lexers
        has_pygments = True
    except ImportError:
        pass


def pager(txt):
    import subprocess, shlex
    if not isinstance(txt, bytes):
        txt = txt.encode()
    subprocess.run(shlex.split(pager_proc) + ['-'], input=txt)


def pager_file(filename):
    import subprocess, shlex
    subprocess.run(shlex.split(pager_proc) + [filename])


def dis(obj):
    """disassemble given stuff"""

    import dis
    import io

    output = io.StringIO()
    dis.dis(obj, file=output)
    pager(output.getvalue())
    output.close()



if use_pygments and has_pygments:
    def highlight(source):
        if not use_pygments or not has_pygments:
            return source

        lexer = pygments.lexers.get_lexer_by_name('python')
        formatter = TerminalFormatter(bg='dark')
        return pygments.highlight(source, lexer, formatter)

else:
    def highlight(txt):
        return txt


def src(obj):
    """Read the source of an object in the interpreter."""

    source = highlight(inspect.getsource(obj))
    pager(source)


def sh(*args, check=True, **kwargs):
    return run(args, check=check, **kwargs).returncode == 0


def _completion():
    import atexit
    import os
    import readline
    import rlcompleter

    readline.parse_and_bind('tab: complete')

    history_dir = os.path.join(os.path.expanduser('~'), python_histfile)

    if not os.path.exists(history_dir):
        os.makedirs(history_dir)

    # support multiple executables:
    # history files keyed by inode of the current py executable.
    executable_inode = str(os.stat(sys.executable).st_ino)
    history_file = os.path.join(history_dir, executable_inode)

    if os.path.exists(history_file):
        readline.read_history_file(history_file)
    atexit.register(readline.write_history_file, history_file)


# bpython has its own completion stuff
if 'bpython' not in sys.modules:
    try:
        _completion()
        del _completion
    except Exception as e:
        sys.stderr.write("failed history and completion init: %s\n" % e)


def _fancy_displayhook(item):
    if item is None:
        return

    global _
    _ = item

    if isinstance(item, int) and not isinstance(item, bool) and item > 0:
        if item >= 2**32:
            display_text = "{0}, 0x{0:x}".format(item)
        else:
            display_text = "{0}, 0x{0:x}, 0b{0:b}".format(item)
    else:
        display_text = pformat(item)

    output = highlight(display_text)
    if output.endswith("\n"):
        print(output, end="")
    else:
        print(output)


# install the hook
sys.displayhook = _fancy_displayhook
del _fancy_displayhook
