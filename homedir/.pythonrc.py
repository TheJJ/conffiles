#!/usr/bin/env python3
#
# jj's pythonrc
#
# Copyright (c) 2012-2014 Jonas Jelten <jj@sft.mx>
#
# Licensed GPLv3 or later.

import inspect
import os
import sys

sys.ps1 = '\x01\x1b[36m\x02>>>\x01\x1b[m\x02 '
sys.ps2 = '\x01\x1b[36m\x02...\x01\x1b[m\x02 '

python_histfile = ".py_history"

if "PAGER" in os.environ:
    pager_proc = os.environ["PAGER"]
else:
    pager_proc = "less -R -S"

use_pygments = False
has_pygments = False


if use_pygments:
    try:
        import pygments
        import pygments.formatters
        import pygments.lexers
        has_pygments = True
    except ImportError:
        sys.stderr.write("pygments not found.\n")


def __pager_launch(txt):
    import subprocess
    import shlex
    pager = subprocess.Popen(shlex.split(pager_proc), stdin=subprocess.PIPE)
    pager.communicate(txt)
    pager.wait()


def dis(obj):
    """disassemble given stuff"""

    import dis
    __pager_launch(dis.dis(obj))


def src(obj):
    """Read the source of an object in the interpreter."""

    def highlight(source):
        if not use_pygments or not has_pygments:
            return source

        lexer = pygments.lexers.get_lexer_by_name('python')
        formatter = pygments.formatters.terminal.TerminalFormatter()
        return pygments.highlight(source, lexer, formatter)

    source = highlight(inspect.getsource(obj))
    __pager_launch(source)


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


try:
    _completion()
    del _completion
except Exception as e:
    sys.stderr.write("failed history and completion init: %s\n" % e)


try:
    # alternative for dir() to view members
    from see import see
except ImportError:
    pass
