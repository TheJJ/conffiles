#!/usr/bin/env python3

"""
jj's pythonrc

Copyright (c) 2012-2022 Jonas Jelten <jj@sft.lol>
Licensed GPLv3 or later.
"""


# these imports are available in interactive python shells
import asyncio
import base64
import datetime
import importlib
import inspect
import io
import json
import math
import os
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
import time
import traceback

from pathlib import Path
from pprint import pprint, pformat
from subprocess import call, run

try:
    # alternative for dir() to view members
    from see import see
except ImportError:
    pass

PAGER_INVOCATION = os.environ.get("PAGER", "less -S -i -R -M --shift 5")
HISTSIZE = 50000

USE_PYGMENTS = True
HAS_PYGMENTS = False


# cython on the fly compilation
try:
    import pyximport
    pyximport.install(
        build_in_temp=False,
        reload_support=True,
        inplace=True,
        language_level=sys.version_info.major
    )
except ImportError:
    pass


if USE_PYGMENTS:
    try:
        import pygments
        from pygments.formatters import TerminalFormatter
        import pygments.lexers
        HAS_PYGMENTS = True
    except ImportError:
        pass


def pager(txt):
    if not isinstance(txt, bytes):
        txt = txt.encode()
    subprocess.run(shlex.split(PAGER_INVOCATION) + ['-'], input=txt)


def pager_file(filename):
    subprocess.run(shlex.split(PAGER_INVOCATION) + [filename])


def dis(obj):
    """disassemble given stuff"""

    import dis as pydis

    output = io.StringIO()
    pydis.dis(obj, file=output)
    pager(output.getvalue())
    output.close()


if USE_PYGMENTS and HAS_PYGMENTS:
    def highlight(source):
        if not USE_PYGMENTS or not HAS_PYGMENTS:
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


def loc(obj):
    """Get the definition location of give object."""
    srcfile = inspect.getsourcefile(obj)
    _, srcline = inspect.getsourcelines(obj)
    return "%s:%d" % (srcfile, srcline)


def cd(name):
    """Change the current directory to the given one."""
    os.chdir(name)


def pwd():
    """Return the current directory."""
    return os.getcwd()


def cat(name, binary=False, lines=False):
    """Read the given file and return its contents."""
    mode = "rb" if binary else "r"
    with open(name, mode) as fd:
        if lines:
            return fd.readlines()
        return fd.read()


def catln(name, binary=False):
    """Read the lines of the given file and return them."""
    return cat(name, binary, lines=True)


def ls(*args, recurse=False, merge=False):
    """
    List the current directory, or if given, all the files/directories.
    recurse: list contents recursively
    merge: don't return a dict entry for each listed, instead combine all results to one set
    """

    to_scan = list()

    if not args:
        to_scan.append(".")
    else:
        to_scan.extend(args)

    result = dict()

    if recurse:
        for inode in to_scan:
            result[inode] = os.walk(inode)
    else:
        for inode in to_scan:
            result[inode] = os.listdir(inode)

    if merge:
        if recurse:
            raise Exception("merge only available for non-recursive listings")

        result_set = set()
        for vals in result.values():
            result_set.update(vals)

        return result_set

    if len(to_scan) == 1:
        return result[to_scan[0]]

    return result


def sh(*args, check=True, **kwargs):
    """
    Execute the given commands and
    return True if the command exited with 0.
    """
    return run(args, check=check, **kwargs).returncode == 0


def stopwatch(average=False):
    """
    keyboard-driven stopwatch lol
    if average is True, print exponential moving average of all measurements.
    """
    import sys
    import termios
    import tty

    stdin = sys.stdin.fileno()
    tattr = termios.tcgetattr(stdin)

    try:
        tty.setcbreak(stdin, termios.TCSANOW)

        if not average:
            print('start')
            t = time.time()
        else:
            t = 0
            avg = None
            samples = 0

        while True:
            char = sys.stdin.buffer.read(1)
            if char == b'\x04':  # EOF
                break

            n = time.time()
            duration = n - t

            if not average:
                print(duration)
            else:
                if t == 0:
                    print('start with exponential smoothing')
                else:
                    samples += 1
                    if avg is None:
                        avg = duration
                    else:
                        smooth = 1 / samples
                        avg = smooth * duration + (1-smooth) * avg

                    print(avg)
            t = n
    except KeyboardInterrupt:
        print("ciao!")
    finally:
        termios.tcsetattr(stdin, termios.TCSANOW, tattr)


def timer(duration, display=True, interval=1.0, notify=True):
    """
    countdown function
    """
    import time
    start = time.time()

    while time.time() < start + duration:
        left = start + duration - time.time()
        if display:
            print(" %.02f\r" % left, end="")
        wait = (left % interval) or interval
        try:
            time.sleep(wait)
        except KeyboardInterrupt:
            break

    msg = "time of %.04fs has passed" % (time.time() - start)
    if display:
        print(msg)
    if notify:
        subprocess.run(["notify-send", msg])

    return (time.time() - start)


def _completion():
    """
    set up readline and history.
    supports parallel sessions and appends only the new history part
    to the history file.
    """
    import atexit
    import readline

    # the same setuip is also done in stdlib/site.py
    # in `enablerlcompleter()`, but with less cool history file names
    # and keybindings.

    libedit = "libedit" in readline.__doc__
    if libedit:
        print("libedit detected - history may not work at all...")

        readline_statements = (
            "bind '^[[A' ed-search-prev-history",
            "bind '^[[B' ed-search-next-history",
            "bind '^[[5C' vi-next-word",
            "bind '^[[5D' vi-prev-word",
            "bind '^B' ed-command",
            "bind '^P' ed-search-prev-history",
            "bind '^N' ed-search-next-history",
            "bind '^l' ed-clear-screen",
            "bind '^r' em-inc-search-prev",
            "bind '^I' rl_complete",
        )
    else:
        readline_statements = (
            r'"\e[A": history-search-backward',
            r'"\e[B": history-search-forward',
            r'"\e[C": forward-char',
            r'"\e[D": backward-char',
            r'"\eOd": backward-word',
            r'"\eOc": forward-word',
            r'"\e[3^": kill-word',
            r'"\C-h": backward-kill-word',
            'tab: complete',
        )

    for rlcmd in readline_statements:
        readline.parse_and_bind(rlcmd)

    # special-hack: when we're included from .pdbrc,
    # this is set.
    if getattr(sys, "_load_pdb", None):
        hist_filename = ".python_pdbhistory"
    else:
        hist_filename = f".python{sys.version_info.major}_history"

    history_file = Path(os.path.expanduser('~')) / hist_filename

    histfile_ok = True
    h_len = 0

    if history_file.exists():
        try:
            readline.read_history_file(str(history_file))
            h_len = readline.get_current_history_length()
        except OSError:
            print(f"failed to read existing history file {history_file!r}!")
            histfile_ok = False
    else:
        history_file.touch()

    if h_len == 0:
        # if we have no history, force-add one entry so `site.py` doesn't
        # create .python_history as second history file...
        readline.add_history("lol")

    def save(h_size, prev_h_len, histfile):
        new_h_len = readline.get_current_history_length()
        readline.set_history_length(h_size)
        readline.append_history_file(new_h_len - prev_h_len, histfile)

    if histfile_ok:
        atexit.register(save, HISTSIZE, h_len, str(history_file))

    return history_file


def cororun(coro):
    """
    run the given coroutine and block until it's done
    """
    try:
        return asyncio.run(coro, debug=True)
    except KeyboardInterrupt:
        print("cancelled coro run")


def _fancy_displayhook(item):
    if item is None:
        return

    global _
    _ = item

    if isinstance(item, (float, int)) and abs(item - time.time()) < (60 * 60 * 365 * 10):
        # this is a UNIX timestamp within 10 years of today
        dt = datetime.datetime.fromtimestamp(item, tz=datetime.timezone.utc).astimezone()
        display_text = "%r (%r %s)" % (item, dt.isoformat(), dt.strftime("%Z"))

    elif isinstance(item, int) and not isinstance(item, bool) and item > 0:
        if item >= 2**32:
            display_text = "{0}, 0x{0:x}".format(item)
        else:
            display_text = "{0}, 0x{0:x}, 0b{0:b}".format(item)

    else:
        term_width, term_height = shutil.get_terminal_size(fallback=(80, 24))
        display_text = pformat(item, width=term_width)

    output = highlight(display_text)
    if output.endswith("\n"):
        print(output, end="")
    else:
        print(output)


# format numbers and nested structures fancily
sys.displayhook = _fancy_displayhook
del _fancy_displayhook


if 'bpython' not in sys.modules:
    # fancy prompt. bpython doesn't like nor need this
    # the \x01 and \x02 tell readline to ignore the chars

    sys.ps1 = '\x01\x1b[36m\x02>>>\x01\x1b[m\x02 '
    sys.ps2 = '\x01\x1b[36m\x02...\x01\x1b[m\x02 '

    try:
        HISTFILE = _completion()
        del _completion
    except Exception as exc:
        sys.stderr.write("failed history and completion init: %s\n" % exc)
        import traceback
        traceback.print_exc()
        HISTFILE = None
