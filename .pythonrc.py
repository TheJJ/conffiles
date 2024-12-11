#!/usr/bin/env python3

"""
jj's pythonrc

Copyright (c) 2012-2024 Jonas Jelten <jj@sft.lol>
Licensed GPLv3 or later.
"""


# these imports are available in interactive python shells
import asyncio
import base64
import ctypes
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
import site
import struct
import subprocess
import sys
import time
import traceback
from binascii import hexlify, unhexlify
from pathlib import Path
from pprint import pformat
from pydoc import describe, doc, locate
from subprocess import Popen, run


PAGER_INVOCATION = os.environ.get("PAGER", "less -S -i -R -M --shift 5")
HISTSIZE = 50000
PDB = getattr(sys, "_load_pdb", None) is not None

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


USE_PYGMENTS = True
HAS_PYGMENTS = False

if USE_PYGMENTS:
    try:
        import pygments
        import pygments.formatters
        import pygments.lexers
        HAS_PYGMENTS = True
    except ImportError:
        pass


### utility functions for a better cli experience™

def pager(txt):
    # pydoc.getpager() may be better for some cases
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
        lexer = pygments.lexers.get_lexer_by_name('python')
        formatter = pygments.formatters.TerminalFormatter(bg='dark')
        return pygments.highlight(source, lexer, formatter)

    def pprint(obj) -> None:
        """Colored pretty printing."""
        print(highlight(pformat(obj)), end="")
else:
    def highlight(txt):
        return txt

    from pprint import pprint


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


def cat(*names, binary=False, lines=False):
    """Read the given file and return its contents."""
    mode = "rb" if binary else "r"
    ret = list()
    for name in names:
        with open(name, mode) as fd:
            if lines:
                ret.extend(fd.readlines())
            ret.append(fd.read())

    if lines:
        return ret
    else:
        return "".join(ret)


def catln(names, binary=False):
    """Read the lines of the given files and return them."""
    return cat(names, binary, lines=True)


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


def cororun(coro):
    """
    run the given coroutine and block until it's done.
    when we're in a coro already, we can't run the event loop again
    except when continuing execution normally...
    """
    try:
        return asyncio.run(coro, debug=True)
    except KeyboardInterrupt:
        print("cancelled coro run")


### internal initialization functions

def _completion():
    """
    set up readline and history.

    python3.13 has a very special readline replacement that is supposed to be better.
    but this setup needs special adjustments.

    supports parallel sessions and appends only the new history part
    to the history file.

    returns the history filename
    """
    import atexit
    import readline

    # we do the cross-version implementation in this file.
    # the same setuip is also done in stdlib/site.py
    # in `enablerlcompleter/register_readline()`,
    # but here we can
    # - multi-process history support (the default overwrites history with the last-closed shell...)
    # - more keybindings
    # - compatibility of <=python3.12 and >=python3.13
    sys.__interactivehook__ = lambda: None

    try:
        from _pyrepl import readline as pyrepl_readline
        from _pyrepl.main import CAN_USE_PYREPL
    except ImportError:
        CAN_USE_PYREPL = False

    USE_PYREPL = CAN_USE_PYREPL and not os.getenv("PYTHON_BASIC_REPL")

    if sys.version_info >= (3, 13):
        libedit = readline.backend == "editline"
    else:
        libedit = "libedit" in readline.__doc__

    if libedit:
        print(".pythonrc.py: libedit detected - history may not work at all...")

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

    # stdlib/site also still initializes regular readline...
    for rlcmd in readline_statements:
        readline.parse_and_bind(rlcmd)

    try:
        readline.read_init_file()
    except OSError:
        # init file not found
        pass

    # special-hack: when we're included from .pdbrc,
    # this is set.
    if PDB:
        history_file = Path("~/.python_pdbhistory").expanduser()
    else:
        old_hist_file = Path(f"~/.python{sys.version_info.major}_history").expanduser()
        history_file = Path(site.gethistoryfile())

        if old_hist_file.exists():
            if not history_file.exists():
                old_hist_file.rename(history_file)
            else:
                print("warning: both old and new history files exist, can't move old to new name.")
                print(f"old: {old_hist_file}")
                print(f"new: {history_file}")

    histfile_ok = True
    h_len = 0

    if USE_PYREPL:
        readline_module = pyrepl_readline
    else:
        readline_module = readline

    if history_file.exists():
        try:
            readline_module.read_history_file(str(history_file))
            h_len = readline_module.get_current_history_length()
        except OSError:
            print(f"failed to read existing history file {history_file!r}!")
            histfile_ok = False
    else:
        history_file.touch()

    if h_len == 0:
        # if we have no history, force-add one entry so `site.py` doesn't
        # create .python_history as second history file...
        readline_module.add_history("lol")

    # we use the fancy C implementation of libreadline to truncate the history file.
    if USE_PYREPL:
        try:
            rl = ctypes.cdll.LoadLibrary("libreadline.so")
        except OSError:
            print("failed to load readline library")
            rl = None

    def save(prev_h_len, histfile, histsize):

        # trim the history size on save only
        # so we can just append the new entries
        readline_module.set_history_length(histsize)

        if USE_PYREPL:
            # TODO: implement history appending by using a virtual file
            # this is an adaption of _pyrepl.readline.write_history_file,
            # but with support for appending.

            # hack to actually get history - sorry, there's no better way in 3.13.
            history = readline_module._get_reader().history
            # amount of new history elements
            new_hist_len = len(history) - prev_h_len

            if new_hist_len > 0:
                # write the new history elements
                new_history_entries = history[-new_hist_len:]
                with open(histfile, "a", encoding="utf-8") as f:
                    for entry in new_history_entries:
                        entry = entry.replace("\n", "\r\n")  # multiline history support
                        f.write(entry)
                        f.write("\n")

                # truncate the file (readline.append_history_file does this as well)
                # and directly uses the libreadline C implementation...
                if rl:
                    ok = rl.history_truncate_file(histfile.encode(), histsize)
                    if ok != 0:
                        print("failed to truncate history file")
        else:
            new_hist_len = readline_module.get_current_history_length() - prev_h_len
            readline_module.append_history_file(new_hist_len, histfile)

    if histfile_ok:
        atexit.register(save, h_len, str(history_file), HISTSIZE)

    return history_file, readline_module


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
        # pprint.pformat does an internal repr(item), which "fixes" some unicode errors!
        display_text = pformat(item, width=term_width)

    output = highlight(display_text)

    try:
        sys.stdout.write(output)
        if not output.endswith("\n"):
            sys.stdout.write("\n")

    except UnicodeEncodeError:
        # to pass cpython's test.test_cmd_line.CmdLineTest.test_displayhook_unencodable
        bytes = display_text.encode(sys.stdout.encoding, 'backslashreplace')
        if hasattr(sys.stdout, 'buffer'):
            sys.stdout.buffer.write(bytes)
        else:
            text = bytes.decode(sys.stdout.encoding, 'strict')
            sys.stdout.write(text)

        if not display_text.endswith("\n"):
            sys.stdout.write("\n")


# format numbers and nested structures fancily
sys.displayhook = _fancy_displayhook
del _fancy_displayhook


if 'bpython' not in sys.modules:
    # fancy prompt. bpython doesn't like nor need this
    # the \x01 and \x02 tell readline to ignore the chars

    sys.ps1 = '\x01\x1b[36m\x02>>>\x01\x1b[m\x02 '
    sys.ps2 = '\x01\x1b[36m\x02...\x01\x1b[m\x02 '

    try:
        HISTFILE, HISTMODULE = _completion()
    except Exception as exc:
        sys.stderr.write("failed history and completion init: %s\n" % exc)
        import traceback
        traceback.print_exc()
        HISTFILE = None
        HISTMODULE = None
    finally:
        del _completion
