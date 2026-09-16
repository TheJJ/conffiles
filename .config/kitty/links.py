from __future__ import annotations

import os
import re
import sys

from collections import defaultdict
from typing import Any


# target_id -> (id fields, ...) -> url_template
targets = {
    "lpbug": (("lpbugid",), "https://bugs.launchpad.net/ubuntu/+bug/{lpbugid}"),
    "debianbug": (("debbugid",), "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug={debbugid}"),
    "ubuntucve": (("cveid",), "https://ubuntu.com/security/cves?q={cveid}"),
}

# text matching, mapped to opening targets (if named group matches)
patterns = [
    (r"(?:\W?(LP:?\s+#?|lp)(?P<lpbugid>\d+)|(?:Closes)?\W+#(?P<debbugid>\d+))",
     {"lpbugid": "lpbug", "debbugid": "debianbug"}),
    (r"(?P<cveid>CVE-\d+-\d+)", "ubuntucve"),
]


def mark(text: str, args: list[str], Mark, extra_cli_args, *_):
    """
    Walk over the current buffer view text and extract link marks.
    """
    link_id = 0

    # target -> identifier -> id (so identifiers can be reused)
    target_ids: dict[str, dict[tuple[Any, ...], int]] = defaultdict(dict)
    marks = list()

    def add_mark(start, end, match, target, info):
        nonlocal link_id
        target_id = tuple(match[key] for key in targets[target][0])

        if prev_id := target_ids[target].get(target_id):
            # reuse id
            mark_id = prev_id
        else:
            # register new id
            mark_id = target_ids[target][target_id] = link_id
            link_id += 1

        marks.append(
            Mark(mark_id, start, end, match.group(0), {"target": target, "groups": match.groupdict()})
        )

    for pattern, target_map in patterns:
        for match in re.finditer(pattern, text):
            if isinstance(target_map, dict):
                # if a single regex can map to multiple link targets
                # take the first matching group as indicator to choose the target.
                for matchvar, target_candidate in target_map.items():
                    if match[matchvar]:
                        target = target_candidate
                        break
            else:
                target = target_map

            start, end = match.span()
            add_mark(start, end, match, target, match.groupdict())

    # multi target matching
    for linematch in re.finditer(r"Launchpad-Bugs-Fixed:((\s+\d+)+)", text):
        start, end = linematch.span()
        for bugmatch in re.finditer(r"(?P<lpbugid>\d+)", linematch.group(0)):
            idstart, idend = bugmatch.span()
            idstart += start
            idend += start
            add_mark(idstart, idend, bugmatch, "lpbug", bugmatch.groupdict())

    # update mark index in ascending order as it occurs in text order
    # we have to submit them in ascending order otherwise
    # kitty's adjust_python_offsets to adjust unicode runes to bytes will error.
    # also we can't reuse indexes (so same letters are displayed) because
    # marks.go/find_marks extracts the "largest index" from the last element...
    prev_mark = None
    for idx, mark in enumerate(sorted(marks, key=lambda m: m.start)):
        mark.index = idx

        if prev_mark and mark.start < prev_mark.end:
            raise Exception(f"overlapping marks: previous={prev_mark} current={mark}")

        yield mark
        prev_mark = mark


def handle_result(args: list[str], data: dict[str, Any], target_window_id: int,
                  boss: BossType, extra_cli_args, *_):
    """
    After link selection, run the associated action.
    """

    for word, match_data in zip(data["match"], data["groupdicts"]):
        target_id = match_data["target"]
        target_url = targets[target_id][1].format(**match_data["groups"])

        boss.open_url(target_url)


# TODO kitty shouldn't call this as __main__ when it's just sourcing it to execute mark()
if __name__ == "__main__":
    if os.environ.get("KITTY_MARK_TEST"):
        import argparse
        cli = argparse.ArgumentParser()
        cli.add_argument("testfile")
        args = cli.parse_args()

        with open(args.testfile) as fd:
            text = fd.read()

        class TestMark:
            def __init__(self, *args):
                self.args = args
                self.start = args[1]
                self.end = args[2]

            def __repr__(self):
                return str(self.args)

        marks = mark(text, [], TestMark, [])
        if not marks:
            print("found no marks.")
        else:
            print("found marks:")

        for mark in marks:
            print(mark)
