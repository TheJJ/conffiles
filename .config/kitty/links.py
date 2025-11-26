from __future__ import annotations

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

patterns = [
    (r"(?:\W?LP:? +#?(?P<lpbugid>\d+)|(?:Closes)?\W+#(?P<debbugid>\d+))",
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
    for pattern, target_map in patterns:
        for match in re.finditer(pattern, text):
            start, end = match.span()
            mark_text = text[start:end]

            if isinstance(target_map, dict):
                # if a single regex can map to multiple link targets
                # take the first matching group as indicator to choose the target.
                for matchvar, target_candidate in target_map.items():
                    if match[matchvar]:
                        target = target_candidate
                        break
            else:
                target = target_map

            target_id = tuple(match[key] for key in targets[target][0])

            if prev_id := target_ids[target].get(target_id):
                # reuse id
                mark_id = prev_id
            else:
                # register new id
                mark_id = target_ids[target][target_id] = link_id
                link_id += 1

            marks.append(
                Mark(mark_id, start, end, mark_text, {"target": target, "groups": match.groupdict()})
            )

    # update mark index in ascending order as it occurs in text order
    # we have to submit them in ascending order otherwise
    # kitty's adjust_python_offsets to adjust unicode runes to bytes will error.
    # also we can't reuse indexes (so same letters are displayed) because
    # marks.go/find_marks extracts the "largest index" from the last element...
    for idx, mark in enumerate(sorted(marks, key=lambda m: m.start)):
        mark.index = idx
        yield mark


def handle_result(args: list[str], data: dict[str, Any], target_window_id: int,
                  boss: BossType, extra_cli_args, *_):
    """
    After link selection, run the associated action.
    """

    for word, match_data in zip(data["match"], data["groupdicts"]):
        target_id = match_data["target"]
        target_url = targets[target_id][1].format(**match_data["groups"])

        boss.open_url(target_url)
