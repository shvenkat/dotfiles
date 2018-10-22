#!/usr/bin/env python3

from argparse import ArgumentParser
from collections import namedtuple
from itertools import groupby
from typing import List, Tuple  # noqa: F401

import fontforge


def main():
    # type: () -> None
    args = make_parser().parse_args()
    if args.glyphs:
        print_glyphs(args.font)
    elif args.ligatures:
        print_ligatures(args.font)
    elif args.ligature_groups:
        print_ligature_groups(args.font)
    elif args.substitutions:
        print_substitutions(args.font)
    elif args.substitution_groups:
        print_substitution_groups(args.font)


def make_parser():
    # type: () -> ArgumentParser
    parser = ArgumentParser()
    parser.add_argument(
        "--glyphs",
        action="store_true",
        help="Print names and codepoints of all glyphs.",
    )
    parser.add_argument(
        "--ligatures",
        action="store_true",
        help="Print ligatures identified by the '.liga' name suffix.",
    )
    parser.add_argument(
        "--ligature-groups",
        action="store_true",
        help=(
            "Print ligatures identified by the '.liga' name suffix, "
            "grouped by first character."
        ),
    )
    parser.add_argument(
        "--substitutions",
        action="store_true",
        help="Print glyph substitutions based on positional substitution info.",
    )
    parser.add_argument(
        "--substitution-groups",
        action="store_true",
        help=(
            "Print glyph substitutions based on positional substitution info,"
            "grouped by last character."
        ),
    )
    parser.add_argument("font", help="Font file to inspect.")
    return parser


def print_glyphs(fontpath):
    # type: (str) -> None
    try:
        font = fontforge.open(fontpath)
        for glyph in font.glyphs():
            print(
                "%s\t%s\t%s" % (glyph.codepoint, glyph.unicode, glyph.glyphname)
            )
    finally:
        font.close()


def print_ligatures(fontpath):
    # type: (str) -> None
    ligature_strs = get_ligature_strs(fontpath)
    for ligature_repr in sorted(ligature_strs):
        print(
            "%s\t%s\t%s\t%s"
            % (
                ligature_repr,
                ligature_repr[0],
                ligature_repr[-1],
                " ".join(ligature_repr),
            )
        )


def print_ligature_groups(fontpath):
    # type: (str) -> None
    ligature_strs = get_ligature_strs(fontpath)
    for first_char, ligatures in groupby(
        sorted(ligature_strs), key=lambda s: s[0]
    ):
        print("%s\t%s" % (first_char, " ".join(ligatures)))


def get_ligature_strs(fontpath):
    # type: (str) -> List[str]
    try:
        font = fontforge.open(fontpath)
        ligature_strs = [
            get_subst_glyph(glyph.glyphname).str
            for glyph in font.glyphs()
            if glyph.glyphname.endswith(".liga")
        ]
    finally:
        font.close()
    return ligature_strs


def print_substitutions(fontpath):
    # type: (str) -> None
    glyph_subs = get_glyph_subst_pairs(fontpath)
    for glyphname, subst_glyph in sorted(
        glyph_subs, key=lambda tpl: tpl[1].str
    ):
        print("%s\t%s" % (subst_glyph.str, glyphname))


def print_substitution_groups(fontpath):
    # type: (str) -> None
    glyph_subs = get_glyph_subst_pairs(fontpath)
    key_func = lambda tpl: tpl[0]
    for key, subs in groupby(sorted(glyph_subs, key=key_func), key=key_func):
        _subs = list(subs)
        ligs = " ".join(
            sorted(sub[1].str for sub in _subs if sub[1].kind is SG_LIG)
        )
        alts = " ".join(
            sorted(sub[1].str for sub in _subs if sub[1].kind is SG_ALT)
        )
        print("%s\t%s\t%s" % (key, ligs, alts))


def get_glyph_subst_pairs(fontpath):
    # type: (str) -> List[Tuple[str, SubstGlyph]]
    try:
        font = fontforge.open(fontpath)
        glyph_subs = set(
            (get_subst_glyph(glyph.glyphname).str, get_subst_glyph(gn))
            for glyph in font.glyphs()
            if glyph.getPosSub("*")
            for ps in glyph.getPosSub("*")
            for gn in ps[2:]
            if gn != "LIG"
            and ".locl" not in gn
            and not any(
                gn.endswith(suffix) for suffix in [".tosf", ".numr", ".dnom"]
            )
        )
    finally:
        font.close()
    return [gs for gs in glyph_subs if gs[0] in NAMED_CHARS.values()]


def get_subst_glyph(glyphname):
    # type: (str) -> SubstGlyph
    if "." in glyphname and glyphname.endswith(".liga"):
        glyphname = glyphname[: glyphname.index(".")]
    if "_" in glyphname:
        subst_glyph = SubstGlyph(
            kind=SG_LIG,
            str="".join(
                NAMED_CHARS.get(char_name, char_name)
                for char_name in glyphname.split("_")
            ),
        )  # type: SubstGlyph
    else:
        subst_glyph = SubstGlyph(
            kind=SG_ALT, str=NAMED_CHARS.get(glyphname, glyphname)
        )
    return subst_glyph


SubstGlyph = namedtuple("SubstGlyph", "kind str")
SG_LIG = "ligature"
SG_ALT = "alternate"


NAMED_CHARS = {
    "ampersand": "&",
    "asciicircum": "^",
    "asciitilde": "~",
    "asterisk": "*",
    "at": "@",
    "backslash": "\\",
    "bar": "|",
    "braceleft": "{",
    "braceright": "}",
    "bracketleft": "[",
    "bracketright": "]",
    "colon": ":",
    "dollar": "$",
    "equal": "=",
    "exclam": "!",
    "greater": ">",
    "hyphen": "-",
    "less": "<",
    "numbersign": "#",
    "parenleft": "(",
    "parenright": ")",
    "percent": "%",
    "period": ".",
    "plus": "+",
    "question": "?",
    "semicolon": ";",
    "slash": "/",
    "underscore": "_",
    "w": "w",  # www
    "x": "x",  # times, as in 0xFF
}


if __name__ == "__main__":
    main()
