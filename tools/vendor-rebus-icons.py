#!/usr/bin/env python3
"""Vendor the Rebus deck's icons as inline SVG paths.

**Why vendored rather than linked.** Triggerfish loads the whole Font Awesome
webfont from a CDN, which is right for an app that already has a network and
uses hundreds of icons. Quadrat uses exactly the 64 in the Rebus deck, is
served by a zero-dependency Node script on the rig, and has to work with the
network off — so it carries the shapes instead. All 64 paths come to ~35 KB,
which is less than the webfont it replaces.

**Why the shapes must be the SAME shapes.** A Rebus identity is a pair of
icons, and the point of it is that the same content wears the same pair
wherever it is shown. Substituting a different icon set would leave the hash
agreeing and the pictures disagreeing, which is worse than not sharing the
identity at all: you would trust a match that is not one.

Run:  python3 tools/vendor-rebus-icons.py <path-to-fontawesome-free-web-dir>

Font Awesome Free icons are CC BY 4.0 (https://fontawesome.com/license/free)
and the attribution is carried into the generated module.
"""
import json, os, re, sys

# The deck, which is Rebus's and must be copied from it rather than guessed.
# A name here that Rebus does not have, or vice versa, is an icon that will
# never render or a path that is never asked for.
DECK = [
    "bomb", "star", "moon", "sun", "cloud", "bolt", "fire", "leaf",
    "tree", "feather", "fish", "frog", "crow", "dove", "cat", "dog",
    "horse", "hippo", "dragon", "spider", "bug", "ghost", "skull", "heart",
    "anchor", "bell", "key", "lock", "gem", "crown", "cube", "dice",
    "flask", "rocket", "bicycle", "car", "plane", "ship", "truck", "bus",
    "tractor", "compass", "map", "book", "guitar", "drum", "music", "umbrella",
    "snowflake", "mountain", "tornado", "meteor", "atom", "brain", "eye", "cow",
    "ambulance", "hammer", "wrench", "gear", "seedling", "spa", "plug", "bath",
]

OUT = os.path.join(os.path.dirname(__file__), "..", "quadrat", "src", "Quadrat", "RebusIcons.purs")


def main(fa):
    meta = json.load(open(os.path.join(fa, "metadata", "icons.json")))
    # Font Awesome 6 renamed icons and kept the old names as aliases; the
    # webfont resolves them and a vendored file cannot, so `ambulance` has to
    # be followed to `truck-medical` here or it silently never draws.
    alias = {}
    for k, v in meta.items():
        for a in (v.get("aliases") or {}).get("names") or []:
            alias[a] = k

    rows = []
    for name in DECK:
        key = name if os.path.exists(os.path.join(fa, "svgs", "solid", name + ".svg")) else alias.get(name)
        if not key:
            sys.exit(f"{name}: no solid icon and no alias for it")
        svg = open(os.path.join(fa, "svgs", "solid", key + ".svg")).read()
        vb = re.search(r'viewBox="([^"]+)"', svg).group(1)
        d = re.search(r'\sd="([^"]+)"', svg).group(1)
        rows.append((name, vb, d))

    body = "\n  , ".join(
        'Tuple "%s" { box: "%s", path: "%s" }' % (n, vb, d) for n, vb, d in rows
    )
    src = HEADER + "  [ " + body + "\n  ]\n"
    with open(OUT, "w") as f:
        f.write(src)
    print(f"wrote {len(rows)} icons to {os.path.normpath(OUT)}")


HEADER = '''-- | **The Rebus deck's icons, as paths.** GENERATED — see
-- | `tools/vendor-rebus-icons.py`; edit that and re-run it, never this.
-- |
-- | Vendored rather than linked. Triggerfish pulls the whole Font Awesome
-- | webfont from a CDN, which suits an app with a network and hundreds of
-- | icons; Quadrat draws exactly these sixty-four, is served by a
-- | zero-dependency Node script on the rig, and has to work with the network
-- | off. The paths come to about 35 KB, less than the webfont they replace.
-- |
-- | **They must be the same shapes.** A Rebus identity is a pair of icons and
-- | its whole value is that one content wears one pair wherever it is shown.
-- | A different icon set would leave the hash agreeing and the pictures
-- | disagreeing — worse than not sharing the identity, because you would
-- | trust a match that is not one.
-- |
-- | Font Awesome Free 6.5.1, CC BY 4.0 — https://fontawesome.com/license/free
module Quadrat.RebusIcons (Icon, iconOf) where

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

-- | One icon: the path, and the viewBox it was drawn in. The box is carried
-- | because Font Awesome's icons are not all the same width — five different
-- | widths across this deck — and a path drawn in the wrong box is stretched.
type Icon = { box :: String, path :: String }

-- | An icon by its deck name, or `Nothing` for a name this build has never
-- | heard of. Nothing rather than a placeholder: a missing icon is a deck
-- | that has moved on without the vendored paths, and a placeholder would
-- | make two different identities look alike.
iconOf :: String -> Maybe Icon
iconOf n = Map.lookup n icons

icons :: Map String Icon
icons = Map.fromFoldable
'''

if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
