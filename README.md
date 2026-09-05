# Friends of Itajara

A looper for people with a sample-playing eurorack module, a sound source
and a Mac: the [Itajara](https://github.com/afcondon/itajara) daemon with a
page on it. **One app, a face per module.** Arbhar's Friend is the first;
Morphagene's, Rample's and QD's are rows of the same table.

Record and overdub into loops, hear the layers, switch any of them out,
trim and rotate a loop while it plays, and then **harvest**: each loop's
layers go onto the module's stick in the module's own layout — for the
Arbhar, a loop is a library bank *and* a scene, each file ten seconds plus
the three that follow it — with a datasheet beside them that says what is
there and where it went. Notes you add (key, timbre, what it is for) ride
along.

Every control is a button on the page. No pedalboard, no MIDI controller
required.

## Running it

```
git clone https://github.com/afcondon/itajara
git clone https://github.com/afcondon/FriendsOfItajara friends-of-itajara
cd itajara/daemon && cargo build --release
./target/release/itajara loop --device <your interface> --layers 6 --yes
cd ../../friends-of-itajara && make serve
open http://localhost:3029/?face=arbhar
```

The two repositories sit side by side: this one reaches the daemon's
`client` and `surface` packages by path. Harvesting needs `msm` on the path
(`cargo install --path SamplesProject/msm`). Node 18+ for `server.mjs`;
PureScript's `spago` for the page.

## Reading it

- `docs/DESIGN.md` — what a face is and is not, where the pieces live, the
  page, saving and harvesting, and what is not built yet.
- `src/Friend/Face.purs` — the table. A new module is one row.
- `server.mjs` — the seam: the page cannot write a file or run a process,
  the daemon must not, so this one zero-dependency file does both.

The face is not the skin: how the page *looks* is `static/friend.css`, the
plain one, and a skin is a second stylesheet in `static/skins/` that replaces
it and leaves the class names alone — `?skin=river` is the Arbhar one: an architectural
drawing, a cousin of Fallingwater — each loop a slab cantilevered from a
hatched core, the water running beneath, a drafting hand for the few words
there are, every button a glyph, the data in a title block. Not a copy of
anyone's panel.

MIT.
