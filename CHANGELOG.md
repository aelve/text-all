# 0.4.0.0

* Dropped `text-show` entirely; now `show` works via `Prelude.show`, which is
  slower but avoids a heavy dependency. It is recommended to use
  `text-format`, `formatting` or `fmt` if fast formatting is needed.

# 0.3.1.0

* Bumped the `text-show` upper bound. (`text-show` added some instances.)

# 0.3.0.2

* Bumped the `text-show` upper bound.

# 0.3.0.1

* Bumped the `text-show` upper bound.

# 0.3.0.0

* Replaced functions like `strictToBuilder` with conversion typeclasses (i.e.
  now it's just `toStrict`, `toLazy`, `toBuilder`, and `toString`).

# 0.2.0.0

* Renamed lots of functions, moved some into other modules.

* Added `Builder`-related functions.

* Reexported `Buildable`.

* Added the `LText` type synonym.

* Made the upper bounds strict.

# 0.1.0.0

First release.
