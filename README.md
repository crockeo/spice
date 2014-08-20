# spice

*spice* is my attempt at writing a semi-useful (not only to me, but to the
ecosystem as a whole) library. It attempts to address the fact that there are
very, very few game engines or frameworks existent for Haskell. *spice* uses
[Elerea](http://hackage.haskell.org/package/elerea) for its FRP library,
[GLFW](http://hackage.haskell.org/package/GLFW) for its OpenGL context, and
[OpenGL](http://hackage.haskell.org/package/OpenGL) for its rendering. All of
these come together to provide a framework for programmers to write their own
videogames in Haskell.

# Installation

To install *spice* first you must make sure you can (or already have) *elerea*,
*GLFW*, and *OpenGL* installed on your machine.

Then to install it simply run:

```bash
# Updating the cabal repository
cabal update

# Installing spice
cabal install spice
```
