---
title:  Silverbane
...

# Overview

Write documentation containing outputs of running command lines, and
transcripts of interacting with interactive comand line programs
(e.g. demonstrating calling api functions in a repl).

Use this program to check this documentation is correct, and to
maintain it.

This program exists because I had a variety of documentation for
various projects, which contained transcripts of running command
lines, and of running interactive command line programs, and I wanted
to check the examples were correct easily. It should complement any of
the existing systems which can test code snippets in documentation.

## Quick example

~~~~~~{sb-file-prefix='--'}

-- File docs/exampledoc.md

This is an example of using python as a simple calculator:

~~~~{sb-session='python3' sb-prompt='>>> ' sb-no-initial-text}
>>> 1 + 2
3
>>> 1 + 2
4
>>> 5 + 6
11
~~~~
~~~~~~

Running Silverbane on this exampledoc.md:

~~~~{sb-run= sb-non-zero-exit=}
$ silverbane docs/exampledoc.md
docs/exampledoc.md:4:0: document block doesn't match output:
----------
>>> 1 + 2
3
>>> 1 + 2
3
>>> 5 + 6
11
----------
diff from document to program output:
----------
 >>> 1 + 2
 3
 >>> 1 + 2
-4
+3
 >>> 5 + 6
 11
----------
~~~~

## Quick installation

Prerequisites: Python, GHC and cabal-install. If you don't have these
you can install them with ghcup <https://www.haskell.org/ghcup/>. This
program has only been tested on Linux, and has been tested with GHC
9.8.1.

clone this project and python-wrapper from github

add cabal.project.local to silverbane dir, pointing to python-wrapper

~~~
packages:
        [PATH]/python-wrapper/*.cabal
~~~

then use cabal install to install the silverbane exe

coming soon(tm): hackage, plus binary distribution

# Usage guide

Can check:

* an inline file in documentation matches one on disk
* running a program gives the output stated in documentation
* running an interactive program gives the responses stated in the
  documentation

TODO: elaborate

# Misc

* License: BSD-3
* Source repository: <https://github.com/JakeWheat/silverbane/>
* Bug tracker: <https://github.com/JakeWheat/silverbane/issues>
* Contact: jakewheat@tutanota.com

Question for the reader: I spent a bunch of time writing this tool,
because I really needed it, and didn't find anything that I could use
to test documentation in this way. Please let me know if tools like
this already exist, especially if they are better than this one or
have good ideas not in this tool.