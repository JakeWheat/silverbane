#!/usr/bin/env python3

"""

print on stdout and stderr interleaved, then exit with the exit code
given on the command line

"""

import sys

print("stdoutline1\nstdoutline2")
sys.stdout.flush()

print("stderrline1\nstderrline2", file=sys.stderr)
sys.stderr.flush()

print("stdoutline3\nstdoutline4")
sys.stdout.flush()
print("stderrline3\nstderrline4", file=sys.stderr)
sys.stderr.flush()

my_exit = int(sys.argv[1])
sys.exit(my_exit)
