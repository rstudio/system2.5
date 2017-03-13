#!/usr/bin/env bash

# First argument is the output dir where we'll put job info files
OUTDIR="$1"
shift

# Record the pid of the bash process itself
echo "$$" > "$OUTDIR/ppid.tmp"
mv "$OUTDIR/ppid.tmp" "$OUTDIR/ppid"

# Second argument is the command, sans arguments
PROG="$1"
shift

echo "$@" >&2

# Run the program in the background, with stdout/stderr redirected.
# We have to run in the background so that we can save the PID to
# a file right away.
$PROG "$@" > "$OUTDIR/job.stdout" 2> "$OUTDIR/job.stderr" <&0 &
PID="$!"

# Record the PID
echo "$PID" > "$OUTDIR/pid.tmp"
mv "$OUTDIR/pid.tmp" "$OUTDIR/pid"

wait %1
STATUS="$?"

# Record the exit code
echo "$STATUS" > "$OUTDIR/exitcode.tmp"
mv "$OUTDIR/exitcode.tmp" "$OUTDIR/exitcode"

# We ourselves will exit with the same exit code as the program
exit $STATUS
