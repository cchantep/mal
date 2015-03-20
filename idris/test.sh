#! /bin/sh

SOURCE=`basename "$1"`
DIR=`dirname "$0"`

cd "$DIR"
idris --build mal.ipkg

MODULE=`head -n 1 "$SOURCE" | cut -d ' ' -f 2`
FUN=`echo "$SOURCE" | cut -d '.' -f 1`
TMP=`mktemp ./runstep.XXXXXX`
RUNNER=`basename "$TMP"`".idr"

mv "$TMP" "$RUNNER"

cat > "$RUNNER" << EOF
module Main

import $MODULE

main : IO ()
main = $FUN
EOF

idris "$RUNNER" -o runstep && ./runstep && rm -f ./runstep
rm -f "$RUNNER"
