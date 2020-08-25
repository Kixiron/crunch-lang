#!/bin/sh

printf "building ddlog... "
ddlog -i ddlog/typecheck.dl \
      -L ddlog/modules \
      --output-dir=. \
      --omit-profile \
      --omit-workspace \
      --run-rustfmt

exit_code=$?
if [ $exit_code -ne 0 ]; then
    printf "failed\n"
    exit $exit_code
else
    printf "ok\n"
fi

cd typecheck_ddlog

printf "checking generated code... "
cargo --quiet check
exit_code=$?
if [ $exit_code -ne 0 ]; then
    printf "failed\n"
    exit $exit_code
else
    printf "ok\n"
fi
