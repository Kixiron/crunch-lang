#!/bin/sh

ddlog -i ddlog/typecheck.dl \
      -L ddlog/modules \
      --output-dir=. \
      --omit-profile \
      --omit-workspace
exit_code=$?
if [ $exit_code -ne 0 ]; then
    exit $exit_code
fi

cd typecheck_ddlog
cargo build
