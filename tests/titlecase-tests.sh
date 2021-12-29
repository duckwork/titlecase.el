#!/bin/bash
SCRIPT_PATH="$(dirname "${BASH_SOURCE[0]}")"
emacs \
    -batch \
    -l "$SCRIPT_PATH/titlecase-tests.el" \
    -f ert-run-tests-batch-and-exit
