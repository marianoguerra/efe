#!/usr/bin/env bash

case "$1" in
    "compile-output")
	cd out
    rm -f result.txt
    find -name \*.ex -exec sh -c "echo '#' {} >> result.txt ; elixirc {} >> result.txt" \;
    ;;
   "format-output")
	cd out
    find -name \*.ex -exec sh -c "echo '#' {}; mix format {}" \;
esac
