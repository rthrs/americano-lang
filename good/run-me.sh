#!/bin/bash

if [ ! -f ../interpreter ]; then
    echo "Build project first!"
    exit 1
fi

../interpreter *.am

