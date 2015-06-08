#!/bin/bash

erl -config sys +B -pa $PWD/ebin $PWD/deps/*/ebin \
    -eval 'application:ensure_all_started(erlhome).'