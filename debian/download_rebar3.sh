#!/bin/sh -e
# HACK: rebar3 is not packaged for debian 11, and building it requires rebar3
# itself.

REBAR3_VERSION=3.19.0

if [ -x ./rebar3 ]; then
	echo "rebar3 already downloaded"
	exit 0
fi

set -x
wget -q "https://github.com/erlang/rebar3/releases/download/$REBAR3_VERSION/rebar3"
chmod +x rebar3
