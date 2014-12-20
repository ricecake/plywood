#!/bin/bash
DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
if [ "$DIR" != "$(pwd)" ]; then
	cd $DIR
fi
if [ -z $(which screen) ]; then
	/usr/bin/env erl -pa ../../plywood/ebin/ -pa ../../plywood/deps/*/ebin -s plywood
else
	screen -S plywood -d -m /usr/bin/env erl -pa ../../plywood/ebin/ -pa ../../plywood/deps/*/ebin -s plywood
fi
