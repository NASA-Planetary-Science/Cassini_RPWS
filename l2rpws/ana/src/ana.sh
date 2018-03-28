#!/bin/sh

PGPLOT_DIR=/local/pgplot
export PGPLOT_DIR

LD_RUN_PATH=
export LD_RUN_PATH

LD_LIBRARY_PATH=
export LD_LIBRARY_PATH

echo "LD_LIBRARY_PATH="$LD_LIBRARY_PATH
echo "LD_RUN_PATH="$LD_RUN_PATH

/opt/project/cassini/bin/ana9.00r2
