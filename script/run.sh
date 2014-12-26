#! /bin/sh

DB_DIR=/data/database/Secret
erl -name Secret@192.168.15.222  -pa "/data/Secret/ebin"  -mnesia dir \"${DB_DIR}\"  -s manager_misc