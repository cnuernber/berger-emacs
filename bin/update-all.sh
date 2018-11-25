#!/bin/bash

PROJECTS="tech.jna tech.datatype tech.javacpp-datatype tech.compute tech.opencv tvm-clj tech.ml-base tech.xgboost tech.smile techascent.com"

for PROJECT in $PROJECTS; do
    pushd $PROJECT
    git pull
    popd
done
