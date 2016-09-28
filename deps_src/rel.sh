#!/bin/bash
rel_dir=../deps/
for app in $(ls _build/default/lib/)
do
    mkdir -p $rel_dir/$app
    rm -rf $rel_dir/$app/*
    cp -r _build/default/lib/$app/ebin/ $rel_dir/$app/
    [ -d _build/default/lib/$app/include ] && cp -r _build/default/lib/$app/include/ $rel_dir/$app/
    [ -d _build/default/lib/$app/priv ] && cp -r _build/default/lib/$app/priv/ $rel_dir/$app/
    [ -d _build/default/lib/$app/bin ] && cp -r _build/default/lib/$app/bin/ $rel_dir/$app/
done

