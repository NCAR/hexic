#!/bin/sh

rm -rf build
mkdir build
cd build

cmake \
  -DCMAKE_INSTALL_PREFIX:PATH=~/software/hexic \
  -DIDL_ROOT_DIR:PATH=/opt/share/idl8.5/idl85 \
  ..
