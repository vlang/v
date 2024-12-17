#!/usr/bin/env bash

set -ex

rm -rf vlib/toml/tests/testdata/burntsushi  vlib/toml/tests/testdata/iarna  vlib/toml/tests/testdata/alexcrichton  vlib/toml/tests/testdata/large_toml_file_test.toml

./v retry -- ./v download --target-folder vlib/toml/tests/testdata https://gist.githubusercontent.com/Larpon/89b0e3d94c6903851ff15559e5df7a05/raw/62a1f87a4e37bf157f2e0bfb32d85d840c98e422/large_toml_file_test.toml

./v retry -- git clone -n https://github.com/iarna/toml-spec-tests.git vlib/toml/tests/testdata/iarna
git -C vlib/toml/tests/testdata/iarna checkout 1880b1a

./v retry -- git clone -n https://github.com/toml-lang/toml-test.git vlib/toml/tests/testdata/burntsushi
git -C vlib/toml/tests/testdata/burntsushi checkout f30c716

./v retry -- git clone -n https://github.com/toml-rs/toml.git vlib/toml/tests/testdata/alexcrichton
git -C vlib/toml/tests/testdata/alexcrichton reset --hard 499e8c4
