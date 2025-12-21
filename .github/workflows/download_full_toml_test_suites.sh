#!/usr/bin/env bash

set -ex

rm -rf vlib/toml/tests/testdata/iarna vlib/toml/tests/testdata/toml_rs vlib/toml/tests/testdata/toml_lang vlib/toml/tests/testdata/large_toml_file_test.toml

./v retry -- ./v download -o vlib/toml/tests/testdata/large_toml_file_test.toml https://gist.githubusercontent.com/Larpon/89b0e3d94c6903851ff15559e5df7a05/raw/62a1f87a4e37bf157f2e0bfb32d85d840c98e422/large_toml_file_test.toml

./v retry -- git clone -n https://github.com/iarna/toml-spec-tests.git vlib/toml/tests/testdata/iarna
git -C vlib/toml/tests/testdata/iarna checkout 1880b1a

./v retry -- git clone -n https://github.com/toml-lang/toml-test.git vlib/toml/tests/testdata/toml_lang
git -C vlib/toml/tests/testdata/toml_lang checkout c6a78f1

./v retry -- git clone -n https://github.com/toml-rs/toml.git vlib/toml/tests/testdata/toml_rs
git -C vlib/toml/tests/testdata/toml_rs reset --hard 499e8c4
