#!/usr/bin/env bash

set -ex

rm -rf vlib/toml/tests/testdata/iarna vlib/toml/tests/testdata/toml_rs vlib/toml/tests/testdata/toml_lang vlib/toml/tests/testdata/large_toml_file_test.toml

./v retry -- ./v download -o vlib/toml/tests/testdata/large_toml_file_test.toml https://gist.githubusercontent.com/Larpon/89b0e3d94c6903851ff15559e5df7a05/raw/62a1f87a4e37bf157f2e0bfb32d85d840c98e422/large_toml_file_test.toml

./v retry -- git clone -n https://github.com/iarna/toml-spec-tests.git vlib/toml/tests/testdata/iarna
git -C vlib/toml/tests/testdata/iarna checkout 1880b1a

./v retry -- git clone -n https://github.com/toml-lang/toml-test.git vlib/toml/tests/testdata/toml_lang
git -C vlib/toml/tests/testdata/toml_lang checkout c6a78f1

# A few history notes of toml-rs (previously alexcrichton):
#  commit 7f5472c the test-suite dir moves to the crates/ sub-directory
#  commit 8461f7c *a lot* of test files are removed in *hope* that they are covered by the compliance test suite (assumed to be BurntSushi/toml-test, later toml-lang/toml-test)
#  commit 9bd454c the last known good commit we can test against
./v retry -- git clone -n https://github.com/toml-rs/toml.git vlib/toml/tests/testdata/toml_rs
git -C vlib/toml/tests/testdata/toml_rs reset --hard 9bd454c
