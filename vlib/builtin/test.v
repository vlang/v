// Copyright (c) 2019 V lang team. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import test

// Checks whether two elements are equal
pub fn assert_eq<T>(left, right T) {
    test.assert_eq(left, right)
}

// Checks whether two integers are not equal
pub fn assert_ne<T>(left T, right T) {
    test.assert_ne(left, right)
}

// Checks whether provided value is set to true
// It returns value to be compatible with builtin `assert` statement
pub fn assert_true(val bool) bool {
    return test.assert_true(val)
}
