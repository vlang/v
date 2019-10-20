// Copyright (c) 2019 V lang team. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import (
    test
    term
)

fn test_asserts() {
    test.assert_true(true)

    // test.assert_eq(1, 1)

    test.assert_ne("abc", "abc")

    test.assert_true(true)
}

fn test_unescape_newline() {
    value := "1000\n2000"

    test.assert_eq(test.unescape_newline(value), "1000\\n2000")
}

fn test_build() {
    left := "1000\n1000"
    right := "1320\n1000"

    mut expected_msg := ""
    expected_msg += "Left:  `1" + term.green("00") + "0\\n1000`" + test.NEWLINE_SEPARATOR
    expected_msg += "Right: `1" + term.red("32") + "0\\n1000`" + test.NEWLINE_SEPARATOR

    test.assert_eq(expected_msg, test.build_diff(left, right))
}
