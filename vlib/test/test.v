// Copyright (c) 2019 V lang team. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module test

import term


// Checks whether two values are equal
pub fn assert_eq<T>(left, right T) {
    if left == right {
        C.__increase_g_test_oks()
        return
    }
    
    eprintln(assert_eq_fail_msg(left.str(), right.str()))
    C.__increase_g_test_fails()
    
    // Panic will print backtrace, so user'll be able to see where
    // exactly assert happened.
    panic("")
}

// Checks whether two values are not equal
pub fn assert_ne<T>(left T, right T) {
    if left != right {
        C.__increase_g_test_oks()
        return
    }
    
    eprintln(assert_ne_fail_msg(left.str(), right.str()))
    C.__increase_g_test_fails()
    
    panic("")
}

// Checks whether provided value is set to true
// This functions exists for support of builtin assert keyword
pub fn assert_true(val bool) bool {
    assert_eq(true, val)
    return true
}

// Builds a colored diff between two strings
fn build_diff(left_input string, right_input string) string {
    left := unescape_newline(left_input)
    right := unescape_newline(right_input)

    mut min_length := 0
    if left.len <= right.len {
        min_length = left.len
    } else {
        min_length = right.len
    }

    mut left_diff := ""
    mut right_diff := ""
    mut i := 0

    for i < min_length {
        if left[i] == right[i] {
            left_diff += left[i].str()
            right_diff += right[i].str()
            i += 1
            continue
        }

        mut left_current_diff := ""
        mut right_current_diff := ""

        for i < min_length && left[i] != right[i] {
            left_current_diff += left[i].str()
            right_current_diff += right[i].str()
            i += 1
        }

        left_diff += term.green(left_current_diff)
        right_diff += term.red(right_current_diff)
    }

    if left.len > right.len {
        left_diff += term.green(left.substr(right.len, left.len))
    } else if right.len > left.len {
        right_diff += term.red(right.substr(left.len, right.len))
    }
    
    mut diff_msg := "Left:  `$left_diff`" + NEWLINE_SEPARATOR
    diff_msg += "Right: `$right_diff`" + NEWLINE_SEPARATOR

    return diff_msg
}

fn assertion_failure_msg() string {
    fail_msg_prefix := term.red("Assertion failure:") + NEWLINE_SEPARATOR
    return fail_msg_prefix
}

fn assert_eq_fail_msg(left string, right string) string {
    mut message := assertion_failure_msg()

    message += build_diff(left, right)
    
    return message
}

fn assert_ne_fail_msg(left string, right string) string {
    mut message := assertion_failure_msg()

    message += "Equal values in assert_ne: $left == $right" + NEWLINE_SEPARATOR
    
    return message
}

fn assert_bool_fail_msg(expected bool, got bool) string {
    mut message := assertion_failure_msg()

    message += "Expected " + term.green(expected.str()) + ", got " + term.red(got.str()) + NEWLINE_SEPARATOR

    return message
}

fn assert_bool(expected bool, got bool) {
    if expected != got {
        eprintln(assert_bool_fail_msg(expected, got))
        C.__increase_g_test_fails()
        
        panic("")
    }
    C.__increase_g_test_oks()
}

// Replaces \n in string with \\n
fn unescape_newline(input string) string {
    return input.split('\n').join('\\n')
}

const (
    // TODO should be OS-independent
    NEWLINE_SEPARATOR = "\n"
)
