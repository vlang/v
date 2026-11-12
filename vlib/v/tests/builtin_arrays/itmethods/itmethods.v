module itmethods

// any tests a scalar argument to a module function.
pub fn any(x int) bool { return x == 10 }

// count tests an array argument to a module function.
pub fn count(xs []int) bool { return xs == [10] }
