module answer

pub const value = 42

// cached_value reads a constant qualified with its declaring module.
pub fn cached_value() int { return answer.value }
