module answer

pub const value = value()

// value returns the value used to initialize the same-named constant.
pub fn value() int { return 42 }

// cached_value reads the constant from its declaring module.
pub fn cached_value() int { return value }
