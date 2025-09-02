// vtest vflags: -d new_2 -d new_a_2 -d new_b_2 -d new_c_2 -d new_d_2 -d new_e_2
module main

// this is comment, should skip

$if new_1 ? {
	// this is comment, should skip
	import os
	// this is comment, should skip
} $else $if new_2 ? {
	// this is comment, should skip
	import math
	// this is comment, should skip
} $else {
	// this is comment, should skip
	import time
	// this is comment, should skip
}
// this is comment, should skip

const t = $if amd64 { 1 } $else { 2 }

$if new_a_1 ? {
	pub type Digits = u64
} $else $if new_a_2 ? {
	pub type Digits = u32
} $else {
	pub type Digits = u8
}

$if new_b_1 ? {
	pub const const1 = '123'
} $else $if new_b_2 ? {
	pub const const1 = 123
} $else {
	pub const const1 = 1.1
}

$if new_c_1 ? {
	pub enum Enum1 {
		enum1_a
		enum1_b
	}
} $else $if new_c_2 ? {
	pub enum Enum1 {
		enum1_c
		enum1_d
	}
} $else {
	pub enum Enum1 {
		enum1_e
		enum1_f
	}
}

$if new_d_1 ? {
	pub struct Struct1 {
		a int
	}
} $else $if new_d_2 ? {
	pub struct Struct1 {
		b int
	}
} $else {
	pub struct Struct1 {
		c int
	}
}

$if new_e_1 ? {
	pub fn ret() string {
		return 'new_e_1'
	}
} $else $if new_e_2 ? {
	pub fn ret() string {
		return 'new_e_2'
	}
} $else {
	pub fn ret() string {
		return 'new_e_3'
	}
}

fn test_main() {
	assert math.max(1, 2) == 2
	assert t in [1, 2]
	assert sizeof(Digits) == 4 // Digits == u32
	assert const1 == 123
	_ := Enum1.enum1_d // should compile
	_ := Struct1{
		b: 123
	} // should compile
	assert ret() == 'new_e_2'
}
