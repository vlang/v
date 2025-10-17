// for vls module test
module sample_mod2

// public

pub struct PublicStruct2 {
	data_int    int
	data_string string
	data_u8     u8
}

pub const public_const2 = 'a public const'

pub enum PublicEnum2 {
	a
	b
	c
}

pub interface PublicInterface2 {
	val int
	method() string
}

pub type PublicAlias2 = u8 | u16 | int

pub fn public_fn2(val int) string {
	return '${val}'
}

// private

struct PrivateStruct2 {
	data        int
	data_string string
}

const private_const2 = 'a private const'

enum PrivateEnum2 {
	a
	b
	c
}

interface PrivateInterface2 {
	val int
	method() string
}

type PrivateAlias2 = u8 | u16 | int

fn private_fn2(val int) string {
	return '${val}'
}
