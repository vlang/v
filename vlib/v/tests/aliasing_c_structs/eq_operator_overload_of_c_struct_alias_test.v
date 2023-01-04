#include "@VMODROOT/cstructs.h"

struct C.__mpz_struct1 {
	x int
}

[typedef]
struct C.__mpz_struct_typedef {
	x int
}

type Bigint = C.__mpz_struct1
type BigintTypedef = C.__mpz_struct_typedef

pub fn (a Bigint) == (b Bigint) bool {
	println('>>>>         struct a.x: ${a.x} | == | b.x: ${b.x}  => ${a.x == b.x:-5} <<<<')
	return a.x == b.x
}

pub fn (a BigintTypedef) == (b BigintTypedef) bool {
	println('>>>> typedef struct a.x: ${a.x} | == | b.x: ${b.x}  => ${a.x == b.x:-5} <<<<')
	return a.x == b.x
}

fn test_ordinary_c_struct_aliases_with_an_equal_op_overload() {
	bi_1 := Bigint{
		x: 12
	}
	bi_2 := Bigint{
		x: 34
	}
	bi_3 := Bigint{
		x: 12
	}
	println(bi_1)
	println(bi_2)
	assert bi_1 == bi_1
	assert bi_1 == bi_3
	assert bi_1 != bi_2
}

fn test_typedefed_c_struct_aliases_with_an_equal_op_overload() {
	bit_1 := BigintTypedef{
		x: 55
	}
	bit_2 := BigintTypedef{
		x: 99
	}
	bit_3 := BigintTypedef{
		x: 55
	}
	println(bit_1)
	println(bit_2)
	println(bit_3)
	assert bit_1 == bit_1
	assert bit_1 == bit_3
	assert bit_1 != bit_2
}
