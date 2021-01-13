struct Abc {
	x int
}

struct Xyz {
	y int
}

struct XxYyZz {
	y int
}

type MySumType = Abc | Xyz

type AnotherSumType = XxYyZz | int

type SuperSumType = MySumType | AnotherSumType | string

fn test_typeof_for_builtin_int_types() {
	assert typeof(i8(1)) == 'i8'
	assert typeof(i16(1)) == 'i16'
	assert typeof(int(1)) == 'int'
	// assert typeof(1) == 'int_literal'
	assert typeof(i64(1)) == 'i64'
	assert typeof(byte(1)) == 'byte'
	assert typeof(u16(1)) == 'u16'
	assert typeof(u32(1)) == 'u32'
	assert typeof(u64(1)) == 'u64'
}

fn test_typeof_for_builtin_float_types() {
	assert typeof(f32(1.0)) == 'f32'
	assert typeof(f64(1.0)) == 'f64'
	// assert typeof(1.0) == 'float_literal'
}

fn test_typeof_for_builtin_string_type() {
	assert typeof('abc') == 'string'
	assert typeof('/v/nv/vlib/v/tests/typeof_simple_types_test.v') == 'string'
	assert typeof('22') == 'string'
}

fn test_typeof_for_structs() {
	assert typeof(Abc{}) == 'Abc'
	assert typeof(Xyz{}) == 'Xyz'
}

//
fn mysumtype_typeof(x MySumType) string {
	return typeof(x)
}

fn test_typeof_for_sumtypes() {
	z_abc := Abc{}
	z_xyz := Xyz{}
	assert mysumtype_typeof(z_abc) == 'Abc'
	assert mysumtype_typeof(z_xyz) == 'Xyz'
}

//
fn supersumtype_typeof(x SuperSumType) string {
	return typeof(x)
}

fn mst(x MySumType) MySumType {
	return x
}

fn test_typeof_for_sumtypes_of_sumtypes() {
	assert supersumtype_typeof('abc') == 'string'
	assert supersumtype_typeof(mst(Abc{})) == 'MySumType'
}
