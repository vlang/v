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

type SuperSumType = AnotherSumType | MySumType | string

fn test_typeof_for_builtin_int_types() {
	assert typeof(i8(1)).name == 'i8'
	assert typeof(i16(1)).name == 'i16'
	assert typeof(int(1)).name == 'int'
	// assert typeof(1).name == 'int_literal'
	assert typeof(i64(1)).name == 'i64'
	assert typeof(u8(1)).name == 'u8'
	assert typeof(u16(1)).name == 'u16'
	assert typeof(u32(1)).name == 'u32'
	assert typeof(u64(1)).name == 'u64'
	//
	assert typeof(byte(1)).name == 'byte'
	assert typeof(char(1)).name == 'char'
}

fn test_typeof_for_builtin_float_types() {
	assert typeof(f32(1.0)).name == 'f32'
	assert typeof(f64(1.0)).name == 'f64'
	// assert typeof(1.0).name == 'float_literal'
}

fn test_typeof_for_builtin_string_type() {
	assert typeof('abc').name == 'string'
	assert typeof('/v/nv/vlib/v/tests/typeof_simple_types_test.v').name == 'string'
	assert typeof('22').name == 'string'
}

fn test_typeof_for_structs() {
	assert typeof(Abc{}).name == 'Abc'
	assert typeof(Xyz{}).name == 'Xyz'
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
