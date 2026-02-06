type SumType = f64 | int

type MyByte = u8

struct Struct {
	a int
}

type Alias = int

fn test_main() {
	arr := [1, 2, 3]
	mut t := ?[]int([]int{len: 10, init: 2})
	assert t != none
	t = ?[]int(arr)
	assert t != none
	t = ?[]int([]int{len: 1, init: 0})
	assert t != none

	mut t2 := ?int(1)
	assert t2 != none
	mut t3 := ?f64(1.2)
	assert t3 != none
	mut t4 := ?string('')
	assert t4 != none
	mut t5 := ?SumType(1)
	assert t5 != none
	mut t6 := ?SumType(none)
	assert t6 == none
	mut t7 := ?Struct(Struct{})
	assert t7 != none
}

fn test_cast() {
	var := ?u8(1)
	println(?u8(var))
	println(?u8(?u8(255)))
	println(?int(none))
	println(?int(var?))

	a := ?Struct{}
	assert a == none
	b := ?Struct(Struct{})
	assert b != none

	mut v1 := ?bool(1)
	assert v1?.str() == 'true'
	v1 = ?bool(true)
	assert v1?.str() == 'true'
	v1 = ?bool(0)
	assert v1?.str() == 'false'
	v1 = ?bool(false)
	assert v1?.str() == 'false'
}

fn test_cast_alias() {
	assert '${?MyByte(0)}' == 'Option(MyByte(0))'
	assert '${?MyByte(255)}' == 'Option(MyByte(255))'
	assert '${?MyByte(?u8(0))}' == 'Option(MyByte(0))'
	assert '${?MyByte(?u8(255))}' == 'Option(MyByte(255))'
}
