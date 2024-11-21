// Copyright (c) 2022, 2023 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

fn test_sort_the_set() {
	mut objs := Set.new()!

	val12 := Integer.from_i64(12)
	val32 := Integer.from_i64(32)
	valbol := Boolean.new(false)
	valnull := Null{}
	valapp := RawElement.new(Tag{.application, false, 34}, [u8(44), 45])!
	valctx := RawElement.new(Tag{.context_specific, false, 35}, [u8(50), 55])!

	objs.add_element(val12)! // tag: 2
	objs.add_element(val32)! // tag: 2
	objs.add_element(valctx)! //
	objs.add_element(valapp)! //
	objs.add_element(valbol)! // tag: 1
	objs.add_element(valnull)! // tag: 5

	awal := objs.fields.clone()
	mut exp := Set.new()!
	exp.add_element(valbol)!
	exp.add_element(val12)!
	exp.add_element(val32)!
	exp.add_element(valnull)!
	exp.add_element(valapp)!
	exp.add_element(valctx)!
	// [valbol, val12, val32, valnull, valapp, valctx]}

	objs.sort_set_fields()

	// this not compiled with -cstrict option
	// assert awal != objs.fields

	assert objs.equal(exp)
}

fn test_sort_the_setof() ! {
	mut objs := SetOf.new[Integer]()!

	val1 := Integer.from_i64(1)
	val2 := Integer.from_i64(12)
	val3 := Integer.from_i64(323)
	val4 := Integer.from_i64(4325)
	val5 := Integer.from_i64(44446)
	val0 := Integer.from_i64(0)
	val6 := Boolean.new(false)

	// randomly added to array
	objs.add_element(val4)! // tag: 2
	objs.add_element(val2)! // tag: 2
	objs.add_element(val5)! // tag: 2
	objs.add_element(val1)! // tag: 2
	objs.add_element(val3)! // tag: 2
	objs.add_element(val0)! // tag: 2

	awal := objs.fields.clone()
	awalexp := [val4, val2, val5, val1, val3, val0]
	assert awal == awalexp
	// dump(objs)

	objs.sort_setof_fields()

	// dump(objs)
	exp := [val0, val1, val2, val3, val4, val5]
	assert awal != objs.fields
	assert objs.fields == exp
}

fn test_set_encode() ! {
	mut set1 := Set.new()!

	set1.add_element(Boolean.new(false))!
	set1.add_element(Null{})!
	set1.add_element(Integer.from_int(4))!
	// boolean tag:1 length: 3, integer tag:2 length: 3, null tag: 5 length: 2, total length: 8
	// so, it should sort to
	// [boolean, integer, null]
	exp := [u8(0x31), 8, 1, 1, 0, 2, 1, 4, 5, 0]
	out := encode(set1)!

	assert out == exp

	back, _ := Set.decode(exp)!

	backout := encode(back)!
	assert out == backout
}

fn test_setof_encode() ! {
	mut set1 := SetOf.new[Integer]()!

	set1.add_element(Integer.from_int(55))!
	set1.add_element(Integer.from_int(4))!
	set1.add_element(Integer.from_int(666))!
	// 666 serialized to [2, 154]
	// encoded sort: [int1, int2, int3]
	exp := [u8(0x31), 10, 2, 1, 4, 2, 1, 55, 2, 2, 2, 0x9a]
	out := encode(set1)!

	assert out == exp
	// todo : decode back
}
