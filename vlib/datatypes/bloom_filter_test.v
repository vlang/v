module datatypes

import hash

fn hash_func(s string) u32 {
	val64 := hash.sum64_string(s, 0x12345678)
	return u32(val64)
}

fn test_bloom_filter_fast() {
	mut b := new_bloom_filter_fast[string](hash_func)
	b.add('hello world')
	b.add('v is awesome')
	b.add('power by v')
	assert b.exists('hello world') == true
	assert b.exists('v is awesome') == true
	assert b.exists('power by v') == true
	assert b.exists('my world') == false
}

fn test_bloom_filter_fast_normal() {
	mut b := new_bloom_filter[string](hash_func, 65536, 16) or { panic(err) }
	b.add('hello world')
	b.add('v is awesome')
	b.add('power by v')
	assert b.exists('hello world') == true
	assert b.exists('v is awesome') == true
	assert b.exists('power by v') == true
	assert b.exists('my world') == false
}

fn test_bloom_filter_false_positive() {
	// every `add` will set 8 bits in the table(total length = 16), so overflow very quickly
	mut b := new_bloom_filter[string](hash_func, 16, 8) or { panic(err) }
	b.add('hello world')
	b.add('v is awesome')
	b.add('power by v')
	assert b.exists('hello world') == true
	assert b.exists('v is awesome') == true
	assert b.exists('power by v') == true
	assert b.exists('my world') == true // false positive
}

fn test_bloom_filter_fast_union_intersection() {
	mut a := new_bloom_filter_fast[string](hash_func)
	mut b := new_bloom_filter_fast[string](hash_func)

	a.add('power by v')
	a.add('silly c')
	a.add('super rust')

	b.add('hello world')
	b.add('v is awesome')
	b.add('power by v')

	assert a.exists('power by v') == true
	assert a.exists('silly c') == true
	assert a.exists('super rust') == true
	assert a.exists('power c++') == false

	assert b.exists('hello world') == true
	assert b.exists('v is awesome') == true
	assert b.exists('power by v') == true
	assert b.exists('my world') == false

	// a || b test
	mut c := a.@union(b) or { panic(err) }
	assert c.exists('silly c') == true
	assert c.exists('super rust') == true
	assert c.exists('power c++') == false
	assert c.exists('hello world') == true
	assert c.exists('v is awesome') == true
	assert c.exists('power by v') == true
	assert c.exists('my world') == false

	// a && b test
	mut d := a.intersection(b) or { panic(err) }
	assert d.exists('silly c') == false
	assert d.exists('super rust') == false
	assert d.exists('power c++') == false
	assert d.exists('hello world') == false
	assert d.exists('v is awesome') == false
	assert d.exists('power by v') == true
	assert d.exists('my world') == false
}
