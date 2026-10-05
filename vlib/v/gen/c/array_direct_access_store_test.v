module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

// An element store in a function marked @[direct_array_access] has to go through
// the array data pointer, the same way an element load does. When it did not, a
// hot loop that writes array elements compiled to an array__set() call per
// element while the reads next to it were inlined, which cost 2x to 11x on
// write-heavy loops. The load below the store is direct in both functions, so
// only the store is different.
fn test_direct_array_access_makes_element_stores_direct() {
	path := os.join_path(os.vtmp_dir(), 'direct_array_store_${os.getpid()}.v')
	os.write_file(path, "@[direct_array_access]
fn direct_store(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] = u64(i)
	}
}

@[direct_array_access]
fn direct_add(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] += 1
	}
}

@[direct_array_access]
fn direct_bits(mut a []u64) {
	a[0] &= 3
	a[1] /= 2
}

@[direct_array_access]
fn direct_pow(mut a []u32) {
	a[0] **= 3
}

@[direct_array_access]
fn direct_shift(mut a []u32) {
	a[0] <<= 4
}

@[direct_array_access]
fn direct_i128(mut a []u128) {
	a[0] += u128(1)
}

@[direct_array_access]
fn direct_str(mut a []string) {
	a[0] += 'x'
}

@[direct_array_access]
fn direct_fixed_elem(mut a [][2]int) {
	a[0] = [2]int{3, 4}
}

@[direct_array_access]
fn arr_of(a []u32) []u32 {
	return a
}

@[direct_array_access]
fn direct_pow_from_call(mut a []u32) {
	arr_of(a)[0] **= 2
}

fn checked_store(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] = u64(i)
	}
}

fn checked_add(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] += 1
	}
}

fn main() {
	mut a := []u64{len: 2}
	direct_store(mut a, 2)
	direct_add(mut a, 2)
	direct_bits(mut a)
	direct_i128(mut []u128{len: 1})
	mut p := []u32{len: 1}
	direct_pow(mut p)
	direct_shift(mut p)
	direct_pow_from_call(mut p)
	direct_str(mut []string{len: 1})
	direct_fixed_elem(mut [][2]int{len: 1})
	checked_store(mut a, 2)
	checked_add(mut a, 2)
}
")!
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	g.set_target(pref.target_from('linux', 'amd64') or { panic(err) })
	generated := g.gen_with_used_options(a, used, &tc, true)

	direct_store := direct_store_body(generated, 'direct_store')
	assert direct_store.contains('(*((u64*)((__v3_internal_symbol_array_store_base_0)->data) + (__v3_internal_symbol_array_store_index_0))) = '), direct_store
	assert !direct_store.contains('array__set'), direct_store

	direct_add := direct_store_body(generated, 'direct_add')
	assert direct_add.contains('__v3_internal_symbol_array_store_value_0 += '), direct_add
	assert direct_add.contains('(*((u64*)((__v3_internal_symbol_array_store_base_0)->data) + (__v3_internal_symbol_array_store_index_0))) = __v3_internal_symbol_array_store_value_0;'), direct_add
	assert !direct_add.contains('array__set'), direct_add

	direct_bits := direct_store_body(generated, 'direct_bits')
	assert direct_bits.contains('&= (3)'), direct_bits
	assert direct_bits.contains('/= (2)'), direct_bits
	assert !direct_bits.contains('array__set'), direct_bits

	// `**` and a 128-bit element have no C compound operator at all, so the
	// store has to compute the value the way the bounds-checked store does and
	// assign it, or the generated C does not compile.
	assert !generated.contains('**='), 'power assign emitted as **= '

	direct_pow := direct_store_body(generated, 'direct_pow')
	assert direct_pow.contains('__v_pow_u64'), direct_pow
	assert !direct_pow.contains('array__set'), direct_pow

	direct_shift := direct_store_body(generated, 'direct_shift')
	assert direct_shift.contains('<<'), direct_shift
	assert !direct_shift.contains('array__set'), direct_shift

	direct_i128 := direct_store_body(generated, 'direct_i128')
	assert direct_i128.contains('__v_u128_add'), direct_i128
	assert !direct_i128.contains('array__set'), direct_i128

	// `**` reads the element through the lvalue before writing it, so the base
	// and the index are hoisted into temporaries: a call in either place has to
	// run once, not once per mention of the lvalue.
	direct_pow_from_call := direct_store_body(generated, 'direct_pow_from_call')
	assert direct_pow_from_call.contains('Array* __v3_internal_symbol_array_store_base_0 = &arr_of(*a); int __v3_internal_symbol_array_store_index_0 = 0;'), direct_pow_from_call
	assert direct_pow_from_call.split('arr_of(*a)').len == 2, direct_pow_from_call

	// An element type that needs its own lowering keeps the bounds-checked store,
	// which is where that lowering is applied to the value handed to array__set().
	direct_str := direct_store_body(generated, 'direct_str')
	assert direct_str.contains('array__set'), direct_str

	direct_fixed_elem := direct_store_body(generated, 'direct_fixed_elem')
	assert direct_fixed_elem.contains('array__set'), direct_fixed_elem

	// Without the attribute the store keeps its bounds check, which is what makes
	// the two functions a pair rather than one behaviour tested twice.
	checked_store := direct_store_body(generated, 'checked_store')
	assert checked_store.contains('array__set('), checked_store

	checked_add := direct_store_body(generated, 'checked_add')
	assert checked_add.contains('array__set('), checked_add
}

// direct_store_body returns one generated function body, so the assertions read
// as the store under test instead of repeating the C signature of each one. The
// last occurrence is the definition: the generated C declares every function
// before any of them is defined.
fn direct_store_body(generated string, name string) string {
	return generated.all_after_last('void ${name}(').all_after('{').all_before('\n}').trim_space()
}
