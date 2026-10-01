module c

import v.pref

// Loads written as a read-modify-write of 0. Each one writes the location, so it faults
// on read-only memory; see https://github.com/vlang/v/issues/29137 .
const rmw_load_forms = [
	'__atomic_fetch_add((byte*)ptr, 0, 5)',
	'__atomic_fetch_add((u16*)ptr, 0, 5)',
	'__atomic_fetch_add((u32*)ptr, 0, 5)',
	'__atomic_fetch_add((u64*)ptr, 0, 5)',
	'(uintptr_t)0, 5)',
	'__atomic_add_fetch(ptr, 0, 5)',
]

// tcc_atomic_load_calls are the tcc loads, one per width, through libtcc1.a's helpers.
const tcc_atomic_load_calls = [
	'__atomic_load_1((byte*)ptr, 5)',
	'__atomic_load_2((u16*)ptr, 5)',
	'__atomic_load_4((u32*)ptr, 5)',
	'__atomic_load_8((u64*)ptr, 5)',
]

fn atomic_helpers_test_gen() FlatGen {
	mut g := FlatGen.new()
	g.target = pref.target_from('linux', 'amd64') or { panic(err) }
	g.ccompiler = 'gcc'
	return g
}

// tcc_and_other_branches splits C at the `#else` that closes its first `#ifdef __TINYC__`,
// skipping the conditionals nested inside that branch.
fn tcc_and_other_branches(c_code string) (string, string) {
	lines := c_code.split_into_lines()
	start := lines.index('#ifdef __TINYC__')
	assert start >= 0, c_code
	mut depth := 0
	for i in start + 1 .. lines.len {
		line := lines[i]
		if line.starts_with('#if') {
			depth++
		} else if line.starts_with('#endif') {
			depth--
		} else if line.starts_with('#else') && depth == 0 {
			return lines[start + 1..i].join('\n'), lines[i + 1..].join('\n')
		}
	}
	panic('no #else closes the #ifdef __TINYC__ branch:\n${c_code}')
}

// No atomic helper, for any compiler, emulates a load with a read-modify-write.
fn test_atomic_helpers_never_emulate_a_load_with_a_read_modify_write() {
	mut g := atomic_helpers_test_gen()
	g.tinyc_atomic_libcall_decls()
	g.prealloc_atomic_compat_decls()
	g.atomic_builtin_compat_decls()
	c_code := g.sb.str()
	for form in rmw_load_forms {
		assert !c_code.contains(form), form
	}
}

// The tcc branch loads through libtcc1.a's sized helpers; gcc/clang use __atomic_load_n.
fn test_atomic_loads_are_real_loads_for_tcc_and_other_compilers() {
	mut g := atomic_helpers_test_gen()
	g.atomic_builtin_compat_decls()
	tcc, other := tcc_and_other_branches(g.sb.str())
	for call in tcc_atomic_load_calls {
		assert tcc.contains(call), call
	}
	assert other.contains('__atomic_load_n((u64*)ptr, 5)'), other
}

// tcc has no prototypes for its libcalls, so each load helper must be declared.
fn test_tcc_atomic_load_helpers_are_declared() {
	mut g := atomic_helpers_test_gen()
	g.tinyc_atomic_libcall_decls()
	c_code := g.sb.str()
	assert c_code.contains('extern byte __atomic_load_1(byte* ptr, int order);'), c_code
	assert c_code.contains('extern u16 __atomic_load_2(u16* ptr, int order);'), c_code
	assert c_code.contains('extern u32 __atomic_load_4(u32* ptr, int order);'), c_code
	assert c_code.contains('extern u64 __atomic_load_8(u64* ptr, int order);'), c_code
}

// The -prealloc arena loads are real loads for tcc and for gcc/clang too.
fn test_prealloc_atomic_loads_are_real_loads() {
	mut g := atomic_helpers_test_gen()
	g.prealloc_atomic_compat_decls()
	tcc, other := tcc_and_other_branches(g.sb.str())
	assert tcc.contains('(int)__atomic_load_4((u32*)ptr, 5)'), tcc
	assert tcc.contains('(long long)__atomic_load_8((u64*)ptr, 5)'), tcc
	assert other.contains('v_prealloc_atomic_load_i32(int *ptr) { return __atomic_load_n(ptr, 5); }'), other
	assert other.contains('v_prealloc_atomic_load_i64(long long *ptr) { return __atomic_load_n(ptr, 5); }'), other
}
