import os

const vexe = @VEXE

fn generated_windows_c(cc string, name string, source string) !string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'msvc_as_cast_cgen_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	os.write_file(source_path, source)!
	cmd := '${os.quoted_path(vexe)} -os windows -cc ${cc} -o - ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln(res.output)
	}
	assert res.exit_code == 0
	return res.output
}

fn generated_windows_msvc_c(name string, source string) !string {
	return generated_windows_c('msvc', name, source)
}

fn c_chunk(c_source string, marker string) string {
	start := c_source.index(marker) or {
		assert false
		return ''
	}
	end := if start + 3000 < c_source.len { start + 3000 } else { c_source.len }
	return c_source[start..end]
}

fn must_index(text string, needle string) int {
	return text.index(needle) or {
		assert false
		return -1
	}
}

fn must_index_after(text string, needle string, start int) int {
	return text.index_after(needle, start) or {
		assert false
		return -1
	}
}

fn test_msvc_result_call_as_cast_uses_plain_temp() ! {
	c_source := generated_windows_msvc_c('result_call_as_cast', [
		'module main',
		'',
		'struct First {',
		'	n int',
		'}',
		'struct Second {',
		'	n int',
		'}',
		'type Variant = First | Second',
		'',
		'fn make_variant() !Variant {',
		'	return Variant(Second{n: 7})',
		'}',
		'',
		'fn use_variant() !int {',
		'	value := make_variant()! as Second',
		'	other := (make_variant()!) as Second',
		'	return value.n + other.n',
		'}',
		'',
		'fn main() {',
		'	_ = use_variant() or { 0 }',
		'}',
	].join('\n'))!
	use_variant := c_chunk(c_source, 'main__use_variant(void) {')
	assert use_variant.contains('main__Second value =*')
	assert use_variant.contains('main__Second other =*')
	assert use_variant.contains('builtin____as_cast')
	assert !use_variant.contains('({')
	assert !use_variant.contains('val__make_variant')
}

fn test_msvc_sumtype_direct_call_as_cast_evaluates_source_once() ! {
	c_source := generated_windows_msvc_c('sumtype_direct_call_as_cast', [
		'module main',
		'',
		'struct Alpha {',
		'	n int',
		'}',
		'struct Beta {',
		'	n int',
		'}',
		'type Variant = Alpha | Beta',
		'',
		'fn make_variant() Variant {',
		'	return Variant(Alpha{n: 3})',
		'}',
		'',
		'fn use_variant() int {',
		'	value := make_variant() as Alpha',
		'	return value.n',
		'}',
		'',
		'fn main() {',
		'	_ = use_variant()',
		'}',
	].join('\n'))!
	use_variant := c_chunk(c_source, 'main__use_variant(void) {')
	assert use_variant.count('main__make_variant()') == 1
	assert use_variant.contains('main__Alpha value =*')
	assert use_variant.contains('builtin____as_cast')
	assert !use_variant.contains('({')
	assert !use_variant.contains('main__make_variant())._')
}

fn test_msvc_interface_direct_call_as_cast_evaluates_source_once() ! {
	c_source := generated_windows_msvc_c('interface_direct_call_as_cast', [
		'module main',
		'',
		'interface Thing {',
		'	id() int',
		'}',
		'struct Impl {',
		'	n int',
		'}',
		'fn (i Impl) id() int {',
		'	return i.n',
		'}',
		'fn make_thing() Thing {',
		'	return Impl{n: 7}',
		'}',
		'',
		'fn use_thing() int {',
		'	value := make_thing() as Impl',
		'	return value.id()',
		'}',
		'',
		'fn main() {',
		'	_ = use_thing()',
		'}',
	].join('\n'))!
	use_thing := c_chunk(c_source, 'main__use_thing(void) {')
	assert use_thing.count('main__make_thing()') == 1
	assert use_thing.contains('main__Impl value =*')
	assert use_thing.contains('builtin____as_cast')
	assert !use_thing.contains('({')
	assert !use_thing.contains('main__make_thing())._')
}

fn test_windows_gcc_plain_call_as_cast_evaluates_source_once() ! {
	c_source := generated_windows_c('gcc', 'plain_call_as_cast', [
		'module main',
		'',
		'struct Alpha {',
		'	n int',
		'}',
		'struct Beta {',
		'	n int',
		'}',
		'type Variant = Alpha | Beta',
		'',
		'fn make_variant() Variant {',
		'	return Variant(Alpha{n: 3})',
		'}',
		'',
		'fn use_variant() int {',
		'	value := make_variant() as Alpha',
		'	return value.n',
		'}',
		'',
		'fn main() {',
		'	_ = use_variant()',
		'}',
	].join('\n'))!
	use_variant := c_chunk(c_source, 'main__use_variant(void) {')
	assert use_variant.count('main__make_variant()') == 1
	assert use_variant.contains('({')
	assert use_variant.contains('builtin____as_cast')
	assert !use_variant.contains('main__make_variant())._')
}

fn test_msvc_if_expr_as_cast_with_plain_call_is_not_hoisted() ! {
	c_source := generated_windows_msvc_c('if_expr_plain_call_as_cast', [
		'module main',
		'',
		'struct Arr {',
		'	elem int',
		'}',
		'struct Other {}',
		'type Info = Arr | Other',
		'',
		'struct TypeSym {',
		'	info Info',
		'}',
		'',
		'fn sym(i int) TypeSym {',
		'	return TypeSym{info: Info(Arr{elem: i})}',
		'}',
		'',
		'fn choose(left TypeSym, idx int) Arr {',
		'	info := if left.info is Arr {',
		'		left.info as Arr',
		'	} else {',
		'		sym(idx).info as Arr',
		'	}',
		'	return info',
		'}',
		'',
		'fn main() {',
		'	_ = choose(TypeSym{info: Info(Other{})}, 3)',
		'}',
	].join('\n'))!
	choose := c_chunk(c_source, 'main__choose(main__TypeSym left, int idx) {')
	assert choose.contains('main__Arr info = ((left.info)._typ')
	assert choose.contains('main__sym(idx).info')
	assert choose.contains('builtin____as_cast')
	assert !choose.contains('/* if prepend */')
	assert !choose.contains('goto _t')
}

fn test_msvc_direct_call_as_cast_in_if_expr_stays_in_branch() ! {
	c_source := generated_windows_msvc_c('if_expr_direct_call_as_cast', [
		'module main',
		'',
		'struct Counter {',
		'mut:',
		'	n int',
		'}',
		'struct Arr {',
		'	elem int',
		'}',
		'struct Other {}',
		'type Info = Arr | Other',
		'',
		'fn make_info(mut counter Counter) Info {',
		'	counter.n++',
		'	return Info(Arr{elem: counter.n})',
		'}',
		'',
		'fn choose(left bool) int {',
		'	mut counter := Counter{}',
		'	base := Info(Arr{elem: 1})',
		'	info := if left {',
		'		base as Arr',
		'	} else {',
		'		make_info(mut counter) as Arr',
		'	}',
		'	return info.elem + counter.n',
		'}',
		'',
		'fn main() {',
		'	_ = choose(true)',
		'}',
	].join('\n'))!
	choose := c_chunk(c_source, 'main__choose(bool left) {')
	branch_guard := must_index(choose, 'if (left)')
	make_info := must_index_after(choose, 'main__make_info((voidptr)&counter)', branch_guard)
	assert !choose[..branch_guard].contains('main__make_info((voidptr)&counter)')
	assert branch_guard < make_info
	assert choose.contains('builtin____as_cast')
	assert !choose.contains('({')
}

fn test_msvc_direct_call_as_cast_keeps_and_or_short_circuit() ! {
	c_source := generated_windows_msvc_c('short_circuit_direct_call_as_cast', [
		'module main',
		'',
		'struct Counter {',
		'mut:',
		'	n int',
		'}',
		'struct FirstValue {',
		'	n int',
		'}',
		'struct SecondValue {',
		'	n int',
		'}',
		'type VariantValue = FirstValue | SecondValue',
		'',
		'fn make_v(mut counter Counter) VariantValue {',
		'	counter.n++',
		'	return VariantValue(SecondValue{n: 7})',
		'}',
		'',
		'fn check(left bool, right bool) int {',
		'	mut counter := Counter{}',
		'	if left || (make_v(mut counter) as SecondValue).n == 7 {',
		'		counter.n += 10',
		'	}',
		'	if right && (make_v(mut counter) as SecondValue).n == 7 {',
		'		counter.n += 100',
		'	}',
		'	return counter.n',
		'}',
		'',
		'fn main() {',
		'	_ = check(true, false)',
		'}',
	].join('\n'))!
	check := c_chunk(c_source, 'main__check(bool left, bool right) {')
	left_guard := must_index(check, '= (left);\n\tif (!_t')
	first_make := must_index_after(check, 'main__make_v((voidptr)&counter)', left_guard)
	right_guard := must_index_after(check, '= (right);\n\tif (_t', first_make)
	second_make := must_index_after(check, 'main__make_v((voidptr)&counter)', right_guard)
	assert check.count('main__make_v((voidptr)&counter)') == 2
	assert !check[..left_guard].contains('main__make_v((voidptr)&counter)')
	assert left_guard < first_make
	assert right_guard < second_make
	assert !check[first_make + 1..right_guard].contains('main__make_v((voidptr)&counter)')
	assert !check.contains('({')
}

fn test_msvc_result_call_as_cast_keeps_and_or_short_circuit() ! {
	c_source := generated_windows_msvc_c('short_circuit_result_call_as_cast', [
		'module main',
		'',
		'struct Counter {',
		'mut:',
		'	n int',
		'}',
		'struct FirstValue {',
		'	n int',
		'}',
		'struct SecondValue {',
		'	n int',
		'}',
		'type VariantValue = FirstValue | SecondValue',
		'',
		'fn make_v(mut counter Counter) !VariantValue {',
		'	counter.n++',
		'	return VariantValue(SecondValue{n: 7})',
		'}',
		'',
		'fn check(left bool, right bool) !int {',
		'	mut counter := Counter{}',
		'	if left || (make_v(mut counter)! as SecondValue).n == 7 {',
		'		counter.n += 10',
		'	}',
		'	if right && (make_v(mut counter)! as SecondValue).n == 7 {',
		'		counter.n += 100',
		'	}',
		'	return counter.n',
		'}',
		'',
		'fn main() {',
		'	_ = check(true, false) or { 0 }',
		'}',
	].join('\n'))!
	check := c_chunk(c_source, 'main__check(bool left, bool right) {')
	left_guard := must_index(check, '= (left);\n\tif (!_t')
	first_make := must_index_after(check, 'main__make_v((voidptr)&counter)', left_guard)
	right_guard := must_index_after(check, '= (right);\n\tif (_t', first_make)
	second_make := must_index_after(check, 'main__make_v((voidptr)&counter)', right_guard)
	assert check.count('main__make_v((voidptr)&counter)') == 2
	assert !check[..left_guard].contains('main__make_v((voidptr)&counter)')
	assert left_guard < first_make
	assert right_guard < second_make
	assert !check[first_make + 1..right_guard].contains('main__make_v((voidptr)&counter)')
	assert !check.contains('({')
	assert !check.contains('val__make_v')
}
