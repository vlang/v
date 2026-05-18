// vtest build: !linux && !windows
module cleanc

import os
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_flag_enum_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

fn test_generate_c_rewrites_flag_enum_zero_static_call() {
	csrc := generate_c_for_test('
@[flag]
enum Bits {
	a
	b
}

fn main() {
	mut bits := Bits.zero()
	bits.set(.a)
	_ = bits.has(.a)
}
')
	assert csrc.contains('(Bits)(0)')
	assert !csrc.contains('Bits__zero')
}

fn test_generate_c_escapes_keyword_struct_fields_in_map_eq() {
	csrc := generate_c_for_test("
struct Item {
	short string
}

fn main() {
	a := {
		'x': Item{short: 'a'}
	}
	b := {
		'x': Item{short: 'b'}
	}
	_ = a == b
}
")
	assert csrc.contains('string__eq(va._short, vb._short)')
	assert !csrc.contains('string__eq(va.short, vb.short)')
}

fn test_generate_c_uses_concrete_map_method_name_in_generic_comptime_body() {
	code := [
		'fn (m map[string]int) query_item(name string) ?int {',
		'	return m[name]',
		'}',
		'',
		'struct Holder {',
		'	items map[string]int',
		'}',
		'',
		'fn find_item[T](value T) ?int {',
		'	@DLR@if T is Holder {',
		'		return value.items.query_item(@SQ@x@SQ@)',
		'	}',
		'	return none',
		'}',
		'',
		'fn main() {',
		'	_ = find_item[Holder](Holder{})',
		'}',
	].join('\n').replace('@DLR@', '$').replace('@SQ@', "'")
	csrc := generate_c_for_test(code)
	assert csrc.contains('Map_string_int__query_item(')
	assert !csrc.contains('map__query_item(')
}

fn test_generate_c_filters_lifetime_params_from_generic_struct_binding() {
	csrc := generate_c_for_test('
struct Value {
	n int
}

struct Ref[^a, T] {
	value T
}

struct Holder[^a] {
	item Ref[^a, Value]
}
')
	assert csrc.contains('struct Ref {')
	assert csrc.contains('Value value;')
	assert !csrc.contains('T value;')
}

fn test_generate_c_lowers_pointer_type_params_receivers_fields_and_generics() {
	csrc := generate_c_for_test('
struct Foo {
	value int
}

struct Node[T] {
	value T
}

struct Holder {
	item &Foo
	node &Node[Foo]
}

fn ptr_value(foo &Foo) int {
	return foo.value
}

fn (foo &Foo) method_value() int {
	return foo.value
}

fn main() {
	foo := Foo{}
	_ := ptr_value(&foo)
	_ := foo.method_value()
}
')
	assert csrc.contains('Foo* item;')
	assert csrc.contains('Node* node;')
	assert csrc.contains('Foo value;')
	assert csrc.contains('ptr_value(Foo* foo)')
	assert csrc.contains('Foo__method_value(Foo* foo)')
	assert !csrc.contains('int item;')
	assert !csrc.contains('int ptr_value(int foo)')
}

fn test_generate_c_match_on_enum_does_not_constrain_branch_array_literals() {
	csrc := generate_c_for_test('
enum Choice {
	color
	none
}

fn choices(id Choice) []string {
	return match id {
		.color { ["never", "auto"] }
		.none { []string{} }
	}
}
')
	assert csrc.contains('sizeof(string)')
	assert !csrc.contains('&(Choice[')
}

fn test_generate_c_struct_eq_recurses_into_nested_string_fields() {
	csrc := generate_c_for_test('
struct Encoding {
	label string
}

enum Kind {
	auto
	disabled
	some
}

struct Mode {
	kind     Kind = .auto
	encoding Encoding
}

fn same(a Mode, b Mode) bool {
	return a == b
}
')
	assert csrc.contains('string__eq(')
	assert csrc.contains('.encoding.label')
}

fn test_generate_c_emits_generic_method_helper_specialization_body() {
	csrc := generate_c_for_test('
struct Mapper {}

struct Schema {}

fn (mut m Mapper) helper[T]() int {
	return 1
}

fn (mut m Mapper) parse[T]() int {
	return m.helper[T]()
}

fn main() {
	mut m := Mapper{}
	_ = m.parse[Schema]()
}
')
	assert csrc.contains('int Mapper__parse_T_Schema(Mapper* m) {')
	assert csrc.contains('int Mapper__helper_T_Schema(Mapper* m) {')
}

fn test_generate_c_specializes_implicit_generic_function_calls_from_all_args() {
	csrc := generate_c_for_test('
struct Left {}
struct RightA {}
struct RightB {}

fn pair[T, U](x T, y U) int {
	_ = x
	_ = y
	return 1
}

fn main() {
	left := Left{}
	a := RightA{}
	b := RightB{}
	_ = pair(left, a)
	_ = pair(left, b)
}
')
	assert csrc.contains('int pair_T_Left_RightA(Left x, RightA y);')
	assert csrc.contains('int pair_T_Left_RightB(Left x, RightB y);')
	assert csrc.contains('pair_T_Left_RightA(left, a)')
	assert csrc.contains('pair_T_Left_RightB(left, b)')
	assert !csrc.contains('pair(left, a)')
	assert !csrc.contains('pair(left, b)')
}

fn test_generate_c_does_not_specialize_plain_generic_function_for_struct_fields() {
	csrc := generate_c_for_test('
enum Pattern {
	a
}

struct Thing {
	pattern Pattern
	named bool
}

fn (thing Thing) find_at() int {
	_ = thing
	return 1
}

fn find[M](matcher M) int {
	return matcher.find_at()
}

fn main() {
	thing := Thing{}
	_ = find(thing)
}
')
	assert csrc.contains('int find_T_Thing(Thing matcher) {')
	assert csrc.contains('return Thing__find_at(matcher);')
	assert !csrc.contains('find_T_Pattern')
	assert !csrc.contains('find_T_bool')
	assert !csrc.contains('Pattern__find_at')
	assert !csrc.contains('bool__find_at')
}

fn test_generate_c_specializes_nested_generic_calls_from_active_bindings() {
	csrc := generate_c_for_test('
struct Matcher {}
struct Captures {}

fn outer[M, T](matcher M, mut caps T) {
	inner(matcher, mut caps)
}

fn inner[M, T](matcher M, mut caps T) {
	_ = matcher
	_ = caps
}

fn main() {
	matcher := Matcher{}
	mut caps := Captures{}
	outer(matcher, mut caps)
}
')
	assert csrc.contains('void outer_T_Matcher_Captures(Matcher matcher, Captures* caps) {')
	assert csrc.contains('inner_T_Matcher_Captures(matcher, caps);')
	assert csrc.contains('void inner_T_Matcher_Captures(Matcher matcher, Captures* caps) {')
	assert !csrc.contains('inner(matcher, caps)')
}

fn test_generate_c_emits_value_receiver_methods_used_by_interface_wrappers() {
	csrc := generate_c_for_test('
interface Counter {
	len() int
}

struct Counts {}

fn (counts Counts) len() int {
	_ = counts
	return 1
}

fn use_counter(counter Counter) int {
	return counter.len()
}

fn main() {
	counts := Counts{}
	_ = use_counter(counts)
}
')
	assert csrc.contains('int Counts__len(Counts counts) {')
	assert csrc.contains('static int __iface_wrap_Counter_Counts_len(void* _obj) {')
	assert csrc.contains('return Counts__len(*(((Counts*)_obj)));')
}

fn test_generate_c_preserves_void_result_or_block_side_effects() {
	csrc := generate_c_for_test('
fn may_fail() ? {
	return none
}

fn main() {
	mut saw_error := false
	may_fail() or {
		saw_error = true
	}
	_ = saw_error
}
')
	assert csrc.contains('saw_error = true;')
}

fn test_generate_c_passes_mut_generic_param_address_as_existing_pointer() {
	csrc := generate_c_for_test('
struct Captures {}

fn visit[T](mut value T, cb fn (&T)) {
	cb(&value)
}

fn main() {
	mut captures := Captures{}
	visit(mut captures, fn (_captures &Captures) {})
}
')
	assert csrc.contains('((void (*)(Captures*))cb)(value);')
	assert !csrc.contains('cb(&value);')
}

fn test_generate_c_captures_mut_param_as_existing_pointer() {
	csrc := generate_c_for_test('
fn touch(mut dst []u8) {
	dst << u8(1)
}

fn outer(mut dst []u8) {
	cb := fn [mut dst] () {
		touch(mut dst)
	}
	cb()
}

fn main() {
	mut dst := []u8{}
	outer(mut dst)
}
')
	assert csrc.contains('_capture_0 = dst;')
	assert !csrc.contains('_capture_0 = &dst;')
}

fn test_generate_c_rewrites_continue_in_generic_comptime_field_loop() {
	code := [
		'struct Schema {',
		'	skip int',
		'	keep int',
		'}',
		'',
		'fn count_fields[T]() int {',
		'	mut n := 0',
		'	@DLR@for field in T.fields {',
		'		if field.name == @SQ@skip@SQ@ {',
		'			continue',
		'		}',
		'		n++',
		'	}',
		'	return n',
		'}',
		'',
		'fn main() {',
		'	_ = count_fields[Schema]()',
		'}',
	].join('\n').replace('@DLR@', '$').replace('@SQ@', "'")
	csrc := generate_c_for_test(code)
	assert csrc.contains('goto __v_ctf_continue_')
	assert csrc.contains('__v_ctf_continue_')
}

fn test_generate_c_exposes_generic_comptime_field_attrs() {
	code := [
		'struct Schema {',
		'	field int @[repeats; short: u]',
		'}',
		'',
		'fn attrs[T]() []string {',
		'	mut out := []string{}',
		'	@DLR@for field in T.fields {',
		'		out = field.attrs',
		'	}',
		'	return out',
		'}',
		'',
		'fn main() {',
		'	_ = attrs[Schema]()',
		'}',
	].join('\n').replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('"repeats"')
	assert csrc.contains('"short: u"')
}

fn test_generate_c_struct_default_does_not_write_string_value_to_pointer_field() {
	csrc := generate_c_for_test('
struct Builder[^a] {
	glob &^a string
}

fn make[^a]() &Builder[^a] {
	return &Builder[^a]{}
}

fn (mut b Builder[^a]) touch[^a]() &Builder[^a] {
	return &b
}
')
	assert csrc.contains('string* glob;')
	assert !csrc.contains('.glob = (string)')
}

fn test_generate_c_specializes_plain_method_on_each_generic_receiver_instance() {
	csrc := generate_c_for_test('
enum MatchKind {
	none
	hit
}

struct ValueA {}
struct ValueB {}

struct Match[T] {
	kind  MatchKind = .none
	value T
}

fn first() Match[ValueA] {
	return Match[ValueA]{
		kind: .hit
		value: ValueA{}
	}
}

fn second() Match[ValueB] {
	return Match[ValueB]{
		kind: .hit
		value: ValueB{}
	}
}

fn (m Match[T]) is_hit() bool {
	return m.kind == .hit
}

fn main() {
	_ = first().is_hit()
	_ = second().is_hit()
}
')
	assert csrc.contains('bool Match__is_hit(Match m) {')
	assert csrc.contains('bool Match_T_ValueB__is_hit(Match_T_ValueB m);')
	assert csrc.contains('bool Match_T_ValueB__is_hit(Match_T_ValueB m) {')
	assert csrc.contains('return ((Match_T_ValueB){.kind = MatchKind__hit')
	assert csrc.contains('Match__is_hit(first())')
	assert csrc.contains('Match_T_ValueB__is_hit(second())')
	assert !csrc.contains('Match__is_hit(second())')
}

fn test_generate_c_declares_generic_struct_literal_with_specialized_type() {
	csrc := generate_c_for_test('
struct Other {}
struct Value {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn make_match() Match[Value] {
	return Match[Value]{}
}

fn main() {
	_ = make_other()
	mut mat := Match[Value]{}
	mat = make_match()
}
')
	assert csrc.contains('Match_T_Value mat = ((Match_T_Value){0});')
	assert csrc.contains('mat = make_match();')
	assert !csrc.contains('Match mat = ((Match_T_Value){0});')
}

fn test_generate_c_declares_lifetime_generic_struct_literal_with_specialized_type() {
	csrc := generate_c_for_test('
struct Other {}
struct IgnoreMatch[^a] {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn matched[^a]() Match[IgnoreMatch[^a]] {
	_ = make_other()
	mut mat := Match[IgnoreMatch[^a]]{}
	return mat
}

fn main() {
	_ = matched()
	}
	')
	assert csrc.contains('Match_T_IgnoreMatch matched()')
	assert !csrc.contains('\tMatch mat = ((Match_T_IgnoreMatch){0});')
}

fn test_generate_c_uses_specialized_lifetime_generic_type_for_if_expr_and_array_literal() {
	csrc := generate_c_for_test('
struct Other {}
struct IgnoreMatch[^a] {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn make_match[^a]() Match[IgnoreMatch[^a]] {
	return Match[IgnoreMatch[^a]]{}
}

fn matched[^a](flag bool) Match[IgnoreMatch[^a]] {
	_ = make_other()
	a := make_match()
	b := make_match()
	mut selected := if flag {
		make_match()
	} else {
		Match[IgnoreMatch[^a]]{}
	}
	for mat in [a, b, selected] {
		return mat
	}
	return Match[IgnoreMatch[^a]]{}
}
')
	assert csrc.contains('Match_T_IgnoreMatch selected = ({ Match_T_IgnoreMatch _if_expr_t')
	assert csrc.contains('new_array_from_c_array(3, 3, sizeof(Match_T_IgnoreMatch)')
	assert csrc.contains('Match_T_IgnoreMatch mat = ')
	assert !csrc.contains('new_array_from_c_array(3, 3, sizeof(int)')
}

fn test_generate_c_fixed_array_index_expr_has_element_type() {
	csrc := generate_c_for_test('
const month_days = [31, 28, 31]!

fn max_day(month int) int {
	value := month_days[month - 1] + 1
	return value
}
')
	assert csrc.contains('int value = (month_days')
	assert !csrc.contains('fixed_int_3 value')
}

fn test_generate_c_indexes_mut_array_receiver_through_pointer_data() {
	csrc := generate_c_for_test('
fn (mut a []string) touch_each() {
	for mut s in a {
		s = s.clone()
	}
}

fn main() {
	mut items := ["x"]
	items.touch_each()
}
	')
	assert csrc.contains('touch_each')
	assert !csrc.contains('(a).data')
}

fn test_generate_c_indexes_variadic_string_param_as_array() {
	csrc := generate_c_for_test('
fn use_patterns(patterns ...string) {
	for pattern in patterns {
		_ = pattern
	}
}

fn main() {
	use_patterns("*.v")
}
')
	assert csrc.contains('use_patterns')
	assert !csrc.contains('(patterns).str')
}

fn test_generate_c_drops_leaked_static_type_receiver_in_constructor_call() {
	csrc := generate_c_for_test('
struct Match {}

fn Match.new(start int, end int) Match {
	_ = start
	_ = end
	return Match{}
}

fn build(end int) []Match {
	return [Match.new(0, end)]
}
')
	assert csrc.contains('Match__new(0, end)')
	assert !csrc.contains('Match__new(Match,')
}

fn test_generate_c_lowers_map_literal_for_in_before_array_fallback() {
	csrc := generate_c_for_test("
fn collect() []string {
	mut res := []string{}
	for label, v in {
		'days': 24
		'h': 1
	} {
		res << '\${v}\${label}'
	}
	return res
}
")
	assert csrc.contains('DenseArray__key')
	assert csrc.contains('string label =')
	assert !csrc.contains('for (int label = 0;')
	assert !csrc.contains('+ label))')
}

fn test_generate_c_lowers_map_index_assign_after_empty_map_literal_decl() {
	csrc := generate_c_for_test('
struct Inst {
	typ    Kind
	target int
}

enum Kind {
	split
	jmp
	other
}

struct Compiler {
mut:
	prog []Inst
}

fn (mut c Compiler) mark() {
	mut targets := map[int]bool{}
	for inst in c.prog {
		if inst.typ == .split || inst.typ == .jmp {
			targets[inst.target] = true
		}
	}
}
')
	assert csrc.contains('map__set(&targets')
	assert !csrc.contains('cannot resolve map type for index expr')
}

fn test_generate_c_lowers_mut_array_for_in_after_cap_only_array_literal_decl() {
	csrc := generate_c_for_test('
struct Inst {
mut:
	n int
}

fn rewrite() []Inst {
	mut new_prog := []Inst{cap: 4}
	new_prog << Inst{
		n: 1
	}
	for mut inst in new_prog {
		inst.n = 2
	}
	return new_prog
}
')
	assert csrc.contains('Inst* inst')
	assert !csrc.contains('for (; ; )')
}

fn test_generate_c_uses_string_substr_for_array_string_index_slice() {
	csrc := generate_c_for_test("
const names = ['January']!

fn short() string {
	return names[0][0..3]
}

fn main() {
	_ = short()
}
")
	assert csrc.contains('string__substr(')
	assert !csrc.contains('array__slice(names')
}

fn test_generate_c_lowers_map_field_for_in_with_ignored_key() {
	csrc := generate_c_for_test('
struct Def {
	name string
}

struct Builder {
	types map[string]Def
}

fn collect(builder Builder) []Def {
	mut defs := []Def{}
	for _, def in builder.types {
		defs << def
	}
	return defs
}

fn main() {
	_ = collect(Builder{
		types: map[string]Def{}
	})
}
')
	assert csrc.contains('DenseArray__value')
	assert !csrc.contains('cannot resolve map type for index expr')
}

fn test_generate_c_uses_string_methods_after_branch_assignment() {
	csrc := generate_c_for_test('
fn count_matches(files []string, pat string) int {
	mut count := 0
	for file in files {
		mut f := ""
		if file.contains("/") {
			parts := file.split("/")
			f = parts[parts.len - 1]
		} else {
			f = file
		}
		if f.starts_with(pat) || f.ends_with(pat) {
			count++
		}
	}
	return count
}

fn main() {
	_ = count_matches(["abc"], "a")
}
')
	assert csrc.contains('string__starts_with(f, pat)')
	assert csrc.contains('string__ends_with(f, pat)')
	assert !csrc.contains('int__starts_with')
	assert !csrc.contains('int__ends_with')
}

fn test_generate_c_lowers_sort_comparator_on_array_selector() {
	csrc := generate_c_for_test('
struct Def {
mut:
	globs []string
}

fn ordered(def Def) Def {
	mut cloned := def
	cloned.globs.sort(a < b)
	return cloned
}

fn main() {
	_ = ordered(Def{})
}
')
	assert csrc.contains('array__sort_with_compare')
	assert csrc.contains('compare_strings')
	assert !csrc.contains('array__sort(&cloned.globs, (a < b))')
}

fn test_generate_c_uses_string_methods_after_if_expr_assignment() {
	csrc := generate_c_for_test('
fn count_matches(files []string, dir string, pat string) int {
	mut count := 0
	for file in files {
		mut fpath := file
		f := if file.contains("/") {
			pathwalk := file.split("/")
			pathwalk[pathwalk.len - 1]
		} else {
			fpath = if dir == "." { file } else { dir + "/" + file }
			file
		}
		if f.starts_with(pat) || f.ends_with(pat) || f.contains(pat) {
			count++
		}
		_ = fpath
	}
	return count
}

fn main() {
	_ = count_matches(["abc"], ".", "a")
}
')
	assert csrc.contains('string__starts_with(f, pat)')
	assert csrc.contains('string__ends_with(f, pat)')
	assert csrc.contains('string__contains(f, pat)')
	assert !csrc.contains('int__starts_with')
	assert !csrc.contains('int__ends_with')
	assert !csrc.contains('int__contains')
}
