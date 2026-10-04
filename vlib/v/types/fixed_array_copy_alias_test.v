module types

import os
import v.parser
import v.pref

const fixed_array_copy_types = 'struct Hero { mut: skills [4]int choices []int }
struct Party { mut: inner &Hero = unsafe { nil } leader Hero team [2]Hero items []Hero }
'

const fixed_array_copy_alias_error = 'aliases mutable data from an immutable value, clone it first (or use `unsafe`)'

fn check_fixed_array_copy_source(name string, body string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'v3_fixed_array_copy_alias_${name}_${os.getpid()}.v')
	os.write_file(path, fixed_array_copy_types + body + '\nfn main() {}\n') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_fixed_array_field_of_a_mutable_copy_is_its_own_storage() {
	tc := check_fixed_array_copy_source('field', 'fn f(hero Hero) int {
 mut upgraded := hero
 upgraded.skills[1] = 3
 (upgraded).skills[2] = 4
 return upgraded.skills[1]
}')
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_fixed_arrays_nested_by_value_in_a_mutable_copy_are_its_own_storage() {
	tc := check_fixed_array_copy_source('nested', 'fn f(party Party) int {
 mut trained := party
 trained.leader.skills[0] = 1
 trained.team[1].skills[2] = 5
 return trained.leader.skills[0]
}')
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_dynamic_array_field_of_a_mutable_copy_still_aliases_its_source() {
	tc := check_fixed_array_copy_source('dynamic', 'fn f(hero Hero) {
 mut upgraded := hero
 upgraded.choices[0] = 3
}')
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`upgraded.choices` ${fixed_array_copy_alias_error}', tc.errors.str()
}

fn test_fixed_array_behind_a_pointer_field_still_aliases_its_source() {
	tc := check_fixed_array_copy_source('pointer_field', 'fn f(party Party) {
 mut trained := party
 trained.inner.skills[0] = 1
}')
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`trained.inner.skills` ${fixed_array_copy_alias_error}', tc.errors.str()
}

fn test_fixed_array_behind_a_dynamic_array_still_aliases_its_source() {
	tc := check_fixed_array_copy_source('dynamic_element', 'fn f(party Party) {
 mut trained := party
 trained.items[0].skills[1] = 3
}')
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`trained.items[0].skills` ${fixed_array_copy_alias_error}', tc.errors.str()
}

fn test_fixed_array_behind_a_sum_type_smartcast_still_aliases_its_source() {
	tc := check_fixed_array_copy_source('smartcast', 'type Skilled = Hero | int
struct Holder { mut: skilled Skilled names []string }
fn f(holder Holder) {
 mut copy := holder
 if mut copy.skilled is Hero {
  copy.skilled.skills[0] = 1
 }
}')
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == '`copy.skilled.skills` ${fixed_array_copy_alias_error}', tc.errors.str()
}

fn test_fixed_array_behind_a_pointer_copy_still_aliases_its_source() {
	tc := check_fixed_array_copy_source('pointer_copy', '@[heap]
struct Boxed { mut: skills [4]int }
fn f(boxed &Boxed) {
 mut alias := boxed
 alias.skills[1] = 3
}')
	assert tc.errors.any(it.msg == '`alias.skills` ${fixed_array_copy_alias_error}'), tc.errors.str()
}
