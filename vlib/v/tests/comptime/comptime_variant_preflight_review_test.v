import os
import time

fn run_variant_review_case(name string, source string, check_only bool) {
	check_variant_review_case(name, source, check_only, '')
}

fn check_variant_review_case(name string, source string, check_only bool, expected_error string) {
	root := os.join_path(os.vtmp_dir(), 'variant_review_${name}_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	binary := os.join_path(root, 'main')
	os.write_file(path, source) or { panic(err) }
	args := if check_only { [@VEXE, '-check', path] } else { [@VEXE, '-o', binary, path] }
	compiled := os.exec(args)
	if expected_error != '' {
		assert compiled.exit_code != 0, compiled.output
		assert compiled.output.contains(expected_error), compiled.output
		return
	}
	assert compiled.exit_code == 0, compiled.output
	if check_only {
		return
	}
	result := os.exec([binary])
	assert result.exit_code == 0, result.output
}

fn test_nested_variant_guards_preserve_both_payloads() {
	run_variant_review_case('nested', r'
type Choice = int | string
type Alias = Choice
fn payload_name[T](_ T) string { return typeof(T).name }
fn names[T](a T, b T, enabled bool) string {
	$for variant in T.variants {
		if a is variant {
			if b is variant && enabled {
				return payload_name(a) + ":" + payload_name(b)
			}
		}
	}
	return "disabled"
}
fn main() {
	assert names(Alias(42), Alias(1), true) == "int:int"
	assert names(Alias("a"), Alias("b"), true) == "string:string"
	assert names(Alias(42), Alias("b"), true) == "disabled"
	assert names(Alias(42), Alias(1), false) == "disabled"
}', false)
}

fn test_concrete_variant_guard_preserves_mutable_sum_storage() {
	run_variant_review_case('mutable', r'
type Choice = int | string
fn replace(mut value Choice) {
	$for variant in Choice.variants {
		if mut value is variant {
			value = Choice("next")
			return
		}
	}
}
fn main() {
	mut value := Choice(42)
	replace(mut value)
	assert value == Choice("next")
	replace(mut value)
	assert value == Choice("next")
}', false)
}

fn test_concrete_variant_passes_infer_independent_array_types() {
	run_variant_review_case('arrays', r'
type Choice = int | string
fn describe(value Choice) string {
	$for variant in Choice.variants {
		if value is variant {
			items := [value]
			snapshot := items.clone()
			assert snapshot.len == 1
			assert snapshot[0] == value
			return typeof(snapshot).name
		}
	}
	return "missing"
}
fn main() {
	assert describe(Choice(42)) == "[]int"
	assert describe(Choice("hello")) == "[]string"
}', false)
}

fn test_concrete_variant_guard_applies_nested_interface_smartcasts() {
	// Exercise preflight independently of native interface-in-sum lowering.
	run_variant_review_case('interface', r'
interface Named { name string }
struct Record {
	name string
	x int
}
type Choice = Named | int
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is Named {
				if value is Record { return value.x }
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(Named(Record{"record", 42}))) == 42
	assert extract(Choice(1)) == -1
}', true)
}

fn test_concrete_variant_conditions_preserve_numeric_type_ids() {
	run_variant_review_case('numeric', r'
type Choice = int | string
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ == 8 && 8 == variant.typ && variant.typ <= 8 {
				return value
			} $else {
				return value.len
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(42)) == 42
	assert extract(Choice("hello")) == 5
}', false)
}

fn test_concrete_variant_preflight_defers_outer_field_metadata() {
	run_variant_review_case('outer', r'
struct Record { marker int }
type Choice = int | string
fn extract(value Choice) int {
	$for field in Record.fields {
		$for variant in Choice.variants {
			if value is variant {
				$if variant.typ is int && field.name == "marker" {
					return value
				} $else {
					return value.len
				}
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(42)) == 42
	assert extract(Choice("hello")) == 5
}', false)
}

fn test_concrete_variant_preflight_resolves_threads_condition() {
	source := r'
type Choice = int | string
fn worker() {}
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is int && threads {
				return value
			} $else {
				return value.len
			}
		}
	}
	return -1
}
fn main() {
	job := spawn worker()
	job.wait()
	assert extract(Choice(42)) == 42
	assert extract(Choice("hello")) == 5
}'
	run_variant_review_case('threads', source, false)
	run_variant_review_case('no_threads', source.replace('&& threads', '&& !threads').replace('job := spawn worker()\n\tjob.wait()', 'worker()'), false)
}

fn test_variant_option_refinements_preserve_ordinary_option_inference() {
	run_variant_review_case('optional', r'
type Choice = ?string | int
fn text(value Choice) string {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?string {
				if value != none { return value }
				return "none"
			}
		}
	}
	return "other"
}
fn booleans(first ?bool, second ?bool) bool {
	if first != none {
		local := first
		if second != none { return local && !second }
		return local
	}
	return false
}
fn main() {
	assert text(Choice(?string("payload"))) == "payload"
	assert text(Choice(?string(none))) == "none"
	assert text(Choice(42)) == "other"
	assert booleans(true, false)
	assert !booleans(false, true)
	assert booleans(true, none)
}', false)
}

fn test_variant_preflight_defers_local_metadata_values() {
	run_variant_review_case('local_metadata', r'
type Choice = int | string
fn extract(value Choice) int {
	$for variant in Choice.variants {
		type_id := variant.typ
		if value is variant {
			$if variant.typ is int && type_id == 8 {
				return value
			} $else {
				return value.len
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(42)) == 42
	assert extract(Choice("hello")) == 5
}', false)
}

fn test_variant_preflight_normalizes_optional_metadata_ids() {
	run_variant_review_case('optional_id', r'
type Choice = ?string | int
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ == 21 {
				if value != none { return value.len }
				return 0
			} $else {
				return value
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(?string("hello"))) == 5
	assert extract(Choice(?string(none))) == 0
	assert extract(Choice(42)) == 42
}', false)
}

fn test_metadata_dependent_variant_matches_keep_arm_refinements() {
	// Exercise checker preflight independently of native interface-in-sum lowering.
	source := r'
interface Named { name string }
struct Record {
	name string
	x int
}
type Choice = Named | int
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is Named {
				match value {
					Record {
						println(variant.typ)
						return value.x
					}
					else {}
				}
			}
		}
	}
	return -1
}
fn main() {
	_ = extract(Choice(Named(Record{"record", 42})))
}'
	run_variant_review_case('match_metadata', source, true)
	check_variant_review_case('invalid_match_metadata', source.replace('return value.x', 'return value.missing'), true, 'type `Record` has no field named `missing`')
}

fn test_variant_preflight_unwraps_if_guard_bindings() {
	source := r'
type Choice = ?int | string
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?int {
				if payload := value {
					return payload
				} else {
					return 0
				}
			} $else {
				return value.len
			}
		}
	}
	return -1
}
fn bump(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?int {
				if mut payload := value {
					payload += 1
					println(variant.typ)
					return payload
				} else {
					println(variant.typ)
					return 0
				}
			}
		}
	}
	return -1
}
fn scoped(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?int {
				if payload := value {
					assert payload == 42
					println(variant.typ)
				}
				payload := "after"
				return payload.len
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(?int(42))) == 42
	assert extract(Choice(?int(none))) == 0
	assert extract(Choice("hello")) == 5
	assert bump(Choice(?int(42))) == 43
	assert bump(Choice(?int(none))) == 0
	assert scoped(Choice(?int(42))) == 5
	assert scoped(Choice(?int(none))) == 5
}'
	run_variant_review_case('option_guard', source, false)
	check_variant_review_case('invalid_option_guard', source.replace('return payload\n', 'return payload.len\n'), true, '`int` has no property `len`')
}

fn test_variant_preflight_preserves_option_guard_fallthrough() {
	source := r'
type Choice = ?int | string
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?int {
				if value == none { return 0 }
				return value
			} $else {
				return value.len
			}
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(?int(42))) == 42
	assert extract(Choice(?int(none))) == 0
	assert extract(Choice("text")) == 4
}'
	run_variant_review_case('none_guard_fallthrough', source, false)
	enclosed := source.replace('fn extract', 'fn enabled() bool { return true }\nfn extract').replace('return value\n', 'if enabled() { return value }; return 0\n')
	run_variant_review_case('none_guard_unrelated_if', enclosed, false)
	scalar_match := source.replace('return value\n', 'match value { 42 { return value } else { return value } }\n')
	run_variant_review_case('none_guard_scalar_match', scalar_match, false)
	check_variant_review_case('invalid_fallthrough_payload', source.replace('return value\n', 'return value.len\n'), true, '`int` has no property `len`')
	scoped := source.all_before('fn main()').replace('fn extract(value Choice) int', 'fn extract(value Choice, enabled bool) int').replace('if value == none { return 0 }', 'if enabled {\n\t\t\t\t\tif value == none { return 0 }\n\t\t\t\t\tprintln(variant.typ)\n\t\t\t\t}') + 'fn main() { _ = extract(Choice(?int(none)), false) }'
	check_variant_review_case('conditional_fallthrough_scope', scoped, true, '`?int` as type `int`')
}

fn test_variant_preflight_resolves_reflected_fallthrough_targets() {
	deferred := r'
type Choice = int | string
fn consume(value Choice) { assert value is int || value is string }
fn visit(value Choice) {
	$for variant in Choice.variants {
		if value !is variant { return }
		_ = variant.typ
		consume(value)
	}
}
fn main() {
	visit(Choice(42))
	visit(Choice("text"))
}'
	run_variant_review_case('deferred_reflected_fallthrough', deferred, false)
	run_variant_review_case('deferred_reflected_assert', deferred.replace('consume(value)\n', 'assert value is variant\n\t\tconsume(value)\n'), false)
	run_variant_review_case('outer_reflected_fallthrough', r'
type Choice = int | string
type Other = bool | int
fn consume(value Choice) { assert value is int || value is string }
fn visit(value Choice) {
	$for outer in Choice.variants {
		$for inner in Other.variants {
			if value !is outer { return }
			_ = outer.typ
			_ = inner.typ
			consume(value)
		}
	}
}
fn main() {
	visit(Choice(42))
	visit(Choice("text"))
}', false)
	source := r'
type Choice = int | string
fn extract(value Choice) int {
	$for variant in Choice.variants {
		if value is variant {
			if value !is variant { return 0 }
			$if variant.typ is int { return value } $else { return value.len }
		}
	}
	return -1
}
fn main() {
	assert extract(Choice(42)) == 42
	assert extract(Choice("text")) == 4
}'
	run_variant_review_case('concrete_reflected_fallthrough', source, false)
	run_variant_review_case('concrete_reflected_assert', source.replace('if value !is variant { return 0 }', 'assert value is variant'), false)
	check_variant_review_case('invalid_reflected_fallthrough_payload', source.replace('return value }', 'return value.len }'), true, '`int` has no property `len`')
}

fn test_variant_preflight_preserves_local_storage_after_guards() {
	source := r'
type Choice = int | string
fn visit(value Choice) {
	$for variant in Choice.variants {
		_ = variant.typ
		if value !is int { return }
		mut local := value
		local = Choice("text")
		assert local is string
	}
}
fn main() { visit(Choice(42)) }'
	// These controls check acceptance of declared local storage during preflight.
	run_variant_review_case('declared_fallthrough_local', source, true)
	run_variant_review_case('declared_assert_local', source.replace('if value !is int { return }', 'assert value is int'), true)
	run_variant_review_case('metadata_assignment_after_none_guard', r'
type Choice = int | string
fn extract() int {
	mut x := ?int(42)
	$for variant in Choice.variants {
		if x == none { return 0 }
		x = ?int(variant.typ)
		if payload := x { return payload }
	}
	return -1
}
fn main() { assert extract() == 8 }', false)
}

fn test_variant_preflight_keeps_option_layers_separate() {
	source := r'
type Choice = ?int | string
fn extract(value ?Choice) int {
	if value == none { return 0 }
	$for variant in Choice.variants {
		if value is variant {
			$if variant.typ is ?int { return value or { 0 } } $else { return value.len }
		}
	}
	return -1
}
fn main() {
	assert extract(?Choice(none)) == 0
	assert extract(?Choice(Choice(?int(none)))) == 0
	assert extract(?Choice(Choice(?int(42)))) == 42
	assert extract(?Choice(Choice("text"))) == 4
}'
	// The base compiler also has native codegen failures for this nested option shape.
	run_variant_review_case('outer_option_layer', source, true)
	compound := source.replace('fn extract', 'fn enabled() bool { return true }\nfn extract').replace('if value is variant {', 'if value is variant && enabled() {')
	run_variant_review_case('outer_option_compound_layer', compound, true)
	matched := source.replace('if value is variant {\n\t\t\t$if variant.typ is ?int { return value or { 0 } } $else { return value.len }\n\t\t}', '_ = variant.typ\n\t\tmatch value {\n\t\t\t?int { return value or { 0 } }\n\t\t\tstring { return value.len }\n\t\t}')
	run_variant_review_case('outer_option_match_layer', matched, true)
	for name, guarded in {
		'if':    source
		'match': matched
	} {
		run_variant_review_case('inner_option_guard_${name}', guarded.replace('return value or { 0 }', 'if value == none { return 0 }; return value'), true)
	}
	check_variant_review_case('inner_option_requires_unwrap', source.replace('return value or { 0 }', 'return value'), true, '`?int` as type `int`')
}
