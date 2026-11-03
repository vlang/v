import os
import time

fn run_variant_review_case(name string, source string, check_only bool) {
	root := os.join_path(os.vtmp_dir(), 'variant_review_${name}_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	binary := os.join_path(root, 'main')
	os.write_file(path, source) or { panic(err) }
	args := if check_only { [@VEXE, '-check', path] } else { [@VEXE, '-o', binary, path] }
	compiled := os.exec(args)
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
