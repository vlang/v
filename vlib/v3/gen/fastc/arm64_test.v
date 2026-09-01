module fastc

import os
import v3.pref

fn test_fastc_parser_emits_arm64_without_c() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, "\$if arm64 ? {\n\tfn selected() int {\n\t\treturn 2\n\t}\n} \$else {\n\tfn selected() int {\n\t\treturn 100\n\t}\n}\n\nfn add(a int, b int) int {\n\treturn a + b\n}\n\nfn is_letter(c u8) bool {\n\treturn (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)\n}\n\nfn main() {\n\tmut sum := 0\n\tfor sum < 3 {\n\t\tsum += 1\n\t}\n\tif add(sum, selected()) == 5 && is_letter(111) && native_features() {\n\t\t\$if arm64 ? {\n\t\t\tprintln('native')\n\t\t} \$else {\n\t\t\tprintln('wrong')\n\t\t}\n\t} else {\n\t\tprintln('wrong')\n\t}\n}\n\nenum FastArm64Mode {\n\tcold\n\twarm\n}\n\nstruct FastArm64Counter {\nmut:\n\tvalue int\n}\n\nfn (mut counter FastArm64Counter) bump() {\n\tcounter.value += 1\n}\n\nfn native_features() bool {\n\tmut counter := FastArm64Counter{}\n\tcounter.bump()\n\tmut labels := map[string]string{}\n\tlabels['backend'] = 'native'\n\tmut modes := map[string]FastArm64Mode{}\n\tmodes['backend'] = FastArm64Mode.warm\n\treturn counter.value == 1 && labels['backend'] == 'native' && modes['backend'] == .warm && 0x100000000 > 0xffffffff && `'` == 39\n}\n") or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		result := generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		assert result.source_paths.len > 0
		assert os.is_executable(output_path)
		assert !os.exists(output_path + '.c')
		run_result := os.execute(output_path)
		assert run_result.exit_code == 0
		assert run_result.output == 'native\n'
	}
}

fn test_fastc_arm64_lexical_scopes_and_array_mutation() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_scopes_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
enum FastArm64DefaultMode {
	cold
	warm
}

@[flag]
enum FastArm64Features {
	read
	write
}

struct FastArm64Defaults {
	retries int = 3
	enabled bool = true
	mode FastArm64DefaultMode = .warm
}

struct FastArm64MapDefaults {
	values map[string]int
}

struct FastArm64CloneInner {
	values map[string]int
}

struct FastArm64CloneOuter {
	inner FastArm64CloneInner
}

struct FastArm64LargeMapHolder {
	values map[string][100]int
}

struct FastArm64Equality {
	head   u64
	tail   u64
	values []int
}

struct FastArm64Waiter {
	value int
}

@[typedef]
struct C.fd_set {}

fn C.usleep(microseconds u32) int

fn (waiter FastArm64Waiter) wait() int {
	return waiter.value + 1
}

fn fast_arm64_spawned_value(base int) int {
	C.usleep(100000)
	println("spawned")
	return base + 1
}

struct FastArm64CustomError {}

struct C.fd_set {}

fn C.FD_ZERO(fdset &C.fd_set)

fn C.FD_SET(fd int, fdset &C.fd_set)

fn C.FD_ISSET(fd int, fdset &C.fd_set) int

fn (err FastArm64CustomError) msg() string {
	return "custom"
}

fn (err FastArm64CustomError) code() int {
	return 42
}

fn add_after_return(mut value int) {
	defer {
		value += 7
	}
	return
}

fn maybe_value(ok bool) ?int {
	if ok {
		return 7
	}
	return none
}

fn result_value(ok bool) !int {
	if ok {
		return 9
	}
	return error("failed")
}

fn custom_error_value() !int {
	return FastArm64CustomError{}
}

fn coded_error_value() !int {
	return error_with_code("coded", 73)
}

fn propagated_result(ok bool) !int {
	value := result_value(ok)!
	return value + 1
}

fn propagated_option(ok bool) ?int {
	value := maybe_value(ok)?
	return value + 2
}

fn fixed_array_sum(values [2]int) int {
	return values[0] + values[1]
}

fn fast_arm64_return_f64() f64 {
	return 1
}

fn fast_arm64_return_pair() (f64, u64) {
	return 2, 3
}

fn fast_arm64_accept_f64(value f64) f64 {
	return value
}

struct FastArm64FloatReceiver {}

fn (receiver FastArm64FloatReceiver) accept_f64(value f64) f64 {
	return value
}

fn main() {
	mut reassigned_float := f64(0)
	reassigned_float = 1
	if reassigned_float != 1.0 {
		println("wrong reassignment conversion")
		return
	}
	context_float := f64(1.5)
	if context_float + 1 != 2.5 || 1 + context_float != 2.5 {
		println("wrong binary conversion")
		return
	}
	mut typed_numbers := map[u64]f64{1: 2}
	typed_numbers[3] = 4
	if typed_numbers[1] != 2.0 || typed_numbers[3] != 4.0 {
		println("wrong typed map conversion")
		return
	}
	mut assigned_numbers := map[u64]f64{}
	assigned_numbers[1] = 2
	if assigned_numbers[1] != 2.0 {
		println("wrong indexed map assignment conversion")
		return
	}
	assigned_numbers.delete(1)
	if assigned_numbers.len != 0 {
		println("wrong map delete key conversion")
		return
	}
	return_left, return_right := fast_arm64_return_pair()
	receiver := FastArm64FloatReceiver{}
	if fast_arm64_return_f64() != 1.0 || return_left != 2.0 || return_right != u64(3)
		|| fast_arm64_accept_f64(4) != 4.0 || receiver.accept_f64(5) != 5.0 {
		println("wrong return or argument conversion")
		return
	}
	typed_floats := []f64{1, 2}
	if typed_floats[0] != 1.0 || typed_floats[1] != 2.0 {
		println("wrong typed array conversion")
		return
	}
	normalized_capacity := []int{len: 3, cap: 1}
	if normalized_capacity.len != 3 || normalized_capacity.cap != 3 || normalized_capacity[2] != 0 {
		println("wrong array capacity normalization")
		return
	}
	values_for_sizeof := [1, 2, 3]
	if sizeof(values_for_sizeof) != 32 {
		println("wrong dynamic array sizeof")
		return
	}
	mut fixed := [2]int{}
	fixed[0] += 2
	fixed[1] = 3
	if sizeof(fixed) != 8 || fixed_array_sum(fixed) != 5 || fixed_array_sum([2]int{}) != 0 {
		println("wrong fixed array literal")
		return
	}
	array_left := [1, 2]
	array_same := [1, 2]
	array_different := [1, 3]
	mut fixed_same := [2]int{}
	fixed_same[0] = 2
	fixed_same[1] = 3
	mut fixed_different := [2]int{}
	fixed_different[0] = 2
	fixed_different[1] = 4
	struct_left := FastArm64Equality{head: 1, tail: 2, values: [3, 4]}
	struct_same := FastArm64Equality{head: 1, tail: 2, values: [3, 4]}
	struct_different := FastArm64Equality{head: 1, tail: 9, values: [3, 4]}
	if array_left != array_same || array_left == array_different || fixed != fixed_same || fixed == fixed_different || struct_left != struct_same || struct_left == struct_different {
		println("wrong aggregate equality")
		return
	}
	struct_items := [struct_same]
	fixed_items := [fixed_same]
	array_items := [array_same]
	if struct_left !in struct_items || struct_different in struct_items || fixed !in fixed_items || fixed_different in fixed_items || array_left !in array_items || array_different in array_items {
		println("wrong aggregate membership")
		return
	}
	mut features := FastArm64Features.read
	features.set(.write)
	if !features.has(.read) || !features.has(.write) {
		println("wrong flag set")
		return
	}
	features.clear(.read)
	if features.has(.read) || !features.has(.write) {
		println("wrong flag clear")
		return
	}
	waiter := FastArm64Waiter{value: 6}
	if waiter.wait() != 7 {
		println("wrong ordinary wait")
		return
	}
	spawned := spawn fast_arm64_spawned_value(10)
	println("before wait")
	if spawned.wait() != 11 {
		println("wrong spawned wait")
		return
	}
	mut shadow := 1
	if true {
		shadow := 2
		if shadow != 2 {
			println("wrong shadow inner")
			return
		}
	}
	if shadow != 1 {
		println("wrong shadow outer")
		return
	}
	mut defer_state := 0
	if false {
		defer {
			defer_state = 99
		}
	}
	if true {
		defer {
			defer_state += 2
		}
		defer {
			defer_state *= 3
		}
		defer_state = 1
	}
	if defer_state != 5 {
		println("wrong scoped defer")
		return
	}
	mut match_state := 0
	match 1 {
		1 {
			defer {
				match_state += 4
			}
		}
		else {
			defer {
				match_state = 99
			}
		}
	}
	if match_state != 4 {
		println("wrong match defer")
		return
	}
	mut break_state := 0
	for break_state == 0 {
		defer {
			break_state += 10
		}
		break
	}
	if break_state != 10 {
		println("wrong break defer")
		return
	}
	mut passes := 0
	mut continue_state := 0
	for passes < 1 {
		passes++
		defer {
			continue_state += 3
		}
		continue
	}
	if continue_state != 3 {
		println("wrong continue defer")
		return
	}
	mut returned := 0
	add_after_return(mut returned)
	if returned != 7 {
		println("wrong return defer")
		return
	}
	mut original := [1, 2]
	mut sliced := original[1..]
	sliced << 3
	if sliced.len != 2 || sliced[0] != 2 || sliced[1] != 3 || original[0] != 1 || original[1] != 2 {
		println("wrong slice growth")
		return
	}
	mut spare_base := [1, 2, 3]
	mut spare_slice := spare_base[1..2]
	spare_slice << 9
	if spare_slice.len != 2 || spare_slice[0] != 2 || spare_slice[1] != 9 || spare_base[0] != 1 || spare_base[1] != 2 || spare_base[2] != 3 {
		println("wrong spare slice append")
		return
	}
	mut delete_base := [1, 2, 3, 4]
	mut delete_slice := delete_base[1..]
	delete_slice.delete(0)
	if delete_slice.len != 2 || delete_slice[0] != 3 || delete_slice[1] != 4 || delete_base[0] != 1 || delete_base[1] != 2 || delete_base[2] != 3 || delete_base[3] != 4 {
		println("wrong sliced delete")
		return
	}
	mut inserted := [1, 3]
	insert_index := 1
	inserted.insert(insert_index, 2)
	if inserted.len != 3 || inserted[0] != 1 || inserted[1] != 2 || inserted[2] != 3 {
		println("wrong insertion")
		return
	}
	mut trimmed := [1, 2]
	trimmed.trim(100)
	if trimmed.len != 2 || trimmed[0] != 1 || trimmed[1] != 2 {
		println("wrong trim")
		return
	}
	mut map_defaults := FastArm64MapDefaults{}
	map_defaults.values["present"] = 7
	if map_defaults.values.len != 1 || map_defaults.values["missing"] != 0 || map_defaults.values["present"] != 7 {
		println("wrong zero map")
		return
	}
	large_map_holders := [1]FastArm64LargeMapHolder{}
	large_missing := large_map_holders[0].values["missing"]
	if large_missing[0] != 0 || large_missing[99] != 0 {
		println("wrong large zero map value")
		return
	}
	cloned_empty := FastArm64CloneOuter{}.inner.values.clone()
	if cloned_empty.len != 0 {
		println("wrong zero map clone")
		return
	}
	empty_keys := FastArm64CloneOuter{}.inner.values.keys()
	empty_values := FastArm64CloneOuter{}.inner.values.values()
	if empty_keys.len != 0 || empty_values.len != 0 {
		println("wrong zero map items")
		return
	}
	mut nested_map := FastArm64CloneOuter{}
	nested_map.inner.values["present"] = 8
	if nested_map.inner.values["present"] != 8 {
		println("wrong nested map insertion")
		return
	}
	zero_outers := [1]FastArm64CloneOuter{}
	mut zero_map_iterations := 0
	for _, _ in zero_outers[0].inner.values {
		zero_map_iterations++
	}
	if zero_map_iterations != 0 {
		println("wrong zero map iteration")
		return
	}
	mut unsigned_range_ran := false
	for _ in u64(0) .. (u64(1) << 63) {
		unsigned_range_ran = true
		break
	}
	if !unsigned_range_ran {
		println("wrong unsigned range")
		return
	}
	mut shifted := u64(1) << 63
	shifted >>= 1
	mut divided := u64(1) << 63
	divided /= 2
	mut remainder := (u64(1) << 63) + 1
	remainder %= 3
	if shifted != u64(1) << 62 || divided != u64(1) << 62 || remainder != 0 {
		println("wrong unsigned compound operation")
		return
	}
	formatted_value := 42
	elapsed := 1.25
	max_unsigned := u64(18446744073709551615)
	long_float := "\${1.0:.200f}"
	if "\${formatted_value:05d}" != "00042" || "\${-7:04d}" != "-007" || "\${formatted_value:x}" != "2a" || "\${formatted_value:04X}" != "002A" || "\${formatted_value:b}" != "101010" || "\${formatted_value:o}" != "52" || "\${max_unsigned:020d}" != "18446744073709551615" || "\${max_unsigned}" != "18446744073709551615" || "\${true}" != "true" || "\${false}" != "false" || "\${u8(65):c}" != "A" || "\${u8(65):3c}" != "  A" || "\${rune(8364):c}" != "€" || "\${elapsed}" != "1.25" || "\${elapsed:.2f}" != "1.25" || "\${elapsed:7.2f}" != "   1.25" || long_float.len != 202 || long_float[0] != `1` || long_float[1] != `.` || long_float[201] != `0` {
		println("wrong interpolation format")
		return
	}
	mut option_handler_ran := false
	option_fallback := maybe_value(false) or {
		option_handler_ran = true
		42
	}
	delayed_option := maybe_value(false)
	option_success := maybe_value(true) or { 99 }
	delayed_option_fallback := delayed_option or { 48 }
	mut error_message_ok := false
	result_fallback := result_value(false) or {
		error_message_ok = err.msg() == "failed"
		43
	}
	delayed_result := coded_error_value()
	result_success := result_value(true) or { 99 }
	mut custom_error_seen := false
	mut custom_error_details_ok := false
	custom_error_fallback := custom_error_value() or {
		if err is FastArm64CustomError && err.msg() == "custom" && err.code() == 42 && err.str() == "custom; code: 42" {
			custom_error_seen = true
		}
		custom_error_details_ok = err.code() == 42 && err.msg() == "custom" && err.str() == "custom; code: 42"
		46
	}
	mut delayed_error_ok := false
	delayed_result_fallback := delayed_result or {
		delayed_error_ok = err.msg() == "coded" && err.code() == 73
		49
	}
	mut coded_error_ok := false
	coded_error_fallback := coded_error_value() or {
		coded_error_ok = err.msg() == "coded" && err.code() == 73 && err.str() == "coded; code: 73"
		47
	}
	propagated_result_fallback := propagated_result(false) or { 44 }
	propagated_result_success := propagated_result(true) or { 99 }
	propagated_option_fallback := propagated_option(false) or { 45 }
	propagated_option_success := propagated_option(true) or { 99 }
	if !option_handler_ran || !error_message_ok || !custom_error_seen || !coded_error_ok
		|| !delayed_error_ok || option_fallback != 42 || option_success != 7
		|| delayed_option_fallback != 48 || result_fallback != 43 || result_success != 9
		|| custom_error_fallback != 46 || delayed_result_fallback != 49
		|| coded_error_fallback != 47 || propagated_result_fallback != 44
		|| propagated_result_success != 10 || propagated_option_fallback != 45
		|| propagated_option_success != 9 {
		println("wrong option result handling")
		return
	}
	mut fd_set := C.fd_set{}
	C.FD_ZERO(&fd_set)
	C.FD_SET(3, &fd_set)
	C.FD_SET(70, &fd_set)
	if C.FD_ISSET(3, &fd_set) == 0 || C.FD_ISSET(70, &fd_set) == 0 || C.FD_ISSET(4, &fd_set) != 0 {
		println("wrong fd set operations")
		return
	}
	C.FD_ZERO(&fd_set)
	if C.FD_ISSET(3, &fd_set) != 0 || C.FD_ISSET(70, &fd_set) != 0 {
		println("wrong fd set zero")
		return
	}
	mut aliased := [1, 2]
	aliased << aliased
	if aliased.len != 4 || aliased[0] != 1 || aliased[1] != 2 || aliased[2] != 1 || aliased[3] != 2 {
		println("wrong aliased append")
		return
	}
	mut alias_base := [1, 2, 3]
	alias_slice := alias_base[1..]
	alias_base << alias_slice
	if alias_base.len != 5 || alias_base[0] != 1 || alias_base[1] != 2 || alias_base[2] != 3 || alias_base[3] != 2 || alias_base[4] != 3 {
		println("wrong sliced alias append")
		return
	}
	defaults := FastArm64Defaults{}
	explicit := FastArm64Defaults{retries: 7}
	if defaults.retries != 3 || !defaults.enabled || defaults.mode != .warm || explicit.retries != 7 || !explicit.enabled || explicit.mode != .warm {
		println("wrong struct defaults")
		return
	}
	mut fixed_map_defaults := [1]FastArm64MapDefaults{}
	mut fixed_map_item := fixed_map_defaults[0]
	fixed_map_item.values["fixed"] = 1
	mut fixed_nested_maps := [1]FastArm64CloneOuter{}
	mut fixed_nested_item := fixed_nested_maps[0]
	fixed_nested_item.inner.values["nested"] = 2
	if fixed_map_defaults[0].values["fixed"] != 1
		|| fixed_nested_maps[0].inner.values["nested"] != 2 {
		println("wrong fixed array map initialization")
		return
	}
	mut indexed := {"b": 2, "a": 1}
	mut keys := indexed.keys()
	keys.sort()
	if keys[0] != "a" || keys[1] != "b" || indexed["a"] != 1 || indexed["b"] != 2 {
		println("wrong map keys copy")
		return
	}
	mut values := indexed.values()
	values[0] = 99
	if indexed["a"] != 1 || indexed["b"] != 2 {
		println("wrong map values copy")
		return
	}
	println("native")
}
'
		os.write_file(source_path, source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'before wait\nspawned\nnative\n'
	}
}

fn test_fastc_arm64_map_storage_and_numeric_conversions() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_map_numeric_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := 'import os

struct WideMapDefaults {
	values map[string][100]int
}

struct FloatValues {
	values []f64
}

struct NumericReceiver {}

struct MutableValue {
mut:
	value int
}

fn scalar_return() f64 {
	return 3
}

fn tuple_return() (f64, u64) {
	return 4, 5
}

fn numeric_argument(value f64) f64 {
	return value
}

fn (receiver NumericReceiver) numeric_argument(value f64) f64 {
	return value
}

fn delayed_value(ok bool) ?int {
	if ok {
		return 7
	}
	return none
}

fn maybe_zero(ok bool) ?int {
	if ok {
		return 0
	}
	return none
}

fn option_worker(expect_success bool) bool {
	for _ in 0 .. 20000 {
		if expect_success {
			value := maybe_zero(true) or { return false }
			if value != 0 {
				return false
			}
		} else {
			value := maybe_zero(false) or { continue }
			if value == 0 {
				return false
			}
		}
	}
	return true
}

fn main() {
	mut wide_map_defaults := [1]WideMapDefaults{}
	wide_map_defaults[0].values["present"] = 9
	wide_missing := wide_map_defaults[0].values["missing"]
	mut reassigned_float := f64(0)
	reassigned_float = 1
	mut numeric_map := map[u64]f64{}
	numeric_map[1] = 2
	numeric_map[u64(2)] = 3
	numeric_map.delete(2)
	typed_values := []f64{1, 2}
	normalized := []int{len: 3, cap: 1}
	left := f64(1.5)
	tuple_float, tuple_unsigned := tuple_return()
	pending := delayed_value(false)
	delayed_fallback := pending or { 42 }
	conditional_absent := if true { maybe_zero(false) } else { maybe_zero(true) }
	conditional_fallback := conditional_absent or { 52 }
	conditional_success := if false { maybe_zero(false) } else { maybe_zero(true) }
	conditional_zero := conditional_success or { 53 }
	float_values := FloatValues{values: [1, 2]}
	mut mutable_values := [MutableValue{value: 1}, MutableValue{value: 2}]
	for mut item in mutable_values {
		item.value += 10
	}
	mut mutable_numbers := [1, 2]
	for mut number in mutable_numbers {
		number++
	}
	absent := maybe_zero(false)
	successful_zero := maybe_zero(true)
	none_comparison_ok := absent == none && !(absent != none)
		&& successful_zero != none && !(successful_zero == none)
	mut nested_values := [][]int{}
	nested_values << [1, 2]
	nested_values << [3]
	mut mutable_map := {"a": 1, "b": 2}
	for _, mut value in mutable_map {
		value += 10
	}
	mut deleting_map := {"a": 1, "b": 2}
	for key, mut value in deleting_map {
		deleting_map.delete(key)
		value = 99
	}
	mut expanding_map := {1: 1, 2: 2}
	for key, mut value in expanding_map {
		for extra in 0 .. 20 {
			expanding_map[key * 100 + extra + 10] = extra
		}
		value = 99
	}
	executed := os.execute("printf arm64-captured")
	failure_a := spawn option_worker(false)
	success_a := spawn option_worker(true)
	failure_b := spawn option_worker(false)
	success_b := spawn option_worker(true)
	thread_options_ok := failure_a.wait() && success_a.wait() && failure_b.wait()
		&& success_b.wait()
	scalar_text := "\${true}:\${u64(9223372036854775808)}"
	if wide_map_defaults[0].values["present"] != 9 || wide_missing[0] != 0 || wide_missing[99] != 0 || reassigned_float != 1.0 || numeric_map[1] != 2.0 || 1 !in numeric_map || 2 in numeric_map || typed_values[0] != 1.0 || typed_values[1] != 2.0 || normalized.len != 3 || normalized.cap < normalized.len || normalized[2] != 0 || left + 1 != 2.5 || scalar_return() != 3.0 || tuple_float != 4.0 || tuple_unsigned != u64(5) || numeric_argument(6) != 6.0 || NumericReceiver{}.numeric_argument(7) != 7.0 || delayed_fallback != 42 || conditional_fallback != 52 || conditional_zero != 0 || float_values.values[0] != 1.0 || float_values.values[1] != 2.0 || mutable_values[0].value != 11 || mutable_values[1].value != 12 || mutable_numbers[0] != 2 || mutable_numbers[1] != 3 || mutable_map["a"] != 11 || mutable_map["b"] != 12 || deleting_map.len != 0 || expanding_map[1] != 1 || expanding_map[2] != 2 || executed.exit_code != 0 || executed.output != "arm64-captured" || !none_comparison_ok || nested_values.len != 2 || nested_values[0].len != 2 || nested_values[0][0] != 1 || nested_values[0][1] != 2 || nested_values[1].len != 1 || nested_values[1][0] != 3 || !thread_options_ok || scalar_text != "true:9223372036854775808" {
		println("wrong")
		return
	}
	println("native")
}
'
		os.write_file(source_path, source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n'
		for field in ['len', 'cap'] {
			invalid_source_path := os.join_path_single(test_dir, 'invalid_${field}.v')
			invalid_output_path := os.join_path_single(test_dir, 'invalid_${field}')
			invalid_source := 'fn negative() int { return -1 }\nfn main() { _ := []int{${field}: negative()} }\n'
			os.write_file(invalid_source_path, invalid_source) or { panic(err) }
			generate_arm64_files([invalid_source_path], prefs, invalid_output_path) or { panic(err) }
			invalid_result := os.execute(invalid_output_path)
			assert invalid_result.exit_code != 0
		}
		exec_source_path := os.join_path_single(test_dir, 'unsupported_exec.v')
		exec_output_path := os.join_path_single(test_dir, 'unsupported_exec')
		os.write_file(exec_source_path, 'import os\n\nfn main() { _ := os.exec(["true"]) }\n') or {
			panic(err)
		}
		mut exec_rejected := false
		generate_arm64_files([exec_source_path], prefs, exec_output_path) or {
			exec_rejected = true
			assert err.msg().contains('does not support `os.exec` on the direct ARM64 backend')
		}
		assert exec_rejected
	}
}

fn test_fastc_arm64_spawn_for_general_programs() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_spawn_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn worker() int { return 1 }\nfn main() { handle := spawn worker(); if handle.wait() == 1 { println("spawned") } }\n') or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'spawned\n'
	}
}

fn test_fastc_arm64_array_index_bounds() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_bounds_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		mut sources := []string{}
		for index in [-1, 2] {
			sources << 'fn main() {\n\tvalues := [1, 2]\n\tindex := ${index}\n\tselected := values[index]\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
			sources << 'fn main() {\n\tmut values := [1, 2]\n\tindex := ${index}\n\tvalues.delete(index)\n}\n'
			sources << 'fn main() {\n\ttext := "hi"\n\tindex := ${index}\n\tselected := text[index]\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
			sources << 'struct FixedHolder {\n\tvalues [2]int\n}\n\nfn main() {\n\tholder := FixedHolder{}\n\tindex := ${index}\n\tselected := holder.values[index]\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		}
		sources << 'fn main() {\n\tvalues := [1, 2]\n\tstart := -1\n\tsliced := values[start..]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tvalues := [1, 2]\n\tstart := 3\n\tsliced := values[start..]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tvalues := [1, 2]\n\tend := 5\n\tsliced := values[1..end]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tvalues := [1, 2]\n\tstart := 2\n\tend := 1\n\tsliced := values[start..end]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\ttext := "hi"\n\tstart := -1\n\tsliced := text[start..]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\ttext := "hi"\n\tend := 100\n\tsliced := text[1..end]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\ttext := "hi"\n\tstart := 2\n\tend := 1\n\tsliced := text[start..end]\n\tif sliced.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tvalues := []int{}\n\tselected := values.last()\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tmut values := []int{}\n\tselected := values.pop()\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tmut values := []int{}\n\tvalues.delete_last()\n\tprintln("unused")\n}\n'
		sources << 'fn main() {\n\tlength := -1\n\tvalues := []int{len: length}\n\tif values.len == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tcapacity := -1\n\tvalues := []int{cap: capacity}\n\tif values.cap == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		for index, source in sources {
			source_path := os.join_path_single(test_dir, 'bounds_${index}.v')
			output_path := os.join_path_single(test_dir, 'bounds_${index}')
			os.write_file(source_path, source) or {
				panic(err)
			}
			generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
			result := os.execute(output_path)
			assert result.exit_code != 0
		}
	}
}

fn test_fastc_arm64_source_location_pseudo_values() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_location_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := 'struct LocationOwner {}

fn (receiver LocationOwner) instance_location() {
	println(@FILE_LINE)
	println(@LOCATION)
}

fn LocationOwner.static_location() {
	println(@FILE_LINE)
	println(@LOCATION)
}

fn main() {
	receiver := LocationOwner{}
	receiver.instance_location()
	LocationOwner.static_location()
}
'
		os.write_file(source_path, source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		resolved_source_path := os.real_path(source_path)
		expected := 'main.v:4\n${resolved_source_path}:5, main.LocationOwner{}.instance_location\nmain.v:9\n${resolved_source_path}:10, main.LocationOwner.static_location (static)\n'
		assert result.output == expected, 'expected `${expected}`, got `${result.output}`'
	}
}

fn test_fastc_arm64_remaining_pseudo_values() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_pseudo_${os.getpid()}')
		git_dir := os.join_path_single(test_dir, '.git')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(git_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		manifest := "Module { name: 'arm64_pseudo' }\n"
		full_hash := '0123456789abcdef0123456789abcdef01234567'
		os.write_file(os.join_path_single(test_dir, 'v.mod'), manifest) or { panic(err) }
		os.write_file(os.join_path_single(git_dir, 'HEAD'), full_hash + '\n') or { panic(err) }
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	println(@VHASH)
	println(@VCURRENTHASH)
	println(@BUILD_DATE)
	println(@BUILD_TIME)
	println(@BUILD_TIMESTAMP)
	println(@CCOMPILER)
	println(@VMODROOT)
	println(@VMOD_FILE)
	println(@VMODHASH)
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.ccompiler = 'clang'
		prefs.vhash = 'arm64-vhash'
		prefs.vcurrent_hash = 'arm64-vcurrent-hash'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		expected := '${prefs.vhash}\n${prefs.vcurrent_hash}\n${prefs.build_date}\n${prefs.build_time}\n${prefs.build_timestamp}\n${prefs.ccompiler}\n${os.real_path(test_dir)}\n${manifest}\n${full_hash[..7]}\n'
		assert result.output == expected, 'expected `${expected}`, got `${result.output}`'

		unknown_path := os.join_path_single(test_dir, 'unknown.v')
		os.write_file(unknown_path, 'fn main() { println(@UNKNOWN_PSEUDO) }\n') or {
			panic(err)
		}
		mut rejected := false
		generate_arm64_files([unknown_path], prefs, output_path) or {
			rejected = true
			assert err.msg().contains('compile-time pseudo value `@UNKNOWN_PSEUDO`')
		}
		assert rejected
	}
}

fn test_fastc_arm64_module_lifecycle_hooks() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_lifecycle_${os.getpid()}')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(dependency_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		dependency_path := os.join_path_single(dependency_dir, 'dependency.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'module main

import dependency

fn init() {
	println("main init")
}

fn cleanup() {
	println("main cleanup")
}

fn main() {
	println("main")
}
') or { panic(err) }
		os.write_file(dependency_path, 'module dependency

fn init() {
	println("dependency init")
}

fn cleanup() {
	println("dependency cleanup")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'dependency init\nmain init\nmain\nmain cleanup\ndependency cleanup\n'
	}
}

fn test_fastc_arm64_rejects_imported_source_output() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_output_alias_${os.getpid()}')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(dependency_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		dependency_path := os.join_path_single(dependency_dir, 'dependency.v')
		os.write_file(source_path, 'module main\n\nimport dependency\n\nfn main() {\n\tdependency.answer()\n}\n') or {
			panic(err)
		}
		dependency_source := 'module dependency\n\npub fn answer() int {\n\treturn 42\n}\n'
		os.write_file(dependency_path, dependency_source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		mut rejected := false
		generate_arm64_files([source_path], prefs, dependency_path) or {
			rejected = true
			assert err.msg().contains('aliases source')
		}
		assert rejected
		assert os.read_file(dependency_path) or { '' } == dependency_source
	}
}
