module fastc

import os
import v3.pref

fn test_fastc_parser_emits_arm64_without_c() {
	$if arm64 ? {
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
	$if arm64 ? {
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
	large_float := 1234567012.3
	if "\${large_float:6.1e}" != "1.2e+09" || "\${large_float:6.1E}" != "1.2E+09" || "\${1.25:.1F}" != "1.3" || "\${123.4:.3g}" != "123" || "\${0.00001234:.3G}" != "1.23E-05" || "\${1.25:.2e}" != "1.25e+00" {
		println("wrong scientific interpolation format")
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
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_map_numeric_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.mkdir_all(dependency_dir) or { panic(err) }
		os.write_file(os.join_path_single(dependency_dir, 'dependency.v'), 'module dependency\n\npub const base = 40\npub const answer = base + 2\n') or {
			panic(err)
		}
		source := 'import os
import dependency

const base = 1

struct WideMapDefaults {
	values map[string][100]int
}

struct FloatValues {
	values []f64
}

struct MapValues {
	values map[u64]f64
}

struct DefaultFloatValues {
	values []f64 = [1, 2]
}

struct Config {
	retries int = 3
}

struct LengthDefaults {
	config Config
	values []int = [4, 5]
	lookup map[string]int = {"answer": 42}
}

@[params]
struct FloatParams {
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

fn sum_contextual_array(values []f64) f64 {
	return values[0] + values[1]
}

fn read_contextual_map(values map[string]f64) f64 {
	return values["value"]
}

fn read_float_params(params FloatParams) f64 {
	return params.values[0] + params.values[1]
}

fn inferred_float_values() []f64 {
	return [1, 2]
}

fn inferred_float_map() map[string]f64 {
	return {"value": 3}
}

fn (receiver NumericReceiver) numeric_argument(value f64) f64 {
	return value
}

fn (receiver NumericReceiver) sum(values []f64) f64 {
	return values[0] + values[1]
}

fn (receiver NumericReceiver) take(values ...int) int {
	return values[0] + values[1]
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

fn maybe_floats(ok bool) ?[]f64 {
	if ok {
		return [9.0]
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

fn writeback_before_return(mut values map[string][]int) {
	for _, mut value in values {
		value[0] = 7
		return
	}
}

fn writeback_before_break(mut values map[string][]int) {
	for _, mut value in values {
		value[0] = 8
		break
	}
}

fn writeback_before_propagation(mut values map[string][]int) ?bool {
	for _, mut value in values {
		value[0] = 9
		_ := maybe_zero(false)?
	}
	return true
}

fn slice_from_parameter(values []int) []int {
	return values[..]
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
	typed_nested_values := [][]f64{[1, 2]}
	inferred_nested_values := [[]f64{1.0}, [2, 3]]
	inferred_nested_map := {"first": []f64{1.0}, "second": [2, 3]}
	mut assigned_float_values := []f64{}
	assigned_float_values = [1, 2]
	mut indexed_float_values := [][]f64{len: 1}
	indexed_float_values[0] = [1, 2]
	mut indexed_map_float_values := map[string][]f64{}
	indexed_map_float_values["value"] = [1, 2]
	mut field_float_values := FloatValues{}
	field_float_values.values = [1, 2]
	mut inserted_float_values := [][]f64{}
	inserted_float_values.insert(0, [1, 2])
	mut prepended_float_values := [][]f64{}
	prepended_float_values.prepend([1, 2])
	mut nested_appended_float_values := [][]f64{len: 1, init: [9.0]}
	nested_appended_float_values[0] << [1, 2]
	typed_map_float_values := map[string][]f64{"value": [1, 2]}
	initialized_float_values := [][]f64{len: 2, init: [1, 2]}
	default_float_values := DefaultFloatValues{}
	params_float_value := read_float_params(values: [1, 2])
	missing_config := map[string]Config{}["missing"]
	length_defaults := []LengthDefaults{len: 2}
	normalized := []int{len: 3, cap: 1}
	left := f64(1.5)
	contextual_array := sum_contextual_array([1, 2])
	contextual_map := read_contextual_map({"value": 3})
	returned_values := inferred_float_values()
	returned_float_map := inferred_float_map()
	mut appended_arrays := [][]f64{}
	appended_arrays << [1, 2]
	mut appended_maps := []map[string]f64{}
	appended_maps << {"value": 3}
	tuple_float, tuple_unsigned := tuple_return()
	pending := delayed_value(false)
	delayed_fallback := pending or { 42 }
	conditional_absent := if true { maybe_zero(false) } else { maybe_zero(true) }
	conditional_fallback := conditional_absent or { 52 }
	conditional_success := if false { maybe_zero(false) } else { maybe_zero(true) }
	conditional_zero := conditional_success or { 53 }
	matched_absent := match true {
		true { maybe_zero(false) }
		else { maybe_zero(true) }
	}
	matched_fallback := matched_absent or { 62 }
	matched_success := match false {
		true { maybe_zero(false) }
		else { maybe_zero(true) }
	}
	matched_zero := matched_success or { 63 }
	aggregate_fallback := maybe_floats(false) or { [1, 2] }
	conditional_floats := if false { []f64{1.0} } else { [2, 3] }
	matched_floats := match false {
		true { []f64{1.0} }
		else { [2, 3] }
	}
	case_values := []f64{1.0, 2.0}
	mut statement_case_matched := false
	match case_values {
		[1, 2] { statement_case_matched = true }
		else {}
	}
	expression_case_matched := match case_values {
		[1, 2] { true }
		else { false }
	}
	float_values := FloatValues{values: [1, 2]}
	map_values := MapValues{values: {1: 2}}
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
	mut slice_base := [1, 2, 3]
	slice_view := slice_base[..]
	slice_base.delete(0)
	mut growing_base := []int{cap: 1}
	growing_base << 4
	growing_view := growing_base[..]
	growing_base << 5
	mut parameter_base := [6, 7, 8]
	parameter_view := slice_from_parameter(parameter_base)
	parameter_base.delete(0)
	mut parameter_growing_base := []int{cap: 1}
	parameter_growing_base << 9
	parameter_growing_view := slice_from_parameter(parameter_growing_base)
	parameter_growing_base << 10
	mut tail_base := [11, 12, 13]
	tail_view := tail_base[..]
	tail_base.delete_last()
	tail_base << 14
	mut pop_base := [15, 16, 17]
	pop_view := pop_base[..]
	popped := pop_base.pop()
	pop_base << 18
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
	mut returned_map := map[string][]int{"value": [1]}
	writeback_before_return(mut returned_map)
	mut broken_map := map[string][]int{"value": [1]}
	writeback_before_break(mut broken_map)
	mut propagated_map := map[string][]int{"value": [1]}
	_ := writeback_before_propagation(mut propagated_map) or { false }
	executed := os.execute("printf arm64-captured; printf arm64-error >&2")
	terminated := os.execute("kill -TERM $$")
	failure_a := spawn option_worker(false)
	success_a := spawn option_worker(true)
	failure_b := spawn option_worker(false)
	success_b := spawn option_worker(true)
	thread_options_ok := failure_a.wait() && success_a.wait() && failure_b.wait()
		&& success_b.wait()
	scalar_text := "\${true}:\${u64(9223372036854775808)}"
	if aggregate_fallback != [1.0, 2.0] || conditional_floats != [2.0, 3.0]
		|| matched_floats != [2.0, 3.0] || !statement_case_matched || !expression_case_matched
		|| propagated_map["value"][0] != 9 || slice_base != [2, 3] || slice_view != [1, 2, 3]
		|| growing_base != [4, 5] || growing_view != [4] || parameter_base != [7, 8]
		|| parameter_view != [6, 7, 8] || parameter_growing_base != [9, 10]
		|| parameter_growing_view != [9] || tail_base != [11, 12, 14]
		|| tail_view != [11, 12, 13] || pop_base != [15, 16, 18]
		|| pop_view != [15, 16, 17] || popped != 17 || dependency.answer != 42 || base != 1 {
		println("wrong aggregate context or propagation cleanup")
		return
	}
	if wide_map_defaults[0].values["present"] != 9 || wide_missing[0] != 0 || wide_missing[99] != 0 || reassigned_float != 1.0 || numeric_map[1] != 2.0 || 1 !in numeric_map || 2 in numeric_map || typed_values[0] != 1.0 || typed_values[1] != 2.0 || typed_nested_values[0][0] != 1.0 || typed_nested_values[0][1] != 2.0 || inferred_nested_values[1] != [2.0, 3.0] || inferred_nested_map["second"] != [2.0, 3.0] || assigned_float_values[0] != 1.0 || assigned_float_values[1] != 2.0 || indexed_float_values[0][0] != 1.0 || indexed_float_values[0][1] != 2.0 || indexed_map_float_values["value"][0] != 1.0 || indexed_map_float_values["value"][1] != 2.0 || field_float_values.values[0] != 1.0 || field_float_values.values[1] != 2.0 || inserted_float_values[0][0] != 1.0 || inserted_float_values[0][1] != 2.0 || prepended_float_values[0][0] != 1.0 || prepended_float_values[0][1] != 2.0 || nested_appended_float_values[0].len != 3 || nested_appended_float_values[0][0] != 9.0 || nested_appended_float_values[0][1] != 1.0 || nested_appended_float_values[0][2] != 2.0 || typed_map_float_values["value"][0] != 1.0 || typed_map_float_values["value"][1] != 2.0 || initialized_float_values[0][0] != 1.0 || initialized_float_values[0][1] != 2.0 || initialized_float_values[1][0] != 1.0 || initialized_float_values[1][1] != 2.0 || default_float_values.values[0] != 1.0 || default_float_values.values[1] != 2.0 || params_float_value != 3.0 || missing_config.retries != 3 || length_defaults[0].config.retries != 3 || length_defaults[1].values != [4, 5] || length_defaults[0].lookup["answer"] != 42 || normalized.len != 3 || normalized.cap < normalized.len || normalized[2] != 0 || left + 1 != 2.5 || contextual_array != 3.0 || contextual_map != 3.0 || returned_values[0] != 1.0 || returned_values[1] != 2.0 || returned_float_map["value"] != 3.0 || appended_arrays[0][0] != 1.0 || appended_arrays[0][1] != 2.0 || appended_maps[0]["value"] != 3.0 || scalar_return() != 3.0 || tuple_float != 4.0 || tuple_unsigned != u64(5) || numeric_argument(6) != 6.0 || NumericReceiver{}.numeric_argument(7) != 7.0 || NumericReceiver{}.sum([1, 2]) != 3.0 || NumericReceiver{}.take(1, 2) != 3 || delayed_fallback != 42 || conditional_fallback != 52 || conditional_zero != 0 || matched_fallback != 62 || matched_zero != 0 || float_values.values[0] != 1.0 || float_values.values[1] != 2.0 || map_values.values[1] != 2.0 || mutable_values[0].value != 11 || mutable_values[1].value != 12 || mutable_numbers[0] != 2 || mutable_numbers[1] != 3 || mutable_map["a"] != 11 || mutable_map["b"] != 12 || deleting_map.len != 0 || expanding_map[1] != 1 || expanding_map[2] != 2 || returned_map["value"][0] != 7 || broken_map["value"][0] != 8 || executed.exit_code != 0 || executed.output != "arm64-capturedarm64-error" || terminated.exit_code != 15 || !none_comparison_ok || nested_values.len != 2 || nested_values[0].len != 2 || nested_values[0][0] != 1 || nested_values[0][1] != 2 || nested_values[1].len != 1 || nested_values[1][0] != 3 || !thread_options_ok || scalar_text != "true:9223372036854775808" {
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

fn test_fastc_arm64_c_variadic_default_promotions() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_c_variadic_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
fn C.printf(format &char, ...)

fn main() {
	format := "%.1f:%d\\n"
	C.printf(format.str, f32(1.5), u8(7))
}
'
		os.write_file(source_path, source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == '1.5:7\n'
	}
}

fn test_fastc_arm64_spawn_for_general_programs() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_spawn_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn worker() int { mut stack := [1048576]u8{}; stack[0] = 7; stack[1048575] = 9; return int(stack[0]) + int(stack[1048575]) }\nfn main() { handle := spawn worker(); if handle.wait() == 16 { println("spawned") } }\n') or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'spawned\n'
		double_wait_source := os.join_path_single(test_dir, 'double_wait.v')
		double_wait_output := os.join_path_single(test_dir, 'double_wait')
		os.write_file(double_wait_source, 'fn worker() int { return 1 }\nfn main() { handle := spawn worker(); _ := handle.wait(); _ := handle.wait() }\n') or {
			panic(err)
		}
		generate_arm64_files([double_wait_source], prefs, double_wait_output) or { panic(err) }
		double_wait_result := os.execute(double_wait_output)
		assert double_wait_result.exit_code != 0
		// `sizeof` must not leave a cached, body-less spawn wrapper behind: a real
		// `spawn worker()` after `sizeof(spawn worker())` has to run the worker body.
		sizeof_spawn_source := os.join_path_single(test_dir, 'sizeof_spawn.v')
		sizeof_spawn_output := os.join_path_single(test_dir, 'sizeof_spawn')
		os.write_file(sizeof_spawn_source, 'fn worker() int { return 42 }\nfn main() { size := sizeof(spawn worker()); handle := spawn worker(); if size == sizeof(u64) && handle.wait() == 42 { println("spawned") } else { println("wrong") } }\n') or {
			panic(err)
		}
		generate_arm64_files([sizeof_spawn_source], prefs, sizeof_spawn_output) or { panic(err) }
		sizeof_spawn_result := os.execute(sizeof_spawn_output)
		assert sizeof_spawn_result.exit_code == 0
		assert sizeof_spawn_result.output == 'spawned\n', sizeof_spawn_result.output
	}
}

fn test_fastc_arm64_binds_c_externs_beyond_the_linker_allowlist() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_cextern_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		// `arc4random` / `arc4random_uniform` are valid libSystem symbols that are not in
		// the linker's force_external_syms allowlist. They must still be bound through the
		// GOT/stubs instead of aborting the link with an unresolved symbol.
		os.write_file(source_path, 'fn C.arc4random() u32\nfn C.arc4random_uniform(upper_bound u32) u32\nfn main() {\n\ttouch := C.arc4random()\n\tbounded := C.arc4random_uniform(u32(1))\n\tif touch == touch && bounded == u32(0) {\n\t\tprintln("bound")\n\t} else {\n\t\tprintln("wrong")\n\t}\n}\n') or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		assert os.is_executable(output_path)
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'bound\n', result.output
	}
}

fn test_fastc_arm64_array_index_bounds() {
	$if arm64 ? {
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

fn test_fastc_arm64_sizeof_is_unevaluated_and_array_shrinks_detach() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_sizeof_shrink_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
fn side_effect() int {
	println("wrong")
	return 1
}

fn main() {
	mut clear_base := [1, 2]
	clear_view := clear_base[..]
	clear_base.clear()
	clear_cap := clear_base.cap
	clear_base << 9
	mut trim_base := [3, 4, 5]
	trim_view := trim_base[..]
	trim_base.trim(1)
	trim_base << 8
	mut bounded_base := []int{len: 3, cap: 10}
	bounded_slice := bounded_base[0..1]
	mut delete_base := []int{len: 3, cap: 10}
	delete_base[0] = 1
	delete_base[1] = 2
	delete_base[2] = 3
	delete_view := delete_base[..]
	delete_base.delete(1)
	mut pop_base := []int{len: 3, cap: 10}
	pop_base[0] = 4
	pop_base[1] = 5
	pop_base[2] = 6
	pop_view := pop_base[..]
	popped := pop_base.pop()
	mut tail_base := []int{len: 3, cap: 10}
	tail_base[0] = 7
	tail_base[1] = 8
	tail_base[2] = 9
	tail_view := tail_base[..]
	tail_base.delete_last()
	mut compact_trim_base := []int{len: 3, cap: 10}
	compact_trim_base[0] = 10
	compact_trim_base[1] = 11
	compact_trim_base[2] = 12
	compact_trim_view := compact_trim_base[..]
	compact_trim_base.trim(2)
	empty := []int{}
	element_size := sizeof(empty[0])
	call_size := sizeof(side_effect())
	if clear_cap != 0 || clear_base != [9] || clear_view != [1, 2] || trim_base != [3, 8]
		|| trim_view != [3, 4, 5] || bounded_slice.cap != 1 || delete_base != [1, 3]
		|| delete_base.cap != 2 || delete_view != [1, 2, 3] || popped != 6
		|| pop_base != [4, 5] || pop_base.cap != 2 || pop_view != [4, 5, 6]
		|| tail_base != [7, 8] || tail_base.cap != 2 || tail_view != [7, 8, 9]
		|| compact_trim_base != [10, 11] || compact_trim_base.cap != 2
		|| compact_trim_view != [10, 11, 12] || element_size != sizeof(int)
		|| call_size != sizeof(int) {
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
	}
}

fn test_fastc_arm64_array_aggregate_defaults_and_nested_clones_are_independent() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_array_clone_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
struct Config {
mut:
	values []int = [4, 5]
	lookup map[string]int = {"x": 1}
}

struct Wrapper {
mut:
	config Config
}

fn main() {
	mut rows := [][]int{len: 2, init: [1, 2]}
	rows[0][0] = 9
	mut maps := []map[string]int{len: 2, init: {"x": 1}}
	maps[0]["x"] = 9
	mut defaults := []Config{len: 2}
	defaults[0].values[0] = 9
	defaults[0].lookup["x"] = 9
	mut initialized := []Config{len: 2, init: Config{values: [6, 7], lookup: {"x": 2}}}
	initialized[0].values[0] = 9
	initialized[0].lookup["x"] = 9
	mut wrapped := []Wrapper{len: 2, init: Wrapper{config: Config{values: [8], lookup: {"x": 3}}}}
	wrapped[0].config.values[0] = 9
	wrapped[0].config.lookup["x"] = 9
	original := [[1, 2], [3]]
	mut copied := original.clone()
	copied[0][0] = 9
	cube := [[[1]]]
	mut cube_copy := cube.clone()
	cube_copy[0][0][0] = 9
	if rows[1][0] != 1 || maps[1]["x"] != 1 || defaults[1].values[0] != 4
		|| defaults[1].lookup["x"] != 1 || initialized[1].values[0] != 6
		|| initialized[1].lookup["x"] != 2 || wrapped[1].config.values[0] != 8
		|| wrapped[1].config.lookup["x"] != 3 || original[0][0] != 1
		|| copied[0][0] != 9 || cube[0][0][0] != 1 || cube_copy[0][0][0] != 9 {
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
	}
}

fn test_fastc_arm64_cleared_array_growth_and_pointer_index_metadata() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_array_metadata_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
fn main() {
	mut base := [1, 2]
	view1 := base[..]
	base.clear()
	base << 3
	view2 := base[..]
	base.insert(0, 4)
	values := []f64{1, 2}
	ptr := &values
	first := ptr[0][0]
	if view1 != [1, 2] || view2 != [3] || base != [4, 3] || first != 1.0 {
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
	}
}

fn test_fastc_arm64_array_many_insert_and_slice_byte_offsets() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_insert_slice_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
fn main() {
	mut values := [1, 4]
	values.insert(1, [2, 3])
	mut base := [u64(1), 2, 3]
	view := base[1..]
	nested := view[1..]
	base.insert(0, u64(0))
	if values != [1, 2, 3, 4] || view != [u64(2), 3] || nested != [u64(3)]
		|| view.offset != int(sizeof(u64)) || nested.offset != 2 * int(sizeof(u64)) {
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
	}
}

fn test_fastc_arm64_struct_layout_attributes() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_struct_layout_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
@[packed]
struct Header {
	tag   u8
	value u64
}

struct PackedOuter {
	prefix u8
	header Header
}

@[aligned: 32]
struct Aligned {
	tag   u8
	value u64
}

struct AlignedOuter {
	prefix u8
	value  Aligned
}

struct BytePair {
	left  u8
	right u8
}

struct NestedBytePair {
	head u8
	pair BytePair
	tail u8
}

fn main() {
	if sizeof(Header) != 9 {
		println("wrong header")
		return
	}
	if sizeof(PackedOuter) != 10 {
		println("wrong packed outer")
		return
	}
	if sizeof(Aligned) != 32 {
		println("wrong aligned")
		return
	}
	if sizeof(AlignedOuter) != 64 {
		println("wrong aligned outer")
		return
	}
	if sizeof(NestedBytePair) != 4 {
		println("wrong nested byte pair")
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
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_overaligned_local_stack_storage() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_overaligned_local_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, '@[aligned: 64]
struct CacheLine {
	value u64
}

fn local_is_aligned() bool {
	value := CacheLine{value: 7}
	return usize(&value) % 64 == 0
}

fn main() {
	if !local_is_aligned() {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_zero_capacity_array_data_is_nil() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_empty_array_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	mut empty := []int{}
	mut cloned := empty.clone()
	mut constructed := []int{len: 0, cap: 0}
	if u64(empty.data) != 0 || u64(cloned.data) != 0 || u64(constructed.data) != 0 {
		println("wrong")
		return
	}
	empty << 1
	cloned << 2
	constructed << 3
	if empty != [1] || cloned != [2] || constructed != [3] {
		println("wrong append")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_execute_output_is_nul_terminated() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_execute_terminated_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'import os

fn main() {
	result := os.execute("printf captured")
	unsafe {
		if result.output.str[result.output.len] != 0 {
			println("wrong")
			return
		}
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_empty_multi_append_preserves_slice_storage() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_empty_append_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	mut base := [1, 2, 3]
	mut view := base[1..]
	view << []int{}
	view[0] = 9
	if base[1] != 9 || view.len != 2 {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_trim_noop_preserves_slice_storage() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_trim_noop_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	mut base := [1, 2, 3]
	mut view := base[..]
	view.trim(100)
	view[0] = 9
	if base[0] != 9 || view.len != 3 {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_nonpositive_raw_push_many_is_noop() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_push_many_noop_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	mut values := [1, 2]
	unsafe { values.push_many(&values[0], -1) }
	if values != [1, 2] {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_string_slice_owns_terminated_storage() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_string_slice_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn C.strlen(value &char) usize
fn C.free(value voidptr)

fn main() {
	text := "abcd"
	part := text[1..3]
	if part != "bc" || C.strlen(part.str) != 2 {
		println("wrong")
		return
	}
	unsafe { C.free(part.str) }
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_replays_imported_initializers_in_declaring_module() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_imported_initializers_${os.getpid()}')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(dependency_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		dependency_path := os.join_path_single(dependency_dir, 'dependency.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(dependency_path, 'module dependency

pub const base = 40

pub struct Config {
pub:
	value int = base
}

pub enum Answer {
	value = base + 2
}
') or { panic(err) }
		os.write_file(source_path, 'module main

import dependency

const base = 1

fn main() {
	config := dependency.Config{}
	if config.value != 40 || int(dependency.Answer.value) != 42 || base != 1 {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_typed_pointer_arithmetic_scales_offsets() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_pointer_arithmetic_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn main() {
	items := [u64(11), 22]
	unsafe {
		next := (&items[0]) + 1
		if *next != 22 {
			println("wrong")
			return
		}
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_ownership_returning_collection_methods_deep_clone() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_collection_ownership_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'struct Holder {
mut:
	values []int
}

fn main() {
	original_map := {"a": [1, 2]}
	mut copied_map := original_map.clone()
	copied_map["a"][0] = 9
	original_nested_map := {"a": {"x": 1}}
	mut copied_nested_map := original_nested_map.clone()
	copied_nested_map["a"]["x"] = 6
	original_holders := {"a": Holder{values: [1, 2]}}
	mut copied_holders := original_holders.clone()
	copied_holders["a"].values[0] = 5
	original_array := [[1, 2], [3, 4]]
	mut reversed := original_array.reverse()
	reversed[0][0] = 8
	holders := [Holder{values: [1]}, Holder{values: [2]}]
	mut reversed_holders := holders.reverse()
	reversed_holders[0].values[0] = 4
	mut extracted := original_map.values()
	extracted[0][0] = 7
	mut extracted_holders := original_holders.values()
	extracted_holders[0].values[0] = 3
	if original_map["a"][0] != 1 || copied_map["a"][0] != 9
		|| original_nested_map["a"]["x"] != 1 || copied_nested_map["a"]["x"] != 6
		|| original_holders["a"].values[0] != 1 || copied_holders["a"].values[0] != 5
		|| original_array[1][0] != 3 || reversed[0][0] != 8 || holders[1].values[0] != 2
		|| reversed_holders[0].values[0] != 4 || extracted[0][0] != 7
		|| extracted_holders[0].values[0] != 3 {
		println("wrong")
		return
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_path_overrides_match_os_normalization() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_paths_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
import os

fn main() {
	joined := os.join_path_single("/tmp/", "file")
	dotted := os.join_path_single("/tmp//a", "./b")
	parent := os.join_path_single("/tmp/a", "../b")
	absolute := os.abs_path("/tmp/a/../b")
	canonical := os.abs_path("/tmp//a/./")
	if joined != "/tmp/file" || dotted != "/tmp/a/b" || parent != "/tmp/a/../b"
		|| absolute != "/tmp/b" || canonical != "/tmp/a" {
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
	}
}

fn test_fastc_arm64_join_path_trim_keeps_owned_base_pointer() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_join_owned_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'import os

fn C.free(value voidptr)

fn main() {
	path := os.join_path_single("", "file")
	if path != "file" {
		println("wrong")
		return
	}
	unsafe { C.free(path.str) }
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_integer_strings_keep_owned_base_pointers() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_integer_owned_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn C.free(value voidptr)

fn main() {
	plain := "\${i64(-123456789)}"
	formatted := "\${u64(0xffff):08X}"
	if plain != "-123456789" || formatted != "0000FFFF" {
		println("wrong")
		return
	}
	unsafe {
		C.free(plain.str)
		C.free(formatted.str)
	}
	println("native")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n', result.output
	}
}

fn test_fastc_arm64_map_rehash_frees_superseded_buckets() {
	$if arm64 ? {
		mut prefs := pref.new_preferences()
		mut program := FastArm64Program.new(prefs, map[string]bool{}, map[string]FastcFunctionSignature{}, map[string]FastArm64TypeDecl{}, map[string]FastArm64ConstantDecl{}, map[string]FastArm64ConstantDecl{})
		program.register_functions()
		rehash_id := program.fn_ids['fast_map_rehash']
		mut free_calls := 0
		for block_id in program.m.funcs[rehash_id].blocks {
			for value_id in program.m.blocks[block_id].instrs {
				value := program.m.values[value_id]
				if value.kind != .instruction {
					continue
				}
				instruction := program.m.instrs[value.index]
				if instruction.op != .call || instruction.operands.len == 0 {
					continue
				}
				callee := program.m.values[instruction.operands[0]]
				if callee.kind == .func_ref && callee.name == 'free' {
					free_calls++
				}
			}
		}
		assert free_calls == 1
	}
}

fn test_fastc_arm64_rejects_fixed_arity_call_mismatches() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_call_arity_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		for invocation in ['sum(1)', 'sum(1, 2, 3)'] {
			os.write_file(source_path, 'fn sum(a int, b int) int {\n\treturn a + b\n}\n\nfn main() {\n\tprintln(${invocation})\n}\n') or { panic(err) }
			mut prefs := pref.new_preferences()
			prefs.backend = 'fastc'
			prefs.user_defines = ['arm64']
			generate_arm64_files([source_path], prefs, output_path) or {
				assert err.msg().contains('function `sum` call with')
				continue
			}
			assert false, '`${invocation}` should fail argument-count validation'
		}
		os.write_file(source_path, '@[params]
struct Options {
	bonus int
}

fn total(base int, rest ...int) int {
	return base + rest.len
}

fn with_options(base int, options Options) int {
	return base + options.bonus
}

fn C.printf(format &char, ...)

fn main() {
	_ = total(1, 2, 3)
	_ = with_options(1)
	_ = with_options(1, bonus: 2)
	format := "%d"
	C.printf(format.str, 1)
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
	}
}

fn test_fastc_arm64_map_move_transfers_state() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_map_move_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, "fn main() {\n\tmut original := {'answer': 42}\n\tmoved := original.move()\n\tprintln('\${original.len}:\${moved['answer']}')\n}\n") or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == '0:42\n', result.output
	}
}

fn test_fastc_arm64_source_location_pseudo_values() {
	$if arm64 ? {
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
	$if arm64 ? {
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

fn test_fastc_arm64_c_style_for_assigning_existing_local() {
	$if arm64 ? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_c_for_assign_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'fn count_down(n int) int {
	mut i := 0
	mut total := 0
	for i = n - 1; i >= 0; i-- {
		total += i
	}
	return total + i
}

fn main() {
	println(count_down(5))
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == '9\n'
	}
}

fn test_fastc_arm64_module_lifecycle_hooks() {
	$if arm64 ? {
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
	$if arm64 ? {
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
