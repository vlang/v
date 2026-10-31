module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_function_fields_take_precedence_over_method_names() {
	$if macos && arm64 {
		source := r'module main
import callbacks
fn C.exit(int)
type Callback = fn (voidptr) voidptr
struct Task { run fn (voidptr) voidptr arg voidptr }
struct AliasedTask { run Callback arg voidptr }
struct Holder { task Task }
struct Process {}
fn (p Process) run(arg voidptr) voidptr { C.exit(90) return arg }
fn callback(arg voidptr) voidptr {
    value := &int(arg)
    unsafe { *value += 7 }
    return arg
}
fn execute(task Task) voidptr { return task.run(task.arg) }
fn execute_pointer(task &Task) voidptr { return task.run(task.arg) }
fn execute_alias(task AliasedTask) voidptr { return task.run(task.arg) }
fn execute_nested(holder Holder) voidptr { return holder.task.run(holder.task.arg) }
fn execute_module(task callbacks.ModuleTask) voidptr { return task.run(task.arg) }
fn execute_module_array(tasks []callbacks.ModuleTask) voidptr {
    for task in tasks { return task.run(task.arg) }
    return unsafe { nil }
}
struct StringTask { label fn (int) string }
fn (p Process) label(value int) string { C.exit(91) return "method" }
fn callback_label(value int) string {
    if value == 23 { return "callback aggregate result" }
    return "wrong argument"
}
fn main() {
    mut value := 2
    task := Task{run: callback, arg: &value}
    if execute(task) != voidptr(&value) || value != 9 { C.exit(1) }
    if execute_pointer(&task) != voidptr(&value) || value != 16 { C.exit(2) }
    aliased := AliasedTask{run: callback, arg: &value}
    if execute_alias(aliased) != voidptr(&value) || value != 23 { C.exit(3) }
    holder := Holder{task: task}
    if execute_nested(holder) != voidptr(&value) || value != 30 { C.exit(4) }
    string_task := StringTask{label: callback_label}
    if string_task.label(23) != "callback aggregate result" { C.exit(5) }
    module_task := callbacks.ModuleTask{run: callback, arg: &value}
    if execute_module(module_task) != voidptr(&value) || value != 37 { C.exit(6) }
    if execute_module_array([module_task]) != voidptr(&value) || value != 44 { C.exit(7) }
}
'
		module_source := r'module callbacks
pub struct ModuleTask {
pub:
    run fn (voidptr) voidptr = unsafe { nil }
    arg voidptr
    force_sync bool
    stop bool
    queued_at_ns u64
    done voidptr
    pushes_in_flight &u32 = unsafe { nil }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_function_field_${building_v}_${os.getpid()}.v')
			module_path := path.all_before_last('.') + '_callbacks.v'
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(module_path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source) or { panic(err) }
			os.write_file(module_path, module_source) or { panic(err) }
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_files([path, module_path])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v {
				tc.annotate_types()
			}
			assert tc.errors.len == 0, tc.errors.str()
			if building_v {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			for name in ['execute', 'execute_pointer', 'execute_alias', 'execute_nested',
				'execute_module', 'execute_module_array'] {
				function := m.funcs.filter(it.name == name)[0]
				mut indirect_calls := 0
				for block_id in function.blocks {
					for value_id in m.blocks[block_id].instrs {
						instruction := m.instrs[m.values[value_id].index]
						if instruction.op == .call_indirect {
							indirect_calls++
						}
						if instruction.op == .call {
							callee := m.values[instruction.operands[0]]
							assert callee.name != 'Process.run', name
						}
					}
				}
				assert indirect_calls == 1, name
			}
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
		}
	}
}
