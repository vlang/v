module c

import os
import v.parser
import v.pref
import v.types

// Execute the actual generated bodies and initializer in a native C consumer.
// Runtime exports may be called before V initialization; application exports must
// still initialize exactly once, including natural names and split cache units.
fn test_builtin_exports_do_not_enter_no_main_initialization() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'builtin_export_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut files := []string{}
	for name, source in {
		'builtin': "module builtin
@[export: 'boot_alias']
fn boot_impl() i32 { return 11 }
@[export: 'boot_natural']
fn boot_natural() i32 { return 12 }
"
		'main':    "module main
@[export: 'user_alias']
fn user_impl() i32 { return 21 }
@[export: 'user_natural']
fn user_natural() i32 { return 22 }
"
		'other':   "module other
fn init() {}
@[export: 'module_alias']
fn module_impl() i32 { return 31 }
@[export: 'other__module_natural']
fn module_natural() i32 { return 32 }
"
	} {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, source)!
		files << path
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(files)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	for mode in ['no_main', 'object', 'cache_split'] {
		mut g := FlatGen.new()
		g.set_suppress_main(true)
		g.set_object_file_mode(mode == 'object')
		g.set_cache_split(mode == 'cache_split')
		generated := g.gen_with_used_options(a, map[string]bool{}, &tc, true)
		mut definitions := ''
		mut symbols := []string{}
		for module_name, names in {
			'builtin': ['boot_impl', 'boot_natural']
			'main':    ['user_impl', 'user_natural']
			'other':   ['module_impl', 'module_natural']
		} {
			for name in names {
				canonical := g.fn_c_name_in_module(module_name, name)
				exported := g.export_fn_name_in_module(module_name, name) or { panic(name) }
				assert (canonical == exported) == name.ends_with('_natural'), '${module_name}: ${name}'
				for symbol in if canonical == exported {
					[canonical]
				} else {
					[canonical, exported]
				} {
					definition := builtin_export_definition(generated, symbol)
					if symbol == exported {
						assert definition.contains('_vno_main_init_caller();') == (module_name != 'builtin'), '${mode}: ${definition}'
					}
					definitions += definition
					symbols << symbol
				}
			}
		}
		mut prototypes := '#include <stdbool.h>\ntypedef int i32;\n'
		for symbol in symbols { prototypes += 'i32 ${symbol}(void);\n' }
		initializer := builtin_export_definition(generated, '_vno_main_init_caller')
		assert initializer.contains('_vinit();'), initializer
		guard := 'static bool _v3_no_main_initialized;\n' + initializer
		// Calls through C function pointers must not initialize merely because a
		// compiler-owned runtime function is exported under another symbol.
		consumer := '
int main(void) {
	int (*runtime_alias)(void) = boot_alias;
	int (*runtime_natural)(void) = boot_natural;
	if (runtime_alias() != 11 || runtime_natural() != 12 || init_calls != 0) return 1;
	if (user_alias() != 21 || init_calls != 1) return 2;
	if (user_natural() != 22 || module_alias() != 31 || other__module_natural() != 32 || init_calls != 1) return 3;
	if (runtime_alias() != 11 || runtime_natural() != 12 || init_calls != 1) return 4;
	return 0;
}
'
		for optimization in ['-O0', '-O3'] {
			executable := os.join_path(root, '${mode}_${optimization}')
			mut inputs := []string{}
			program := os.join_path(root, 'consumer.c')
			if mode == 'cache_split' {
				init_unit := os.join_path(root, 'init.c')
				os.write_file(init_unit, '#include <stdbool.h>\nint init_calls;\nstatic void _vinit(void) { init_calls++; }\n' + guard)!
				os.write_file(program, prototypes + 'extern int init_calls;\nvoid _vno_main_init_caller(void);\n' + definitions + consumer)!
				inputs << init_unit
			} else {
				os.write_file(program, prototypes + 'static int init_calls;\nstatic void _vinit(void) { init_calls++; }\n' + guard + definitions + consumer)!
			}
			inputs << program
			mut args := [cc, '-std=c11', optimization, '-Wall', '-Wextra', '-Werror', '-o', executable]
			args << inputs
			built := os.exec(args)
			assert built.exit_code == 0, built.output
			ran := os.exec([executable])
			assert ran.exit_code == 0, '${mode}: ${ran.output}'
		}
	}
}

fn test_macos_raw_startup_exports_skip_no_main_initialization() ! {
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'raw_startup_export_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "@[export: 'user_alias']
fn implementation() i32 { return 7 }
@[export: 'user_natural']
fn user_natural() i32 { return 9 }
fn main() {}
")!
	output := os.join_path(root, 'main.c')
	built := os.exec([@VEXE, '-os', 'macos', '-arch', 'amd64', '-freestanding', '-d', 'no_main',
		'-gc', 'none', '-o', output, source])
	assert built.exit_code == 0, built.output
	generated := os.read_file(output)!
	// The raw entry calls prepare_start before GS contains V's TLS. The C heap
	// exports can also be reached while _vinit is still constructing globals.
	for symbol in ['v_macos_bare_prepare_start', 'malloc', 'free', 'memcpy'] {
		definition := builtin_export_definition(generated, symbol)
		assert !definition.contains('_vno_main_init_caller();'), definition
	}
	for symbol in ['user_alias', 'user_natural'] {
		definition := builtin_export_definition(generated, symbol)
		assert definition.contains('_vno_main_init_caller();'), definition
	}
}

fn builtin_export_definition(source string, symbol string) string {
	for line in source.split_into_lines() {
		if !line.starts_with('\t') && (line.contains(' ${symbol}(') || line.contains(' (${symbol})(')) && line.ends_with(' {') {
			return line + source.all_after(line).all_before('\n}\n') + '\n}\n'
		}
	}
	panic('missing generated definition: ${symbol}')
}
