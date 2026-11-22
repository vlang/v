// c-api generates a C library release (shared library + header + def + pc +
// demo + README) from the public API of a V module or package.
module main

import os
import rand
import strings
import v.capi

struct Options {
mut:
	dir            string = '.'
	out_dir        string = 'dist'
	lib_path       string
	lib_name       string
	import_path    string
	version        string
	description    string
	no_build       bool
	no_submodules  bool
	prod           bool
	keep_generated bool
}

fn usage() {
	eprintln('usage: v c-api <module-dir> [-o <dir>] [--lib-name <name>] [--version <v>]')
	eprintln('                       [--import-path <v.module.path>] [--no-submodules] [--prod]')
	eprintln('                       [--description <text>] [--no-build] [--keep-generated]')
}

fn parse_args(args []string) !Options {
	mut opts := Options{}
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == '-h' || arg == '--help' {
			usage()
			exit(0)
		}
		if arg == '-o' || arg == '--out' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			value := args[i]
			if value.ends_with('.so') || value.ends_with('.dll') || value.ends_with('.dylib') {
				opts.lib_path = value
				opts.out_dir = os.dir(value)
			} else {
				opts.out_dir = value
			}
		} else if arg == '--lib' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			opts.lib_path = args[i]
		} else if arg == '--lib-name' || arg == '--import-path' || arg == '--version' || arg == '--description' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			if arg == '--lib-name' {
				opts.lib_name = args[i]
			} else if arg == '--import-path' {
				opts.import_path = args[i]
			} else if arg == '--version' {
				opts.version = args[i]
			} else {
				opts.description = args[i]
			}
		} else if arg == '--no-build' {
			opts.no_build = true
		} else if arg == '--no-submodules' {
			opts.no_submodules = true
		} else if arg == '--prod' {
			opts.prod = true
		} else if arg == '--keep-generated' {
			opts.keep_generated = true
		} else if arg.starts_with('-') {
			return error('unknown option ${arg}')
		} else if opts.dir == '.' {
			opts.dir = arg
		} else {
			return error('unexpected argument ${arg}')
		}
		i++
	}
	return opts
}

fn lib_file_name(name string) string {
	os_name := os.user_os()
	if os_name == 'windows' {
		return '${name}.dll'
	}
	if os_name == 'macos' {
		return 'lib${name}.dylib'
	}
	return 'lib${name}.so'
}

fn module_name_of(dir string) string {
	entries := os.ls(dir) or { return '' }
	for entry in entries {
		if !entry.ends_with('.v') || entry.ends_with('_test.v') || entry.ends_with('.c.v') {
			continue
		}
		text := os.read_file(os.join_path(dir, entry)) or { continue }
		for line in text.split('\n') {
			t := line.trim_space()
			if t.starts_with('module ') {
				return t[7..].trim_space()
			}
		}
	}
	return ''
}

fn discover_modules(dir string, mut out []string) {
	name := module_name_of(dir)
	// `module main` directories are programs/shims, not library submodules.
	if name != '' && name != 'main' {
		out << dir
	}
	entries := os.ls(dir) or { return }
	for entry in entries {
		if entry.starts_with('.') || entry in ['cmd', 'testdata', 'tests', 'dist', 'build'] {
			continue
		}
		sub := os.join_path(dir, entry)
		if os.is_dir(sub) {
			discover_modules(sub, mut out)
		}
	}
}

fn rel_dir(root string, dir string) string {
	if dir == root {
		return ''
	}
	prefix := root.trim_right(os.path_separator) + os.path_separator
	if !dir.starts_with(prefix) {
		return ''
	}
	return dir[prefix.len..].replace(os.path_separator, '.')
}

fn base_import_path(root string, opts Options) string {
	if opts.import_path != '' {
		return opts.import_path
	}
	text := os.read_file(os.join_path(root, 'v.mod')) or { return '' }
	for line in text.split('\n') {
		t := line.trim_space()
		if t.starts_with('name:') {
			return t[5..].trim_space().trim("'").trim('"')
		}
	}
	return ''
}

fn import_path_of(rel string, base string) string {
	if rel == '' {
		return base
	}
	return if base == '' { rel } else { '${base}.${rel}' }
}

fn copy_module_files(src string, dst string) ! {
	os.mkdir_all(dst) or { return error('cannot create ${dst}: ${err}') }
	for entry in os.ls(src) or { return error('cannot list ${src}: ${err}') } {
		if entry.ends_with('_test.v') || entry.starts_with('capi_generated') || entry.starts_with('.') {
			continue
		}
		if os.is_dir(os.join_path(src, entry)) {
			continue
		}
		os.copy_file(os.join_path(src, entry), os.join_path(dst, entry)) or {
			return error('cannot copy ${entry}: ${err}')
		}
	}
}

fn body_after_module(code string) string {
	idx := code.index('\n') or { return '' }
	return code[idx + 1..]
}

// combine_main_source puts the runtime first: V requires __global blocks to
// appear before function declarations.
fn combine_main_source(wrappers string, runtime string, imports []string) string {
	mut b := strings.Builder{}
	b.writeln('module main')
	for imp in imports {
		b.writeln('import ${imp}')
	}
	b.writeln('')
	b.writeln(body_after_module(runtime))
	b.write_string(body_after_module(wrappers))
	return b.str()
}

struct BuildPlan {
	model       capi.ApiModel
	infos       []capi.ExportInfo
	wrappers    string
	runtime     string
	build_input string
	vmodules    string
	temp_dir    string
}

fn build_plan(opts Options) !BuildPlan {
	mut warnings := []string{}
	if opts.no_submodules {
		m := capi.collect_model(opts.dir, opts.lib_name) or { return err }
		wrappers, infos := capi.generate_wrappers(m, mut warnings)
		for w in warnings {
			eprintln('c-api: warning: ${w}')
		}
		return BuildPlan{
			model:       m
			infos:       infos
			wrappers:    wrappers
			runtime:     capi.generate_runtime(m)
			build_input: ''
			vmodules:    ''
			temp_dir:    ''
		}
	}
	mut modules := []string{}
	discover_modules(opts.dir, mut modules)
	if modules.len <= 1 {
		m := capi.collect_model(opts.dir, opts.lib_name) or { return err }
		wrappers, infos := capi.generate_wrappers(m, mut warnings)
		for w in warnings {
			eprintln('c-api: warning: ${w}')
		}
		return BuildPlan{
			model:       m
			infos:       infos
			wrappers:    wrappers
			runtime:     capi.generate_runtime(m)
			build_input: ''
			vmodules:    ''
			temp_dir:    ''
		}
	}
	base := base_import_path(opts.dir, opts)
	if base == '' {
		return error('submodule builds need --import-path or a v.mod with a name')
	}
	temp_dir := os.join_path(os.vtmp_dir(), 'capi_build_${rand.ulid()}')
	os.mkdir_all(temp_dir) or { return error('cannot create ${temp_dir}: ${err}') }
	base_parts := base.split('.')
	mut base_dir := temp_dir
	for part in base_parts {
		base_dir = os.join_path(base_dir, part)
	}
	mut imports := []string{}
	mut combined := capi.ApiModel{}
	mut first := true
	for mdir in modules {
		rel := rel_dir(opts.dir, mdir)
		imp := import_path_of(rel, base)
		qualifier := imp.all_after_last('.')
		imports << imp
		short := capi.sanitize(if rel == '' { 'root' } else { rel.replace('.', '_') })
		module_prefix := '${opts.lib_name}_${short}'
		m := capi.collect_model_qualified(mdir, module_prefix, qualifier) or { return err }
		copy_module_files(mdir, os.join_path(base_dir, ...rel.split('.'))) or { return err }
		if first {
			combined = capi.ApiModel{
				module_name: 'main'
				prefix:      capi.sanitize(opts.lib_name)
			}
			first = false
		}
		combined.funcs << m.funcs
		combined.types << m.types
		combined.consts << m.consts
	}
	wrappers, infos := capi.generate_wrappers(combined, mut warnings)
	for w in warnings {
		eprintln('c-api: warning: ${w}')
	}
	runtime := capi.generate_runtime_for(combined, 'main', capi.sanitize(opts.lib_name))
	// One .c.v file holds both the wrappers and the runtime: V only compiles
	// the file passed as input, so a separate companion would be omitted.
	source_path := os.join_path(temp_dir, 'capi_generated.c.v')
	os.write_file(source_path, combine_main_source(wrappers, runtime, imports)) or { return err }
	return BuildPlan{
		model:       combined
		infos:       infos
		wrappers:    wrappers
		runtime:     runtime
		build_input: source_path
		vmodules:    temp_dir
		temp_dir:    temp_dir
	}
}

// make_import_libraries builds the import libraries consumers need on Windows.
// dlltool covers MinGW/GCC; `lib` covers MSVC. Missing tools only warn: the
// generated .def is sufficient for either to be created manually.
fn make_import_libraries(name string, lib_path string, def_path string, out_dir string) {
	gcc_lib := os.join_path(out_dir, 'lib${name}.dll.a')
	dlltool := os.exec(['dlltool', '-d', def_path, '--dllname', os.base(lib_path), '-l', gcc_lib])
	if dlltool.exit_code == 0 {
		println('c-api: importlib ${gcc_lib}')
	} else {
		eprintln('c-api: warning: dlltool unavailable; build ${gcc_lib} from ${def_path} manually')
	}
	msvc_lib := os.join_path(out_dir, '${name}.lib')
	lib := os.exec(['lib', '/def:${def_path}', '/out:${msvc_lib}', '/machine:x64'])
	if lib.exit_code == 0 {
		println('c-api: importlib ${msvc_lib}')
	}
}

fn cleanup_generated(paths []string) {
	for path in paths {
		os.rm(path) or {}
	}
}

fn main() {
	mut generated_files := []string{}
	mut args := os.args[1..].clone()
	if args.len > 0 && (args[0] == 'c-api' || args[0] == 'capi') {
		args = args[1..]
	}
	opts := parse_args(args) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	if !os.is_dir(opts.dir) {
		eprintln('c-api: not a directory: ${opts.dir}')
		exit(1)
	}
	name := if opts.lib_name == '' {
		capi.sanitize(module_name_of(opts.dir))
	} else {
		capi.sanitize(opts.lib_name)
	}
	plan := build_plan(opts) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	if plan.temp_dir == '' {
		wrapper_path := os.join_path(opts.dir, 'capi_generated.v')
		runtime_path := os.join_path(opts.dir, 'capi_generated.c.v')
		if os.exists(wrapper_path) || os.exists(runtime_path) {
			eprintln('c-api: generated files already exist in ${opts.dir}; remove them first')
			exit(1)
		}
		os.write_file(wrapper_path, plan.wrappers) or {
			eprintln('c-api: cannot write ${wrapper_path}: ${err}')
			exit(1)
		}
		os.write_file(runtime_path, plan.runtime) or {
			os.rm(wrapper_path) or {}
			eprintln('c-api: cannot write ${runtime_path}: ${err}')
			exit(1)
		}
		generated_files << [wrapper_path, runtime_path]
	}
	result := capi.emit_files(plan.model, plan.infos, capi.EmitOptions{
		out_dir:     opts.out_dir
		lib_name:    name
		version:     opts.version
		description: opts.description
	}) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	if opts.no_build {
		println('c-api: generated ${result.header}, ${result.def}, ${result.pc}, ${result.demo}, ${result.readme}')
		if !opts.keep_generated {
			cleanup_generated(generated_files)
		}
		return
	}
	lib_path := if opts.lib_path == '' {
		os.join_path(opts.out_dir, lib_file_name(name))
	} else {
		opts.lib_path
	}
	mut build_args := [@VEXE, '-shared', '-enable-globals', '-nocache']
	if opts.prod {
		build_args << '-prod'
	}
	if os.user_os() == 'windows' {
		build_args << '-cflags'
		build_args << '-Wl,--export-all-symbols'
	}
	build_args << '-o'
	build_args << lib_path
	build_input := if plan.build_input == '' { opts.dir } else { plan.build_input }
	build_args << build_input
	old_vmodules := os.getenv_opt('VMODULES')
	if plan.vmodules != '' {
		os.setenv('VMODULES', plan.vmodules, true)
	}
	res := os.exec(build_args)
	if plan.vmodules != '' {
		if value := old_vmodules {
			os.setenv('VMODULES', value, true)
		} else {
			os.unsetenv('VMODULES')
		}
		if !opts.keep_generated {
			os.rmdir_all(plan.temp_dir) or {}
		}
	}
	if !opts.keep_generated {
		cleanup_generated(generated_files)
	}
	if res.exit_code != 0 {
		eprintln('c-api: shared library build failed:')
		eprintln(res.output)
		exit(1)
	}
	println('c-api: built ${lib_path}')
	if os.user_os() == 'windows' {
		make_import_libraries(name, lib_path, result.def, opts.out_dir)
	}
	println('c-api: header ${result.header}')
	println('c-api: def    ${result.def}')
	println('c-api: pkgcfg ${result.pc}')
	println('c-api: demo   ${result.demo}')
	println('c-api: readme ${result.readme}')
}
