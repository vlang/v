module driver

import os
import strings
import v.cmdexec
import v.flat
import v.gen.c as cgen
import v.modulecache
import v.pref
import v.tempname

fn v3_tcc_dylib_add_embedded_inputs(mut closure V3NativeInputClosure, state &V3ModuleCacheState, program_files []string, vroot string) {
	// Cached declaration headers omit ordinary bodies. Also, the native-input
	// classifier can stop at an opaque C input before reaching any resources.
	// Recover their ownership from the original sources in both cases.
	mut sources := clone_string_list_map(state.module_sources)
	sources['main'] << program_files
	for module_name, files in sources {
		for file in files {
			for path in modulecache.embedded_source_paths(file, vroot) {
				if path !in closure.inputs[module_name] {
					closure.inputs[module_name] << path
				}
			}
		}
	}
}

fn v3_tcc_dylib_native_closure(a &flat.FlatAst, prefs &pref.Preferences, user_flags []string, compiler string, native_inputs &cgen.CacheNativeInputs) V3NativeInputClosure {
	mut flags := user_flags.clone()
	flags << cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
	flags = v3_tcc_dylib_native_flags(flags)
	for flag in flags {
		clean := flag.trim_space()
		if clean.starts_with('@') || clean.starts_with('--config') || clean.starts_with('-specs') || clean.starts_with('--specs') || clean.starts_with('-include-pch') || clean.starts_with('-Wp,') {
			return V3NativeInputClosure{ unassignable: 'opaque native compiler options' }
		}
	}
	path := os.join_path(os.vtmp_dir(), 'v3_native_bridge_${tempname.unique_token()}.m')
	defer { os.rm(path) or {} }
	os.write_file(path, cgen.cache_native_dependency_directives(a, prefs.vroot, flags, prefs.target)) or {
		return V3NativeInputClosure{ unassignable: err.msg() }
	}
	mut args := c_object_compile_flags(flags)
	language := cache_probe_language(cgen.cache_native_inputs_language(a, prefs.vroot, flags,
		prefs.c99, prefs.ccompiler, prefs.target), flags)
	args << ['-x', language]
	deps := c_object_dependencies(compiler, args, path)
	if deps.used_fallback {
		if os.getenv('V3_CACHE_TRACE') != '' {
			mut probe_args := args.clone()
			probe_args << ['-M', '-MT', 'v3cache', path]
			result := cmdexec.run(compiler, probe_args)
			eprintln(result.output)
		}
		return V3NativeInputClosure{ unassignable: 'the native bridge dependency manifest is unavailable' }
	}
	if deps.files.any(it.ends_with('.mm')) && deps.files.any(it != path && it.ends_with('.m')) {
		return V3NativeInputClosure{ unassignable: 'mixed Objective-C and Objective-C++ bridge contexts' }
	}
	// Only V declarations describe the bridge. Never open or preprocess native
	// headers to infer declarations, types, macros or implementation ownership.
	mut bridged_modules := map[string]bool{}
	mut v_functions := map[string]bool{}
	for _, export_name in a.export_fn_names { v_functions[export_name] = true }
	mut current_module := 'main'
	mut current_file := ''
	vroot := os.real_path(prefs.vroot) + os.path_separator
	// Bootstrap checkouts can symlink vlib to the original repository.
	vlib_root := os.real_path(os.join_path(prefs.vroot, 'vlib')) + os.path_separator
	for node in a.nodes {
		if node.kind == .file {
			current_module = 'main'
			current_file = a.cached_header_sources[node.value] or { node.value }
		}
		if node.kind == .module_decl { current_module = node.value }
		if node.kind == .fn_decl && current_module != 'main' {
			qualified := if node.value.starts_with(current_module + '.') {
				node.value
			} else {
				current_module + '.' + node.value
			}
			v_functions[restored_fn_c_name(qualified)] = true
		}
		if node.kind == .directive && node.value in ['include', 'insert', 'preinclude', 'postinclude'] {
			arg := node.typ_text().trim_space().trim('"<>')
			real_file := os.real_path(current_file)
			if arg.ends_with('.m') || arg.ends_with('.mm')
				|| (!real_file.starts_with(vroot) && !real_file.starts_with(vlib_root)) {
				bridged_modules[current_module] = true
			}
		}
	}
	mut functions := map[string]bool{}
	current_module = 'main'
	for node in a.nodes {
		if node.kind == .file { current_module = 'main' }
		if node.kind == .module_decl { current_module = node.value }
		if node.kind == .c_fn_decl && bridged_modules[current_module] {
			name := node.value.all_after_last('.')
			if !v_functions[name] && !name.starts_with('__builtin_') && !name.starts_with('__atomic_')
				&& !name.starts_with('__sync_') && name !in ['va_arg', 'va_start', 'va_end', 'va_copy'] {
				functions[name] = true
			}
		}
	}
	// Paths come from V directives, not from scanning the files they name.
	sources := cgen.native_include_input_files(a, prefs.vroot, prefs.target, flags,
		prefs.compile_values).filter(it.ends_with('.m') || it.ends_with('.mm') || it.ends_with('.c') || it.ends_with('.cc') || it.ends_with('.cpp'))
	mut inputs := {
		'__v3_c_flags__': deps.files.filter(it != path)
	}
	// The C manifest cannot see V resources. Keep their owning module so a
	// payload edit restores its source body before regenerating the object.
	for module_name, paths in native_inputs.module_inputs {
		for input in paths {
			if !native_inputs.native_paths[input] && !native_inputs.macro_paths[input] {
				inputs[module_name] << input
			}
		}
	}
	return V3NativeInputClosure{
		inputs:         inputs
		functions:      functions
		native_sources: sources
	}
}

// Objective-C imports have the same header dependencies as C includes.
fn v3_tcc_dylib_include_argument(line string) ?string {
	if arg := v3_include_directive_argument(line) { return arg }
	clean := line.trim_space()
	if !clean.starts_with('#') { return none }
	directive := clean[1..].trim_left(' \t')
	if !directive.starts_with('import') || (directive.len > 6 && (directive[6].is_letter() || directive[6] == `_`)) {
		return none
	}
	return directive[6..].trim_left(' \t')
}

fn v3_tcc_dylib_native_flags(flags []string) []string {
	mut native := []string{cap: flags.len}
	mut i := 0
	for i < flags.len {
		flag := flags[i]
		if flag == '-D' && i + 1 < flags.len && flags[i + 1].all_before('=') == '__TINYC__' {
			i += 2
			continue
		}
		if flag.all_before('=') != '-D__TINYC__' { native << flag }
		i++
	}
	return native
}

// Ask the C compiler which frameworks the consumer's nested includes use.
// Only its dependency manifest is read, never preprocessed header contents.
fn v3_tcc_dylib_program_dependencies(compiler string, flags []string, source_path string, c_standard string) ![]string {
	mut args := c_object_compile_flags(tcc_cached_main_flags(flags))
	args << [c_standard, '-w', '-x', 'c', '-D__TINYC__=1', '-D__has_extension(x)=0']
	deps := c_object_dependencies(compiler, args, source_path)
	if deps.used_fallback { return error('the TCC program dependency manifest is unavailable') }
	return deps.files
}

fn v3_tcc_dylib_program_link_flags(flags []string, dependencies []string, sdk string) []string {
	mut frameworks := map[string]bool{}
	for path in dependencies {
		mut remaining := path
		for {
			index := remaining.index('.framework/') or { break }
			frameworks[os.file_name(remaining[..index])] = true
			remaining = remaining[index + '.framework/'.len..]
		}
	}
	mut program_flags := []string{}
	mut i := 0
	for i < flags.len {
		if flags[i] == '-framework' && i + 1 < flags.len {
			// Native-only frameworks belong to the owner. Repeating all of them
			// can overflow TCC's fixed Mach-O load-command area.
			if frameworks[flags[i + 1]] { program_flags << flags[i..i + 2] }
			i += 2
			continue
		}
		program_flags << flags[i]
		i++
	}
	return tcc_dynamic_link_flags(v3_tcc_macos_framework_flags(program_flags, 'macos', sdk))
}

// Strip only includes of native source paths named by V directives. C headers
// remain untouched and are handled exclusively by the selected C compiler.
fn v3_tcc_dylib_declarations(source string, native_sources []string) string {
	mut out := strings.new_builder(source.len)
	for line in source.split_into_lines() {
		if arg := v3_tcc_dylib_include_argument(line) {
			if arg.starts_with('"') && os.real_path(arg.trim('"')) in native_sources {
				continue
			}
		}
		out.writeln(line)
	}
	return '#define V3CACHE_NATIVE_INTERFACE 1\n' + out.str()
}

struct V3TccDylibBindings {
	owner string
}

fn v3_tcc_dylib_function_bindings(functions map[string]bool) V3TccDylibBindings {
	mut owner := strings.new_builder(256)
	mut names := functions.keys()
	names.sort()
	for name in names {
		// The native compiler checks and retains its exact type. The separate
		// consumer declarations are generated from explicit V C bindings.
		owner.writeln('__typeof__(&' + name + ') v3_native_bridge_' + name + ' = &' + name + ';')
	}
	return V3TccDylibBindings{ owner: owner.str() }
}

fn v3_tcc_dylib_needs_native_sources(state &V3ModuleCacheState, v_interface_path string) bool {
	if !state.native_bridge { return false }
	// This is a generated V declaration interface (.vh), never a C header.
	text := os.read_file(v_interface_path) or { return true }
	return text.contains('#include') && (text.contains('.m"') || text.contains('.mm"'))
}

fn v3_tcc_dylib_module_declarations(source string, state &V3ModuleCacheState) string {
	return v3_tcc_dylib_declarations(source, state.bridge_native_sources)
}

// Expose the framework headers read by the native compiler using ordinary include
// directories: TCC does not implement Clang's framework search syntax.
fn v3_tcc_dylib_sdk_flags(cache_dir string, inputs []string) []string {
	mut flags := ['-D__MACH__=1', '-D__has_extension(x)=0']
	mut frameworks := map[string]string{}
	for input in inputs {
		if !input.contains('.framework/') || !input.contains('/Headers/') { continue }
		headers := input.all_before('/Headers/') + '/Headers'
		root := headers.all_before_last('.framework') + '.framework'
		frameworks[os.file_name(root).all_before_last('.framework')] = headers
	}
	if frameworks.len == 0 { return flags }
	mut names := frameworks.keys()
	names.sort()
	identity := names.map(it + '=' + frameworks[it]).join('\n')
	directory := os.join_path(cache_dir, 'tcc_framework_headers_' + c_hash_bytes(u64(1469598103934665603), identity.bytes()).hex())
	os.mkdir_all(directory) or { return flags }
	for name in names {
		os.symlink(frameworks[name], os.join_path(directory, name)) or {}
	}
	flags << '-I' + directory
	return flags
}
