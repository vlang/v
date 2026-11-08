module driver

import os
import v.cmdexec
import v.flat
import v.gen.c as cgen
import v.pref
import v.tempname

// External linking for the native arm64 backend on macOS.
//
// The built-in Mach-O linker (vlib/v/gen/arm64/linker.v) links one object
// against libSystem, libobjc and Metal only. That is enough, and fast, for
// programs that call libc. A program that needs C code (sokol, freetype, stb,
// Objective-C embedders, `.c`/`.m` sources), static archives, other dylibs,
// frameworks or the Boehm collector is linked the way Go links cgo programs
// with `-linkmode=external`:
//
// 1. the generator writes a relocatable Mach-O object (Gen.write_object);
// 2. the C side of the program (its `#define`s, `#include`s and `#insert`s) is
//    compiled by the C compiler into one support object, in the same order and
//    with the same `#flag`s the C backend would use;
// 3. `cc` links both objects with the program's libraries, archives and
//    frameworks, and the system linker (ld-prime, or ld64.lld) does the rest:
//    archive member selection, dylib binding, stubs, unwind info and signing.

// Arm64LinkMode is the linker the arm64 backend uses for an executable.
enum Arm64LinkMode {
	internal // built-in Mach-O linker, libSystem/libobjc/Metal only
	external // system linker through `cc`
}

// Libraries that libSystem already provides on macOS; asking for them does not
// need the system linker.
const arm64_libsystem_libraries = ['-lm', '-lc', '-lpthread', '-ldl', '-lSystem', '-lrt']

// arm64_link_mode chooses the linker for a program, like Go's
// `-linkmode=auto`: external as soon as the program brings its own native code
// or libraries. `-d arm64_internal_link` and `-d arm64_external_link` force a
// mode (the first one fails at link time when the program needs C).
fn arm64_link_mode(a &flat.FlatAst, directive_flags []string, user_defines []string) (Arm64LinkMode, string) {
	if 'arm64_internal_link' in user_defines {
		return .internal, ''
	}
	if 'arm64_external_link' in user_defines {
		return .external, '-d arm64_external_link'
	}
	if 'gcboehm' in user_defines {
		return .external, 'the Boehm collector'
	}
	if source := arm64_native_source_include(a) {
		return .external, '`${source}`'
	}
	if header := arm64_project_header_include(a) {
		return .external, '`${header}`'
	}
	mut i := 0
	for i < directive_flags.len {
		flag := directive_flags[i].trim_space()
		if flag in ['-framework', '-l', '-L', '-F', '-weak_framework', '-Xlinker'] {
			if flag == '-l' && i + 1 < directive_flags.len
				&& '-l${directive_flags[i + 1].trim_space()}' in arm64_libsystem_libraries {
				i += 2
				continue
			}
			if flag in ['-framework', '-l', '-weak_framework'] {
				next := if i + 1 < directive_flags.len { directive_flags[i + 1] } else { '' }
				return .external, '`${flag} ${next}`'
			}
			i += 2
			continue
		}
		if flag in arm64_libsystem_libraries || flag.starts_with('-L') || flag.starts_with('-F') {
			i++
			continue
		}
		if flag.starts_with('-framework') || flag.starts_with('-l') || flag.starts_with('-Wl,')
			|| c_flag_is_object_file(flag) || c_flag_token_is_link_only(flag) {
			return .external, '`${flag}`'
		}
		i++
	}
	return .internal, ''
}

// arm64_native_source_include returns the first C/Objective-C/assembly source
// that the program includes into its C side.
fn arm64_native_source_include(a &flat.FlatAst) ?string {
	for node in a.nodes {
		if node.kind != .directive || node.value !in ['include', 'insert', 'preinclude', 'postinclude'] {
			continue
		}
		path := node.typ_text().trim_space().trim('"<>')
		ext := os.file_ext(path)
		if ext in ['.c', '.cc', '.cpp', '.cxx', '.C', '.m', '.mm', '.s', '.S'] {
			return path
		}
	}
	return none
}

// arm64_project_header_include returns the first header of the program or of
// its modules (a quoted `#include`, usually `@VMODROOT/...`). Its `static
// inline` functions and macros only exist in the C side, so the program needs
// the system linker even without a C source or a library. System headers
// (`<stdio.h>`) and V's own vlib headers stay on the built-in linker.
fn arm64_project_header_include(a &flat.FlatAst) ?string {
	for node in a.nodes {
		if node.kind != .directive || node.value != 'include' {
			continue
		}
		mut raw := node.typ_text().trim_space()
		if !raw.starts_with('"') && !raw.starts_with('<') {
			// A platform prefix: `darwin "@VMODROOT/x.h"`.
			raw = raw.all_after(' ').trim_space()
		}
		if !raw.starts_with('"') {
			continue
		}
		path := raw.trim('"')
		if path.starts_with('@VEXEROOT/vlib/') || path.starts_with('@VMODROOT/vlib/') {
			continue
		}
		return path
	}
	return none
}

// NativeCSupportSource is the C translation unit that holds the C side of a
// program compiled by a native backend.
struct NativeCSupportSource {
	text         string
	include_dirs []string
	// has_insert reports whether the unit includes files of the program itself
	// (`#insert`, or a quoted `#include` that names an existing file).
	has_insert bool
}

// NativeCDirective is one C preprocessor line of the program's C side.
struct NativeCDirective {
	text          string
	before_import bool
}

// native_c_support_source collects the program's C preprocessor directives:
// the same `#define`, `#undef`, `#include` and `#insert` lines that the C
// backend writes at the top of its translation unit, resolved for `target` (OS
// prefixes such as `#include linux <sys/timerfd.h>`, `@VEXEROOT`, `$d()`), and
// in its order: a module's imports come before the module's own lines after
// its imports (cgen.FlatGen.ordered_c_directives), so `sokol.sgl`'s
// `util/sokol_gl.h` follows `sokol.gfx`'s `sokol_gfx.h` even when the program
// imports `sokol.sgl` first.
fn native_c_support_source(a &flat.FlatAst, vroot string, target pref.Target, compile_values map[string]string) NativeCSupportSource {
	mut include_dirs := []string{}
	mut include_seen := map[string]bool{}
	mut cur_file := ''
	mut cur_module := 'main'
	mut before_import := true
	mut has_insert := false
	mut by_module := map[string][]NativeCDirective{}
	mut module_order := []string{}
	mut module_imports := map[string][]string{}
	for node in a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = 'main'
				before_import = true
				continue
			}
			.module_decl {
				cur_module = node.value
				continue
			}
			.import_decl {
				before_import = false
				dep := node.value
				if dep.len > 0 {
					mut deps := module_imports[cur_module] or { []string{} }
					if dep !in deps {
						deps << dep
						module_imports[cur_module] = deps
					}
				}
				continue
			}
			.directive {}
			else {
				continue
			}
		}
		raw := flat.source_type_name(node.typ).trim_space()
		if raw.len == 0 {
			continue
		}
		mut line := ''
		match node.value {
			'define', 'undef' {
				arg := cgen.native_directive_arg(raw, target) or { continue }
				if arg.len > 0 {
					line = '#${node.value} ${arg}'
				}
			}
			'include', 'preinclude', 'postinclude', 'insert' {
				include_arg := cgen.native_include_arg(raw, vroot, cur_file, target,
					compile_values)
				if include_arg.len == 0 {
					continue
				}
				if include_arg.starts_with('<') {
					line = '#include ${include_arg}'
				} else {
					path := x64_resolve_insert_path(include_arg, cur_file, vroot)
					if path.len > 0 && os.is_file(path) {
						if node.value == 'insert' {
							dir := os.dir(path)
							if dir.len > 0 && dir !in include_seen {
								include_seen[dir] = true
								include_dirs << dir
							}
						}
						line = '#include "${path}"'
						has_insert = true
					} else if node.value != 'insert' {
						line = '#include ${include_arg}'
					}
				}
			}
			else {}
		}
		if line.len == 0 {
			continue
		}
		if cur_module !in by_module {
			by_module[cur_module] = []NativeCDirective{}
			module_order << cur_module
		}
		by_module[cur_module] << NativeCDirective{
			text:          line
			before_import: before_import
		}
	}
	// Inlined headers can expose atomic and pointer-width helpers without
	// carrying the standard fixed-width integer declarations they depend on.
	mut lines := ['#include <stdint.h>']
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for mod in module_order {
		native_c_visit_directive_module(mod, by_module, module_imports, mut visiting, mut
			visited, mut lines)
	}
	return NativeCSupportSource{
		text:         lines.join('\n') + '\n'
		include_dirs: include_dirs
		has_insert:   has_insert
	}
}

fn native_c_visit_directive_module(mod string, by_module map[string][]NativeCDirective, module_imports map[string][]string, mut visiting map[string]bool, mut visited map[string]bool, mut lines []string) {
	if mod in visited || mod in visiting {
		return
	}
	visiting[mod] = true
	directives := by_module[mod] or { []NativeCDirective{} }
	for directive in directives {
		if directive.before_import {
			lines << directive.text
		}
	}
	// Modules without directives are traversed too, so a transitive import's
	// lines still come before an importer's.
	for dep in module_imports[mod] or { []string{} } {
		native_c_visit_directive_module(dep, by_module, module_imports, mut visiting, mut
			visited, mut lines)
	}
	visiting.delete(mod)
	visited[mod] = true
	for directive in directives {
		if !directive.before_import {
			lines << directive.text
		}
	}
}

// Arm64CSideOptions holds what the C side of an arm64 program needs from the
// driver.
struct Arm64CSideOptions {
	a                  &flat.FlatAst     = unsafe { nil }
	prefs              &pref.Preferences = unsafe { nil }
	user_c_flags       []string
	environment_cflags []string
	optimization_flags []string
	c_compiler         string
	compiler_explicit  bool
}

// Arm64CSide is the C side of an arm64 program: its support unit and the
// command that compiles it.
struct Arm64CSide {
mut:
	compiler     string
	target_args  []string
	flags        []string // the user's and the program's `#flag`s
	support      NativeCSupportSource
	compile_args []string // without the input and output
	wrappers     string   // C wrappers for macros and static functions (arm64.Gen.c_function_wrappers)
}

fn arm64_c_side(opts Arm64CSideOptions) !Arm64CSide {
	prefs := opts.prefs
	compiler := if opts.c_compiler in ['', 'tinyc', 'tcc'] { 'cc' } else { opts.c_compiler }
	target_args := c_compiler_target_args(prefs.target, compiler, opts.compiler_explicit, '')!
	mut flags := opts.user_c_flags.clone()
	flags << cgen.cache_directive_flags(opts.a, prefs.vroot, prefs.target, prefs.compile_values)
	support := native_c_support_source(opts.a, prefs.vroot, prefs.target, prefs.compile_values)
	mut args := []string{}
	args << target_args
	args << ['-w', '-fwrapv', '-fno-common']
	args << opts.optimization_flags
	args << opts.environment_cflags
	for dir in support.include_dirs {
		args << '-I${dir}'
	}
	args << x64_native_compile_flags(flags)
	// The C side is Objective-C when the C backend's unit would be: for
	// `-fobjc-arc` and friends in the program's flags (sokol's Metal backend), or
	// for Objective-C inputs (vlib/macos/objc_bridge.h, AppKit embedders).
	mut objective_c := c_flags_need_objective_c(flags)
		|| cgen.generated_c_unit_needs_objective_c(opts.a, prefs.vroot, flags, prefs.c99, prefs.ccompiler, prefs.target)
	if !objective_c && prefs.target.os == 'macos' {
		// A header can be Objective-C on its own (vlib/macos/objc_bridge.h uses
		// `__bridge` casts). Only the compiler can tell: when the unit does not
		// compile as C, it is compiled as Objective-C, a superset of C.
		objective_c = !arm64_c_unit_compiles(compiler, args, support.text, ['-std=gnu11'])
			&& arm64_c_unit_compiles(compiler, args, support.text, ['-x', 'objective-c'])
	}
	if objective_c {
		args << ['-x', 'objective-c']
	} else {
		args << '-std=gnu11'
	}
	return Arm64CSide{
		compiler:     compiler
		target_args:  target_args
		flags:        flags
		support:      support
		compile_args: args
	}
}

// arm64_c_unit_compiles reports whether `text` passes the C compiler's syntax
// and semantic checks with `args` and `mode`.
fn arm64_c_unit_compiles(compiler string, args []string, text string, mode []string) bool {
	source := os.join_path(os.vtmp_dir(), 'v3_arm64_c_check_${os.getpid()}_${tempname.unique_token()}.c')
	os.write_file(source, text) or { return false }
	defer {
		os.rm(source) or {}
	}
	mut check_args := args.clone()
	check_args << mode
	check_args << ['-fsyntax-only', source]
	return cmdexec.run(compiler, check_args).exit_code == 0
}

// Arm64ExternalLink holds what arm64_external_link needs from the driver.
struct Arm64ExternalLink {
	object             string // the generator's relocatable object
	output             string
	environment_cflags []string
	link_ld_flags      []string
	optimization_flags []string
	target             pref.Target
	c99                bool
	no_std             bool
	show_cc            bool
}

// arm64_external_link compiles the program's C side and links it with the
// generator's object through the system C compiler driver.
fn arm64_external_link(side Arm64CSide, opts Arm64ExternalLink) ! {
	token := '${os.getpid()}_${tempname.unique_token()}'
	support_source := os.join_path(os.vtmp_dir(), 'v3_arm64_c_${token}.c')
	support_object := os.join_path(os.vtmp_dir(), 'v3_arm64_c_${token}.o')
	defer {
		os.rm(support_source) or {}
		os.rm(support_object) or {}
	}
	os.write_file(support_source, side.support.text + side.wrappers)!
	mut cc_args := side.compile_args.clone()
	cc_args << ['-c', support_source, '-o', support_object]
	if opts.show_cc {
		println('${side.compiler} ${cc_args.join(' ')}')
	}
	compiled := cmdexec.run(side.compiler, cc_args)
	if compiled.exit_code != 0 {
		return error('arm64: compiling the C side of the program failed:\n${compiled.output}')
	}

	mut link_flags := x64_native_link_flags(side.flags)
	if link_flags.len > 0 {
		// Objects named by `#flag` (Boehm's gc.o, ...) are built from their
		// adjacent sources with the program's compile flags. Sources named by
		// `#flag` (fontstash_impl.c) are compiled by the link command itself, so
		// it keeps the `-I`/`-D` flags too, as the C backend's command does.
		mut stats := CObjectCacheStats{}
		link_flags = prepare_c_flags_for_link(link_flags, opts.environment_cflags, []string{},
			opts.optimization_flags, opts.c99, opts.no_std, '', side.target_args, opts.target,
			side.compiler, false, os.vtmp_dir(), mut stats)!
	}
	mut link_args := []string{}
	link_args << side.target_args
	link_args << [opts.object, support_object]
	link_args << link_flags
	link_args << opts.link_ld_flags
	if opts.target.os == 'macos' {
		// The same 64 MiB main stack as the C backend's executables and the
		// built-in linker's LC_MAIN: recursive passes such as the compiler's
		// checker need it, the more so while each SSA value has its own slot.
		link_args << '-Wl,-stack_size,0x4000000'
	}
	link_args << ['-o', opts.output]
	if opts.show_cc {
		println('${side.compiler} ${link_args.join(' ')}')
	}
	linked := cmdexec.run(side.compiler, link_args)
	if linked.exit_code != 0 {
		return error('arm64: linking with `${side.compiler}` failed:\n${linked.output}')
	}
}

// Reading C constants, as cgo does.
//
// The arm64 backend has no C front end, and V must not parse C headers itself.
// A `C.NAME` constant of the program (an enum value of sokol, a `#define` of
// stb_image) is resolved like cgo resolves its names: the C compiler compiles
// a probe unit with the program's C side plus one line per name, and the
// values are read back from the probe object. Nothing runs, so it works the
// same when the target is not the host. A name that is not an arithmetic
// constant (a function, a variable, a string, a type) fails to compile on its
// own `#line` and is left out, and the generator reports it as before.

const arm64_c_constant_probe_file = '__v_cconst__'

// arm64_c_constant_names lists the `C.NAME` selectors of the program, without
// the names the backend resolves itself.
fn arm64_c_constant_names(a &flat.FlatAst) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	for node in a.nodes {
		if node.kind != .selector || node.children_count == 0 {
			continue
		}
		base := a.nodes[int(a.child(&node, 0))]
		if base.kind != .ident || base.value_text() != 'C' {
			continue
		}
		name := node.value_text()
		if name.len == 0 || name in seen || name in ['environ', 'errno'] || !arm64_is_c_identifier(name) {
			continue
		}
		seen[name] = true
		names << name
	}
	return names
}

fn arm64_is_c_identifier(name string) bool {
	for i, c in name {
		if !(c.is_letter() || c == `_` || (i > 0 && c.is_digit())) {
			return false
		}
	}
	return true
}

// arm64_c_constant_probe_lines writes one probe line per name; each line
// defines the integer value, the floating point value and whether the
// constant has a floating point type.
fn arm64_c_constant_probe_lines(names []string, keep []bool) string {
	mut out := []string{}
	out << '#define V_CCONST_KEEP __attribute__((used, visibility("hidden")))'
	for i, name in names {
		if !keep[i] {
			continue
		}
		out << '#line ${i + 1} "${arm64_c_constant_probe_file}"'
		out << 'V_CCONST_KEEP const double __v_cconst_f_${i} = (double)(${name}); V_CCONST_KEEP const int __v_cconst_k_${i} = _Generic((${name}), float: 1, double: 1, long double: 1, default: 0); V_CCONST_KEEP const long long __v_cconst_i_${i} = (long long)(${name});'
	}
	return out.join('\n') + '\n'
}

// arm64_probe_preamble includes the system headers that the C backend's
// generated C always includes, so `C.FIONREAD` (sys/ioctl.h) or `C.KERN_SUCCESS`
// (mach/mach.h) are seen without an `#include` of their own.
fn arm64_probe_preamble() string {
	mut preamble := ''
	for header in cgen.posix_preamble_headers {
		preamble += '#include <${header}>\n'
	}
	for header in cgen.apple_preamble_headers {
		preamble += '#include <${header}>\n'
	}
	return preamble
}

// arm64_compile_probe compiles `count` probe lines, written by `lines` for the
// ones still kept, after the program's C side, and returns the symbols with
// `symbol_prefix` and which lines were kept. Each line carries a `#line` in
// `probe_file`, so a line that is an error (a name or field the headers do not
// have) is dropped and the probe is compiled again, as cgo does; each round
// removes at least one line.
fn arm64_compile_probe(count int, lines fn ([]bool) string, probe_file string, symbol_prefix string, support_text string, compiler string, compile_args []string) ?(map[string][]u8, []bool) {
	token := '${os.getpid()}_${tempname.unique_token()}'
	probe_source := os.join_path(os.vtmp_dir(), 'v3_arm64_probe_${token}.c')
	probe_object := os.join_path(os.vtmp_dir(), 'v3_arm64_probe_${token}.o')
	defer {
		os.rm(probe_source) or {}
		os.rm(probe_object) or {}
	}
	preamble := arm64_probe_preamble()
	mut keep := []bool{len: count, init: true}
	mut compiled := false
	for _ in 0 .. 4 {
		text := support_text + preamble + lines(keep)
		os.write_file(probe_source, text) or { return none }
		mut args := compile_args.clone()
		args << ['-ferror-limit=0', '-c', probe_source, '-o', probe_object]
		res := cmdexec.run(compiler, args)
		if res.exit_code == 0 {
			compiled = true
			break
		}
		mut dropped := false
		for line in res.output.split_into_lines() {
			if !line.starts_with('${probe_file}:') {
				continue
			}
			index := line.all_after(':').all_before(':').int() - 1
			if index >= 0 && index < count && keep[index] {
				keep[index] = false
				dropped = true
			}
		}
		if !dropped {
			// The C side itself does not compile; the real compile reports it.
			return none
		}
	}
	if !compiled {
		return none
	}
	symbols := macho_object_symbol_bytes(probe_object, symbol_prefix) or { return none }
	return symbols, keep
}

fn arm64_probe_symbol_i64(symbols map[string][]u8, name string) ?i64 {
	bits := symbols[name] or { return none }
	if bits.len < 8 {
		return none
	}
	return unsafe { *(&i64(&u8(bits.data))) }
}

// arm64_probe_c_constants returns the values of the arithmetic C constants
// among `names`, read from the program's C side.
fn arm64_probe_c_constants(names []string, support_text string, compiler string, compile_args []string) map[string]Arm64CConstant {
	mut result := map[string]Arm64CConstant{}
	if names.len == 0 {
		return result
	}
	symbols, keep := arm64_compile_probe(names.len, fn [names] (keep []bool) string {
		return arm64_c_constant_probe_lines(names, keep)
	}, arm64_c_constant_probe_file, '___v_cconst_', support_text, compiler, compile_args) or {
		return result
	}
	for i, name in names {
		if !keep[i] {
			continue
		}
		kind := symbols['___v_cconst_k_${i}'] or { continue }
		if kind.len < 4 {
			continue
		}
		if kind[0] != 0 {
			bits := symbols['___v_cconst_f_${i}'] or { continue }
			if bits.len < 8 {
				continue
			}
			value := unsafe { *(&f64(&u8(bits.data))) }
			result[name] = Arm64CConstant{
				value:    '${value:.17g}'
				is_float: true
			}
		} else {
			value := arm64_probe_symbol_i64(symbols, '___v_cconst_i_${i}') or { continue }
			result[name] = Arm64CConstant{
				value: value.str()
			}
		}
	}
	return result
}

const arm64_c_struct_probe_file = '__v_cstruct__'

// Arm64CStruct is a `struct C.X` declaration: how C names it and the fields
// the V declaration lists.
struct Arm64CStruct {
	v_name string // `C.FT_FaceRec`
	c_type string // `struct FT_FaceRec`, or `FT_Bitmap` for a typedef
	fields []string
}

// Arm64CStructLayout is the size, alignment and field offsets of a C struct.
struct Arm64CStructLayout {
	size    int
	align   int
	offsets map[string]int
}

// arm64_c_structs lists the program's `struct C.X` declarations with fields.
fn arm64_c_structs(a &flat.FlatAst) []Arm64CStruct {
	mut structs := []Arm64CStruct{}
	mut seen := map[string]bool{}
	for node in a.nodes {
		if node.kind != .struct_decl {
			continue
		}
		v_name := node.value_text()
		if !v_name.starts_with('C.') || v_name in seen || node.children_count == 0 {
			continue
		}
		c_name := v_name[2..]
		if !arm64_is_c_identifier(c_name) {
			continue
		}
		attrs := node.typ_text().split(',')
		c_type := if 'typedef' in attrs {
			c_name
		} else if 'union' in attrs {
			'union ${c_name}'
		} else {
			'struct ${c_name}'
		}
		mut fields := []string{}
		for i in 0 .. node.children_count {
			field := a.child_node(&node, i)
			name := field.value_text().trim_left('@')
			if name.len > 0 && arm64_is_c_identifier(name) {
				fields << name
			}
		}
		seen[v_name] = true
		structs << Arm64CStruct{
			v_name: v_name
			c_type: c_type
			fields: fields
		}
	}
	return structs
}

// arm64_c_struct_probe_lines writes one line for the size and alignment of
// each struct and one per field offset, so a declaration or field that C does
// not have only drops its own line.
fn arm64_c_struct_probe_lines(structs []Arm64CStruct, keep []bool) string {
	mut out := []string{}
	out << '#include <stddef.h>'
	out << '#define V_CSTRUCT_KEEP __attribute__((used, visibility("hidden")))'
	mut line := 0
	for i, st in structs {
		if keep[line] {
			out << '#line ${line + 1} "${arm64_c_struct_probe_file}"'
			out << 'V_CSTRUCT_KEEP const long long __v_cstruct_s_${i} = (long long)sizeof(${st.c_type}); V_CSTRUCT_KEEP const long long __v_cstruct_a_${i} = (long long)_Alignof(${st.c_type});'
		}
		line++
		for j, field in st.fields {
			if keep[line] {
				out << '#line ${line + 1} "${arm64_c_struct_probe_file}"'
				out << 'V_CSTRUCT_KEEP const long long __v_cstruct_o_${i}_${j} = (long long)offsetof(${st.c_type}, ${field});'
			}
			line++
		}
	}
	return out.join('\n') + '\n'
}

// arm64_probe_c_struct_layouts reads the layouts of the program's C structs
// from the C compiler, like cgo's -godefs: V declarations of C structs often
// list only the fields a binding reads (vglyph's `C.FT_GlyphSlotRec`), so their
// offsets cannot be computed from the V declaration.
fn arm64_probe_c_struct_layouts(structs []Arm64CStruct, support_text string, compiler string, compile_args []string) map[string]Arm64CStructLayout {
	mut result := map[string]Arm64CStructLayout{}
	mut count := 0
	for st in structs {
		count += 1 + st.fields.len
	}
	if count == 0 {
		return result
	}
	symbols, keep := arm64_compile_probe(count, fn [structs] (keep []bool) string {
		return arm64_c_struct_probe_lines(structs, keep)
	}, arm64_c_struct_probe_file, '___v_cstruct_', support_text, compiler, compile_args) or {
		return result
	}
	mut line := 0
	for i, st in structs {
		struct_line := line
		line += 1 + st.fields.len
		if !keep[struct_line] {
			continue
		}
		size := arm64_probe_symbol_i64(symbols, '___v_cstruct_s_${i}') or { continue }
		align := arm64_probe_symbol_i64(symbols, '___v_cstruct_a_${i}') or { continue }
		mut offsets := map[string]int{}
		for j, field in st.fields {
			if !keep[struct_line + 1 + j] {
				continue
			}
			offset := arm64_probe_symbol_i64(symbols, '___v_cstruct_o_${i}_${j}') or { continue }
			offsets[field] = int(offset)
		}
		result[st.v_name] = Arm64CStructLayout{
			size:    int(size)
			align:   int(align)
			offsets: offsets
		}
	}
	return result
}

// Arm64CFunctionKind is why a C function needs a wrapper (see
// arm64.CFunctionKind).
enum Arm64CFunctionKind {
	macro
	internal
}

const arm64_c_function_probe_file = '__v_cfn__'

// arm64_probe_c_functions finds, among the C functions V code calls, the
// function-like macros and the functions with internal linkage: neither has a
// symbol the linker can bind V's calls to. Like the constants, they are found
// by compiling a probe with the program's C side; a name that the headers do
// not declare fails on its own line and is left for the linker.
fn arm64_probe_c_functions(names []string, support_text string, compiler string, compile_args []string) map[string]Arm64CFunctionKind {
	mut result := map[string]Arm64CFunctionKind{}
	if names.len == 0 {
		return result
	}
	token := '${os.getpid()}_${tempname.unique_token()}'
	probe_source := os.join_path(os.vtmp_dir(), 'v3_arm64_cfn_${token}.c')
	probe_object := os.join_path(os.vtmp_dir(), 'v3_arm64_cfn_${token}.o')
	defer {
		os.rm(probe_source) or {}
		os.rm(probe_object) or {}
	}
	mut keep := []bool{len: names.len}
	for i, name in names {
		keep[i] = arm64_is_c_identifier(name)
	}
	mut compiled := false
	for _ in 0 .. 4 {
		mut lines := []string{}
		lines << '#define V_CFN_KEEP __attribute__((used, visibility("hidden")))'
		for i, name in names {
			if !keep[i] {
				continue
			}
			lines << '#line ${i + 1} "${arm64_c_function_probe_file}"'
			lines << '#ifdef ${name}'
			lines << '#line ${i + 1} "${arm64_c_function_probe_file}"'
			lines << 'V_CFN_KEEP const int __v_cfn_macro_${i} = 1;'
			lines << '#else'
			lines << '#line ${i + 1} "${arm64_c_function_probe_file}"'
			lines << 'V_CFN_KEEP void *const __v_cfn_addr_${i} = (void *)&${name};'
			lines << '#endif'
		}
		os.write_file(probe_source, support_text + lines.join('\n') + '\n') or { return result }
		mut args := compile_args.clone()
		args << ['-ferror-limit=0', '-c', probe_source, '-o', probe_object]
		res := cmdexec.run(compiler, args)
		if res.exit_code == 0 {
			compiled = true
			break
		}
		mut dropped := false
		for line in res.output.split_into_lines() {
			if !line.starts_with('${arm64_c_function_probe_file}:') {
				continue
			}
			index := line.all_after(':').all_before(':').int() - 1
			if index >= 0 && index < names.len && keep[index] {
				keep[index] = false
				dropped = true
			}
		}
		if !dropped {
			return result
		}
	}
	if !compiled {
		return result
	}
	symbols := macho_object_symbols(probe_object) or { return result }
	mut local_functions := map[string]bool{}
	mut macros := map[string]bool{}
	for sym in symbols {
		if sym.name.starts_with('___v_cfn_macro_') {
			macros[sym.name] = true
		} else if !sym.external && sym.name.starts_with('_') {
			local_functions[sym.name[1..]] = true
		}
	}
	for i, name in names {
		if !keep[i] {
			continue
		}
		if '___v_cfn_macro_${i}' in macros {
			result[name] = .macro
		} else if name in local_functions {
			result[name] = .internal
		}
	}
	return result
}

// macho_object_symbol_bytes returns, for each defined symbol of a 64-bit
// Mach-O object whose name starts with `prefix`, the bytes from the symbol to
// the next symbol (or the end of its section).
fn macho_object_symbol_bytes(path string, prefix string) !map[string][]u8 {
	data := os.read_bytes(path)!
	if data.len < 32 || macho_u32(data, 0) != 0xfeedfacf {
		return error('not a 64-bit Mach-O object')
	}
	ncmds := int(macho_u32(data, 16))
	mut sect_addr := []u64{}
	mut sect_size := []u64{}
	mut sect_off := []u64{}
	mut sect_zerofill := []bool{}
	mut off := 32
	for _ in 0 .. ncmds {
		if off + 8 > data.len {
			return error('truncated load commands')
		}
		cmd := macho_u32(data, off)
		cmdsize := int(macho_u32(data, off + 4))
		if cmd == 0x19 {
			nsects := int(macho_u32(data, off + 64))
			for s in 0 .. nsects {
				base := off + 72 + s * 80
				sect_addr << macho_u64(data, base + 32)
				sect_size << macho_u64(data, base + 40)
				sect_off << u64(macho_u32(data, base + 48))
				sect_zerofill << macho_u32(data, base + 64) & 0xff in [u32(0x1), 0xc, 0x12]
			}
		}
		off += cmdsize
	}
	entries := macho_defined_symbols(data)!
	mut result := map[string][]u8{}
	for e in entries {
		if !e.name.starts_with(prefix) || e.sect < 0 || e.sect >= sect_addr.len {
			continue
		}
		mut end := sect_addr[e.sect] + sect_size[e.sect]
		for other in entries {
			if other.sect == e.sect && other.value > e.value && other.value < end {
				end = other.value
			}
		}
		if sect_zerofill[e.sect] {
			result[e.name] = []u8{len: int(end - e.value)}
			continue
		}
		start := int(sect_off[e.sect] + e.value - sect_addr[e.sect])
		stop := int(sect_off[e.sect] + end - sect_addr[e.sect])
		if start < 0 || stop > data.len || stop < start {
			continue
		}
		result[e.name] = data[start..stop].clone()
	}
	return result
}

// Arm64CConstant is a probed C constant (see ssa.NativeCConstant).
struct Arm64CConstant {
	value    string
	is_float bool
}

struct MachOProbeSymbol {
	name     string
	sect     int
	value    u64
	external bool
}

// macho_object_symbols returns the defined symbols of a 64-bit Mach-O object.
fn macho_object_symbols(path string) ![]MachOProbeSymbol {
	data := os.read_bytes(path)!
	return macho_defined_symbols(data)!
}

fn macho_defined_symbols(data []u8) ![]MachOProbeSymbol {
	if data.len < 32 || macho_u32(data, 0) != 0xfeedfacf {
		return error('not a 64-bit Mach-O object')
	}
	ncmds := int(macho_u32(data, 16))
	mut off := 32
	mut symoff := 0
	mut nsyms := 0
	mut stroff := 0
	for _ in 0 .. ncmds {
		if off + 24 > data.len {
			return error('truncated load commands')
		}
		if macho_u32(data, off) == 0x2 {
			symoff = int(macho_u32(data, off + 8))
			nsyms = int(macho_u32(data, off + 12))
			stroff = int(macho_u32(data, off + 16))
		}
		off += int(macho_u32(data, off + 4))
	}
	mut result := []MachOProbeSymbol{}
	for i in 0 .. nsyms {
		base := symoff + i * 16
		if base + 16 > data.len {
			break
		}
		typ := data[base + 4]
		if typ & 0x0e != 0x0e || data[base + 5] == 0 {
			continue
		}
		name_off := stroff + int(macho_u32(data, base))
		mut end := name_off
		for end < data.len && data[end] != 0 {
			end++
		}
		result << MachOProbeSymbol{
			name:     data[name_off..end].bytestr()
			sect:     int(data[base + 5]) - 1
			value:    macho_u64(data, base + 8)
			external: typ & 0x01 != 0
		}
	}
	return result
}

fn macho_u32(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[off + 3]) << 24)
}

fn macho_u64(data []u8, off int) u64 {
	return u64(macho_u32(data, off)) | (u64(macho_u32(data, off + 4)) << 32)
}
