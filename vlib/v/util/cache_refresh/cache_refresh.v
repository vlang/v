module cache_refresh

import os
import encoding.hex
import strconv
import strings
import time
import v.cmdexec
import v.pref
import v.tempname

const record_prefix = '.vup_refresh_'
const record_format = 'vup-cache-refresh-7'
const input_record_format = 'vup-cache-refresh-6'
const entry_record_format = 'vup-cache-refresh-5'
const project_record_format = 'vup-cache-refresh-4'
const object_record_format = 'vup-cache-refresh-3'
const cwd_record_format = 'vup-cache-refresh-2'
const legacy_record_format = 'vup-cache-refresh-1'

// request_uses_prod_tcc applies the production restriction using the recorded compiler
// paths and working directory, including context-sensitive compiler wrappers.
fn request_uses_prod_tcc(request Request) bool {
	return pref.tcc_with_prod_requested_in(request.args, request.cwd)
}

// FlagGroup preserves the ordering and deduplication of a project's C directives.
pub struct FlagGroup {
pub:
	flags    []string
	at_start bool
	in_main  bool
	identity string // Unfiltered group key, for deduplication against module directives.
}

// Request describes how to rebuild one previously cached standard-library module.
pub struct Request {
pub:
	args            []string
	module          string
	is_test         bool
	cwd             string            // Original directory for caller-relative compilation inputs.
	project_flags   []FlagGroup       // Directives from sources outside the refreshed vlib modules.
	imports         []string          // Original vlib imports contributing module-wide C flags.
	entry           string            // Original program input, for interface-specific module aliases.
	show_test_stats bool              // Effective VTEST_SHOW_ASSERTS configuration.
	compile_inputs  []string          // Required caller-supplied files that existed when recorded.
	entry_sources   map[string]string // Source identities tying a live entry to its recorded configuration.
	legacy_headers  map[string]string // Legacy header paths and content signatures awaiting migration.
}

// RefreshOptions controls progress reporting during module cache validation.
@[params]
pub struct RefreshOptions {
pub:
	verbose bool // Report every checked configuration, including unchanged modules.
}

// root_dir returns the cache shared by the configurations of this V checkout,
// using vexe to identify the compiler's isolated cache rather than its helpers.
pub fn root_dir(vroot string, vexe string) string {
	base := os.abs_path(os.getenv_opt('V3CACHE') or {
		default_dir := os.vtmp_dir()
		if os.getenv('V3_TEST_ISOLATE_CACHE') == '1' {
			os.join_path(default_dir, 'v3_test_cache_${hash_text(os.real_path(vexe))}')
		} else {
			default_dir
		}
	})
	return os.join_path(base, 'v3_module_cache_${hash_text(os.real_path(vroot))}')
}

fn hash_text(text string) string {
	mut hash := u64(1469598103934665603)
	for byte in text.bytes() {
		hash = (hash ^ u64(byte)) * u64(1099511628211)
	}
	return hash.hex()
}

fn encode_request(request Request) string {
	mut out := strings.new_builder(128)
	out.writeln(record_format)
	out.writeln(request.module)
	out.writeln(if request.is_test { 'test' } else { 'program' })
	out.write_string(encode_arguments([request.cwd, encode_project_flags(request.project_flags),
		encode_arguments(request.imports), request.entry, request.show_test_stats.str(),
		encode_arguments(request.compile_inputs), encode_entry_sources(request.entry_sources),
		...request.args]))
	return out.str()
}

fn encode_arguments(args []string) string {
	mut out := strings.new_builder(128)
	// Length prefixes preserve spaces, quoting and newlines in C flag values.
	for arg in args {
		out.writeln(arg.len.str())
		out.write_string(arg)
	}
	return out.str()
}

fn decode_request(content string) !Request {
	parts := content.split_nth('\n', 4)
	if parts.len != 4 || parts[0] !in [record_format, input_record_format, entry_record_format,
		project_record_format, object_record_format, cwd_record_format, legacy_record_format] || !valid_module(parts[1])
		|| parts[2] !in ['test', 'program'] {
		return error('invalid module cache refresh record')
	}
	args := decode_arguments(parts[3])!
	if parts[0] == legacy_record_format {
		return Request{ args: args, module: parts[1], is_test: parts[2] == 'test' }
	}
	if args.len == 0 { return error('missing working directory') }
	if parts[0] == cwd_record_format {
		return Request{ args: args[1..].clone(), module: parts[1], is_test: parts[2] == 'test', cwd: args[0] }
	}
	if args.len < 3 { return error('missing project flags') }
	if parts[0] == object_record_format {
		if args[1] !in ['true', 'false'] { return error('invalid object flags') }
		count := strconv.atoi(args[2])!
		if count < 0 || count > args.len - 3 { return error('invalid object flag count') }
		// These records have no provenance: replaying their full flag vector can
		// override updated module directives. Retain their CLI configuration only.
		return Request{ args: args[3 + count..].clone(), module: parts[1], is_test: parts[2] == 'test', cwd: args[0] }
	}
	has_entry := parts[0] in [record_format, input_record_format, entry_record_format]
	if has_entry && args.len < 4 { return error('missing program entry') }
	has_inputs := parts[0] in [record_format, input_record_format]
	if has_inputs && (args.len < 6 || args[4] !in ['true', 'false']) {
		return error('invalid test display configuration')
	}
	if parts[0] == record_format && args.len < 7 { return error('missing entry source identities') }
	arg_offset := if parts[0] == record_format {
		7
	} else if has_inputs {
		6
	} else if has_entry {
		4
	} else {
		3
	}
	return Request{
		args:            args[arg_offset..].clone()
		module:          parts[1]
		is_test:         parts[2] == 'test'
		cwd:             args[0]
		project_flags:   decode_project_flags(args[1])!
		imports:         decode_arguments(args[2])!
		entry:           if has_entry { args[3] } else { '' }
		show_test_stats: has_inputs && args[4] == 'true'
		compile_inputs:  if has_inputs {
			decode_arguments(args[5])!
		} else {
			[]string{}
		}
		entry_sources:   if parts[0] == record_format {
			decode_entry_sources(args[6])!
		} else {
			map[string]string{}
		}
	}
}

fn encode_entry_sources(sources map[string]string) string {
	mut paths := sources.keys()
	paths.sort()
	mut args := []string{}
	for path in paths { args << [path, sources[path]] }
	return encode_arguments(args)
}

fn decode_entry_sources(content string) !map[string]string {
	args := decode_arguments(content)!
	if args.len % 2 != 0 { return error('invalid entry source identities') }
	mut sources := map[string]string{}
	for i := 0; i < args.len; i += 2 {
		signature := args[i + 1]
		hash := signature.all_after(':')
		if !os.is_abs_path(args[i]) || (!signature.starts_with('file:') && !signature.starts_with('dir:'))
			|| hash.len == 0 || hash.len > 16 || !hash.bytes().all(it.is_hex_digit()) {
			return error('invalid entry source identity')
		}
		sources[args[i]] = signature
	}
	return sources
}

fn entry_source_signature(path string) !string {
	if os.is_dir(path) {
		mut files := os.walk_ext(path, '.v')
		files.sort()
		mut inputs := []string{}
		for file in files { inputs << [file, hash_text(os.read_file(file)!)] }
		return 'dir:${hash_text(encode_arguments(inputs))}'
	}
	return 'file:${hash_text(os.read_file(path)!)}'
}

fn snapshot_entry_sources(vlib string, entry string, args []string, cwd string, modules map[string][]string) !map[string]string {
	mut sources := map[string]string{}
	if entry == '' { return sources }
	mut paths := [os.real_path(entry)]
	index := args.index('-file-list')
	if index >= 0 && index + 1 < args.len {
		for input in args[index + 1].split(',') {
			paths << os.real_path(if os.is_abs_path(input) {
				input
			} else {
				os.join_path(cwd, input)
			})
		}
	}
	for _, files in modules {
		for file in files {
			path := os.real_path(file)
			// New source files in a project import can introduce new dependencies.
			if !path.starts_with(vlib + os.path_separator) { paths << os.dir(path) }
		}
	}
	for path in paths {
		if path !in sources { sources[path] = entry_source_signature(path)! }
	}
	return sources
}

fn entry_sources_unchanged(request Request) bool {
	if request.entry == '' { return true }
	// Older live records cannot identify the source/configuration pair safely.
	if request.entry_sources.len == 0 { return false }
	for path, signature in request.entry_sources {
		if (entry_source_signature(path) or { return false }) != signature { return false }
	}
	return true
}

fn decode_arguments(content string) ![]string {
	mut args := []string{}
	mut rest := content
	for rest.len > 0 {
		end := rest.index('\n') or { return error('missing argument length') }
		len := strconv.atoi(rest[..end])!
		rest = rest[end + 1..]
		if len < 0 || len > rest.len { return error('invalid argument length') }
		args << rest[..len]
		rest = rest[len..]
	}
	return args
}

// encode_project_flags serializes project directives for inventory and refresh children.
pub fn encode_project_flags(groups []FlagGroup) string {
	mut args := []string{}
	for group in groups {
		// Group identities contain NUL separators, which environment values cannot carry.
		args << [group.at_start.str(), group.in_main.str(), group.identity.bytes().hex(),
			group.flags.len.str()]
		args << group.flags
	}
	return encode_arguments(args)
}

// decode_project_flags restores ordered project directives from a refresh record.
pub fn decode_project_flags(content string) ![]FlagGroup {
	args := decode_arguments(content)!
	mut groups := []FlagGroup{}
	mut i := 0
	for i < args.len {
		if args.len - i < 4 || args[i] !in ['true', 'false'] || args[i + 1] !in ['true', 'false'] {
			return error('invalid project flag group')
		}
		count := strconv.atoi(args[i + 3])!
		if count < 0 || count > args.len - i - 4 { return error('invalid project flag count') }
		groups << FlagGroup{
			flags:    args[i + 4..i + 4 + count].clone()
			at_start: args[i] == 'true'
			in_main:  args[i + 1] == 'true'
			identity: hex.decode(args[i + 2])!.bytestr()
		}
		i += 4 + count
	}
	return groups
}

// record remembers only modules resolved to this checkout's vlib. Each module
// and flag set has its own atomic record, so concurrent programs cannot lose
// one another's inventories and C-flag variants survive in the same cache.
// The caller's directory preserves relative compilation inputs during refresh.
// project_flags preserves project directives without freezing the vlib's flags.
// Import order is retained because it determines the order of module C directives.
// entry identifies the original source input for project interface scopes.
// Its source identities retire configurations after the project input changes.
// show_test_stats preserves the effective test assertion display mode.
// Cwd-independent builds share records across projects; required compile inputs
// and explicit -file-list files are tracked so their removal retires the obsolete configuration.
pub fn record(dir string, vroot string, args []string, is_test bool, sources map[string][]string, project_flags []FlagGroup, entry string, show_test_stats bool) ! {
	vlib := os.real_path(os.join_path(vroot, 'vlib'))
	inputs := required_compile_inputs(args, project_flags, os.getwd())
	cwd := if entry != '' || flags_need_cwd(args, project_flags) { os.getwd() } else { '' }
	entry_sources := snapshot_entry_sources(vlib, entry, args, cwd, sources)!
	mut modules := []string{}
	for _, files in sources {
		if files.len == 0 { continue }
		source_dir := os.dir(os.real_path(files[0]))
		if !source_dir.starts_with(vlib + os.path_separator) { continue }
		name := source_dir[vlib.len + 1..].replace(os.path_separator, '.')
		if valid_module(name) && name !in modules { modules << name }
	}
	for name in modules {
		request := Request{
			args:            args
			module:          name
			is_test:         is_test
			cwd:             cwd
			project_flags:   project_flags
			imports:         modules.filter(it != 'builtin')
			entry:           entry
			show_test_stats: show_test_stats
			compile_inputs:  inputs
			entry_sources:   entry_sources
		}
		content := encode_request(request)
		path := os.join_path(dir, '${record_prefix}${hash_text(content)}')
		if os.exists(path) {
			continue
		}
		tmp := '${path}.${tempname.unique_token()}'
		os.write_file(tmp, content)!
		os.mv(tmp, path) or {
			os.rm(tmp) or {}
			return err
		}
	}
}

fn valid_module(name string) bool {
	return name.len > 0 && name.split('.').all(it.len > 0
		&& (it[0].is_letter() || it[0] == `_`)
		&& it.bytes().all(it.is_letter() || it.is_digit() || it == `_`))
}

fn compiler_available(vroot string, request Request) bool {
	index := request.args.index('-cc')
	if index < 0 { return true }
	if index + 1 >= request.args.len { return false }
	compiler := request.args[index + 1]
	if os.is_abs_path(compiler) || compiler.contains('/') || compiler.contains('\\') {
		path := if os.is_abs_path(compiler) || request.cwd == '' {
			compiler
		} else {
			os.join_path(request.cwd, compiler)
		}
		return os.is_file(path) && os.is_executable(path)
	}

	if compiler in ['tcc', 'tinyc'] && os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) {
		return true
	}
	os.find_abs_path_of_executable(compiler) or { return false }
	return true
}

fn required_compile_inputs(args []string, project []FlagGroup, cwd string) []string {
	mut flags := []string{}
	mut inputs := []string{}
	for group in project { flags << group.flags }
	mut i := 0
	for i < args.len {
		if args[i] == '-cflags' && i + 1 < args.len {
			flags << cmdexec.split_args(args[i + 1]) or { []string{} }
			i += 2
		} else if args[i] == '-file-list' && i + 1 < args.len {
			for file in args[i + 1].split(',') {
				path := if os.is_abs_path(file) {
					file
				} else {
					os.abs_path(os.join_path(cwd, file))
				}
				if os.is_file(path) && path !in inputs { inputs << path }
			}
			i += 2
		} else {
			i++
		}
	}
	mut dirs := [cwd]
	mut files := []string{}
	mut expected := ''
	for flag in flags {
		mut option := expected
		mut path := flag
		expected = ''
		if option == '' {
			for candidate in ['-include-pch', '-include-pth', '--include-directory', '--imacros',
				'-imacros', '--include', '-include', '-isystem', '-iquote', '-idirafter', '-I'] {
				if flag == candidate {
					expected = candidate
					break
				}
				if flag.starts_with(candidate) {
					option = candidate
					path = flag[candidate.len..]
					if candidate.starts_with('--') { path = path.trim_left('=') }
					break
				}
			}
		}
		if option == '' || path == '' { continue }
		if option in ['-I', '-isystem', '-iquote', '-idirafter', '--include-directory'] {
			dirs << if os.is_abs_path(path) { path } else { os.abs_path(os.join_path(cwd, path)) }
		} else {
			files << path
		}
	}
	for file in files {
		for dir in dirs {
			path := if os.is_abs_path(file) { file } else { os.abs_path(os.join_path(dir, file)) }
			if os.is_file(path) {
				if path !in inputs { inputs << path }
				break
			}
		}
	}
	return inputs
}

fn listed_sources_available(request Request) bool {
	index := request.args.index('-file-list')
	if index < 0 { return true }
	if index + 1 >= request.args.len { return false }
	for input in request.args[index + 1].split(',') {
		path := if os.is_abs_path(input) { input } else { os.join_path(request.cwd, input) }
		if !os.is_file(path) && !os.is_dir(path) { return false }
	}
	return true
}

// collect snapshots vexe's inventory before `v up` replaces the compiler. Old
// caches predate these records: their header filenames still identify vlib
// source directories, so warm those modules for the available C compilers in
// both normal and production mode, for programs and tests. Successfully migrated
// legacy headers are excluded from subsequent snapshots.
// Unreadable and unsupported inventory artifacts are skipped, as are stale live sources.
pub fn collect(vroot string, vexe string) ![]Request {
	root := root_dir(vroot, vexe)
	if !os.is_dir(root) {
		return []
	}
	mut requests := []Request{}
	mut seen := map[string]int{}
	mut legacy_headers := []string{}
	mut configs := os.ls(root) or { return [] }
	configs.sort()
	for config in configs {
		dir := os.join_path(root, config)
		if !os.is_dir(dir) {
			continue
		}
		mut entries := os.ls(dir) or { continue }
		entries.sort()
		mut recorded := map[string]bool{}
		for entry in entries {
			if !entry.starts_with(record_prefix) || entry[record_prefix.len..].contains('.') {
				continue
			}
			path := os.join_path(dir, entry)
			content := os.read_file(path) or { continue }
			request := decode_request(content) or { continue }
			recorded[request.module] = true
			if request.cwd != '' && !os.is_dir(request.cwd) { continue }
			if request.compile_inputs.any(!os.is_file(it)) { continue }
			if !listed_sources_available(request) { continue }
			if !entry_sources_unchanged(request) { continue }
			key := encode_request(request)
			if key !in seen {
				seen[key] = requests.len
				requests << request
			}
		}
		// A configuration written before inventory records existed may gain
		// records for only some modules when another program uses it.
		mut config_headers := entries.filter(it.ends_with('.vh'))
		if recorded.len > 0 {
			// Source-root hashes disambiguate dotted names and builtin submodules.
			for name in recorded.keys() {
				suffix := '_${hash_text(os.real_path(os.join_path(vroot, 'vlib', name.replace('.', os.path_separator))))}.vh'
				config_headers = config_headers.filter(!it.ends_with(suffix))
			}
		}
		for header in config_headers {
			path := os.join_path(dir, header)
			signature := hash_text(os.read_file(path) or { continue })
			if (os.read_file('${path}.vup_migrated') or { '' }) != signature {
				legacy_headers << path
			}
		}
	}
	if legacy_headers.len == 0 {
		return requests
	}
	vlib := os.real_path(os.join_path(vroot, 'vlib'))
	mut legacy_modules := map[string][]string{}
	for file in os.walk_ext(vlib, '.v') {
		dir := os.dir(file)
		if dir == vlib { continue }
		suffix := '_${hash_text(os.real_path(dir))}.vh'
		if !legacy_headers.any(it.ends_with(suffix)) {
			continue
		}
		name := dir[vlib.len + 1..].replace(os.path_separator, '.')
		if valid_module(name) {
			legacy_modules[name] = legacy_headers.filter(it.ends_with(suffix))
		}
	}
	mut modules := legacy_modules.keys()
	modules.sort()
	mut compilers := ['cc', 'clang', 'gcc', 'tcc']
	if compiler := os.getenv_opt('CC') {
		if compiler !in compilers { compilers << compiler }
	}
	for compiler in compilers {
		if compiler == 'tcc' {
			if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) {
				continue
			}
		} else {
			os.find_abs_path_of_executable(compiler) or { continue }
		}
		for prod in [false, true] {
			mut flags := ['-cc', compiler]
			if prod { flags << '-prod' }
			for name in modules {
				mut headers := map[string]string{}
				for path in legacy_modules[name] {
					headers[path] = hash_text(os.read_file(path) or { continue })
				}
				for is_test in [false, true] {
					request := Request{ args: flags, module: name, is_test: is_test, legacy_headers: headers }
					key := encode_request(request)
					if index := seen[key] {
						mut merged := requests[index].legacy_headers.clone()
						for path, signature in headers { merged[path] = signature }
						requests[index] = Request{ ...requests[index], legacy_headers: merged }
					} else {
						seen[key] = requests.len
						requests << request
					}
				}
			}
		}
	}
	return requests
}

// refresh asks the updated compiler to validate and rebuild each inventoried
// module with its original flags and working directory. Compiler validation also
// covers transitive imports, headers, embedded files and compiler changes. Unchanged entries are
// reused, and each newly compiled module is announced on streamed stdout.
// Equivalent synthetic import sequences and live entry commands run once per configuration.
// Both stop after module object validation, before compiling or linking a program unit.
// Object compilation failures are reported even when an ordinary build could fall back.
// Progress identifies configurations producing output, their compiler flags and build modes.
// Successful unchanged configurations are silent unless verbose reporting is requested.
// Records with vanished working directories, required inputs or compilers are skipped.
// Live entries whose project source identity changed are skipped until rebuilt and recorded.
// Legacy headers are marked migrated only after successful refreshes of their variants.
// Disk-full failures stop remaining builds; child output is forwarded without buffering it all.
// TinyCC production builds are never refreshed: the compiler refuses `-cc tcc` with `-prod`,
// and records made before that rule still exist.
pub fn refresh(vexe string, all_requests []Request, options RefreshOptions) ! {
	requests := all_requests.filter(!request_uses_prod_tcc(it))
	if requests.len == 0 { return }
	dir := os.join_path(os.vtmp_dir(), 'vup_cache_refresh_${tempname.unique_token()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('CFLAGS')
	environment.delete('LDFLAGS')
	environment.delete('V3_CACHE_FORCE_SOURCE')
	environment.delete('V3_CACHE_REFRESH_OBJECT_FLAGS')
	environment.delete('V3_CACHE_REFRESH_PROJECT_FLAGS')
	environment.delete('V3_CACHE_REFRESH_ENTRY')
	environment.delete('VTEST_ONLY_FN')
	environment.delete('VTEST_SHOW_ASSERTS')
	environment['VEXE'] = vexe
	environment['V3_CACHE_REFRESH'] = '1'
	environment['V_SKIP_VVMRC'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	mut failures := []string{}
	mut refreshed_entries := map[string]bool{}
	mut migrated_headers := map[string]string{}
	mut incomplete_headers := map[string]bool{}
	mut checked_configurations := 0
	mut reported_progress := false
	for index, request in requests {
		if !valid_module(request.module) { return error('invalid module ${request.module}') }
		for path, signature in request.legacy_headers { migrated_headers[path] = signature }
		if (request.cwd != '' && !os.is_dir(request.cwd))
			|| request.compile_inputs.any(!os.is_file(it))
			|| !listed_sources_available(request)
			|| !entry_sources_unchanged(request)
			|| !compiler_available(os.dir(vexe), request) {
			for path in request.legacy_headers.keys() { incomplete_headers[path] = true }
			continue
		}
		// An upstream removal leaves no module to warm. The compiler's source
		// validation already makes its old objects unusable.
		module_dir := os.join_path(os.dir(vexe), 'vlib', request.module.replace('.', os.path_separator))
		if !os.is_dir(module_dir) {
			for path in request.legacy_headers.keys() { incomplete_headers[path] = true }
			continue
		}
		use_entry := request.entry != '' && os.exists(request.entry)
		if use_entry {
			// The current entry supplies directives and imports; their historical
			// inventory values do not change the live child's command or environment.
			key := encode_arguments([request.cwd, request.entry, request.show_test_stats.str(),
				...request.args])
			if key in refreshed_entries { continue }
			refreshed_entries[key] = true
		}
		name := if request.module == 'builtin' { 'strconv' } else { request.module }
		mut import_names := []string{}
		for module_name in request.imports {
			if !valid_module(module_name) { return error('invalid module ${module_name}') }
			if module_name == 'builtin' { continue }
			if os.is_dir(os.join_path(os.dir(vexe), 'vlib', module_name.replace('.', os.path_separator))) {
				if module_name !in import_names { import_names << module_name }
			}
		}
		// Builtin is implicit in every child. A builtin-only request still imports
		// strconv to warm the shared core object, as before.
		if (request.module != 'builtin' || import_names.len == 0) && name !in import_names {
			import_names.prepend(name)
		}
		// Keep the recorded order, including the target's original position:
		// imported #flag groups can contain ordered include paths and definitions.
		if !use_entry {
			key := encode_arguments([request.cwd, request.is_test.str(), request.show_test_stats.str(),
				encode_project_flags(request.project_flags), encode_arguments(import_names),
				...request.args])
			if key in refreshed_entries { continue }
			refreshed_entries[key] = true
		}
		imports := import_names.map('import ${it} as _\n').join('')
		source := if use_entry {
			request.entry
		} else {
			os.join_path(dir, if request.is_test { 'refresh_test.v' } else { 'refresh.v' })
		}
		body := if request.is_test { 'fn test_cache_refresh() {}' } else { 'fn main() {}' }
		if !use_entry { os.write_file(source, 'module main\n${imports}${body}\n')! }
		mut command := request.args.clone()
		if !use_entry {
			listed := command.index('-file-list')
			if listed >= 0 && listed + 1 < command.len { command.delete_many(listed, 2) }
		}
		// A live entry supplies current directives and project implementers. Only
		// cache its module objects: project linker inputs are deliberately omitted.
		environment['V3_CACHE_REFRESH_ENTRY'] = if use_entry { '1' } else { '' }
		environment['V3_CACHE_REFRESH_PROJECT_FLAGS'] = if use_entry {
			''
		} else {
			encode_project_flags(request.project_flags)
		}
		if request.show_test_stats {
			environment['VTEST_SHOW_ASSERTS'] = '1'
		} else {
			environment.delete('VTEST_SHOW_ASSERTS')
		}
		// CLI parsing trims selectors, while VTEST_ONLY_FN preserves the effective
		// patterns from vtest. Replay them through the same environment path.
		selector := command.index('-run-only')
		if selector >= 0 && selector + 1 < command.len {
			environment['VTEST_ONLY_FN'] = command[selector + 1]
			command.delete_many(selector, 2)
		} else {
			environment.delete('VTEST_ONLY_FN')
		}
		command << ['-skip-running', '-o', os.join_path(dir, 'refresh'), source]
		progress := if options.verbose {
			'> Refreshing module cache [${index + 1}/${requests.len}] (${refresh_configuration(request)}).'
		} else {
			'> Refreshing module cache (${refresh_configuration(request)}).'
		}
		checked_configurations++
		code, disk_full, reported := run_refresh_command(vexe, command, request.cwd, environment,
			progress, options.verbose)
		reported_progress = reported_progress || reported
		if code != 0 {
			failures << '${request.module} (${request.args.join(' ')})'
			eprintln('> Module cache refresh failed [${index + 1}/${requests.len}] (${refresh_configuration(request)}; exit ${code}).')
			if disk_full {
				return error(failures.join(', ') + '; disk is full; cache refresh stopped. Free disk space and run v up again')
			}
		}
	}
	if failures.len > 0 { return error(failures.join(', ')) }
	for path, signature in migrated_headers {
		if path in incomplete_headers || !os.is_file(path) { continue }
		// Do not retire a header rewritten while the migration was in progress.
		if hash_text(os.read_file(path)!) != signature { continue }
		marker := '${path}.vup_migrated'
		tmp := '${marker}.${tempname.unique_token()}'
		os.write_file(tmp, signature)!
		os.mv(tmp, marker) or {
			os.rm(tmp) or {}
			return err
		}
	}
	if checked_configurations > 0 && reported_progress {
		println('> Module cache refresh complete: ${checked_configurations} build configurations checked.')
	}
}

fn run_refresh_command(vexe string, command []string, cwd string, environment map[string]string, progress string, verbose bool) (int, bool, bool) {
	mut reported_progress := verbose
	if reported_progress {
		println(progress)
		flush_stdout()
	}
	mut process := os.new_process(vexe)
	process.set_args(command)
	if cwd != '' { process.set_work_folder(cwd) }
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	mut disk_full := false
	mut tails := ['', '']
	for {
		alive := process.is_alive()
		chunks := [process.stdout_read(), process.stderr_read()]
		for index, chunk in chunks {
			if chunk == '' { continue }
			if !reported_progress {
				// Print context before the first child output without buffering the build log.
				println(progress)
				flush_stdout()
				reported_progress = true
			}
			if index == 0 {
				print(chunk)
				flush_stdout()
			} else {
				eprint(chunk)
				flush_stderr()
			}
			text := tails[index] + chunk
			disk_full = disk_full || text.contains('No space left on device')
				|| text.contains('There is not enough space on the disk')
			tails[index] = if text.len > 128 { text[text.len - 128..] } else { text }
		}
		if !alive && chunks.all(it == '') { break }
		if chunks.all(it == '') { time.sleep(5 * time.millisecond) }
	}
	process.wait()
	code := process.code
	process.close()
	return code, disk_full, reported_progress
}

fn refresh_configuration(request Request) string {
	mode := if '-prod' in request.args { 'production' } else { 'normal' }
	kind := if request.is_test { 'tests' } else { 'program' }
	mut flags := []string{}
	for arg in request.args {
		text := arg.replace('\r', '\\r').replace('\n', '\\n').replace('\t', '\\t')
		flags << if text == '' || text.contains(' ') { os.quoted_path(text) } else { text }
	}
	options := if flags.len > 0 { flags.join(' ') } else { 'default compiler flags' }
	return '${mode} ${kind}; ${options}'
}

fn flags_need_cwd(args []string, project []FlagGroup) bool {
	mut flags := []string{}
	for group in project { flags << group.flags }
	for i, arg in args {
		if arg == '-cflags' && i + 1 < args.len {
			flags << cmdexec.split_args(args[i + 1]) or { []string{} }
		}
	}
	mut expected := false
	for flag in flags {
		if expected {
			if !os.is_abs_path(flag) { return true }
			expected = false
			continue
		}
		for option in ['-include-pch', '-include-pth', '--include-directory', '--imacros', '-imacros',
			'--include', '-include', '-isystem', '-iquote', '-idirafter', '-iframework', '--sysroot',
			'-isysroot', '-I', '-F'] {
			if flag == option {
				expected = true
				break
			}
			if flag.starts_with(option) {
				if !os.is_abs_path(flag[option.len..].trim_left('=')) { return true }
				break
			}
		}
		if !flag.starts_with('-') && flag.ends_with('.c') && !os.is_abs_path(flag) { return true }
	}
	return false
}
