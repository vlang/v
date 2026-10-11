module cache_refresh

import os
import strconv
import strings
import v.tempname

const record_prefix = '.vup_refresh_'
const record_format = 'vup-cache-refresh-3'
const cwd_record_format = 'vup-cache-refresh-2'
const legacy_record_format = 'vup-cache-refresh-1'

// Request describes how to rebuild one previously cached standard-library module.
pub struct Request {
pub:
	args             []string
	module           string
	is_test          bool
	cwd              string   // Original directory for caller-relative compilation inputs.
	object_flags     []string // Effective flags passed to module-object compilation.
	has_object_flags bool     // Older records contain only CLI options.
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
	// Length prefixes preserve spaces, quoting and newlines in C flag values.
	for arg in [request.cwd, request.has_object_flags.str(), request.object_flags.len.str(),
		...request.object_flags, ...request.args] {
		out.writeln(arg.len.str())
		out.write_string(arg)
	}
	return out.str()
}

fn decode_request(content string) !Request {
	parts := content.split_nth('\n', 4)
	if parts.len != 4 || parts[0] !in [record_format, cwd_record_format, legacy_record_format] || !valid_module(parts[1])
		|| parts[2] !in ['test', 'program'] {
		return error('invalid module cache refresh record')
	}
	mut args := []string{}
	mut rest := parts[3]
	for rest.len > 0 {
		end := rest.index('\n') or { return error('missing argument length') }
		len := strconv.atoi(rest[..end])!
		rest = rest[end + 1..]
		if len < 0 || len > rest.len { return error('invalid argument length') }
		args << rest[..len]
		rest = rest[len..]
	}
	if parts[0] == legacy_record_format {
		return Request{ args: args, module: parts[1], is_test: parts[2] == 'test' }
	}
	if args.len == 0 { return error('missing working directory') }
	if parts[0] == cwd_record_format {
		return Request{ args: args[1..].clone(), module: parts[1], is_test: parts[2] == 'test', cwd: args[0] }
	}
	if args.len < 3 || args[1] !in ['true', 'false'] { return error('missing object flags') }
	count := strconv.atoi(args[2])!
	if count < 0 || count > args.len - 3 { return error('invalid object flag count') }
	return Request{
		args:             args[3 + count..].clone()
		module:           parts[1]
		is_test:          parts[2] == 'test'
		cwd:              args[0]
		object_flags:     args[3..3 + count].clone()
		has_object_flags: args[1] == 'true'
	}
}

// record remembers only modules resolved to this checkout's vlib. Each module
// and flag set has its own atomic record, so concurrent programs cannot lose
// one another's inventories and C-flag variants survive in the same cache.
// The caller's directory preserves relative compilation inputs during refresh.
// object_flags includes source directives after filtering out linker-only operands.
pub fn record(dir string, vroot string, args []string, is_test bool, sources map[string][]string, object_flags []string) ! {
	vlib := os.real_path(os.join_path(vroot, 'vlib'))
	for _, files in sources {
		if files.len == 0 {
			continue
		}
		source_dir := os.dir(os.real_path(files[0]))
		if !source_dir.starts_with(vlib + os.path_separator) {
			continue
		}
		name := source_dir[vlib.len + 1..].replace(os.path_separator, '.')
		if !valid_module(name) {
			continue
		}
		request := Request{
			args:             args
			module:           name
			is_test:          is_test
			cwd:              os.getwd()
			object_flags:     object_flags
			has_object_flags: true
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

// collect snapshots vexe's inventory before `v up` replaces the compiler. Old
// caches predate these records: their header filenames still identify vlib
// source directories, so warm those modules for the available C compilers in
// both normal and production mode.
pub fn collect(vroot string, vexe string) ![]Request {
	root := root_dir(vroot, vexe)
	if !os.is_dir(root) {
		return []
	}
	mut requests := []Request{}
	mut seen := map[string]bool{}
	mut legacy_headers := []string{}
	mut configs := os.ls(root)!
	configs.sort()
	for config in configs {
		dir := os.join_path(root, config)
		if !os.is_dir(dir) {
			continue
		}
		mut entries := os.ls(dir)!
		entries.sort()
		mut recorded := map[string]bool{}
		for entry in entries {
			if !entry.starts_with(record_prefix) || entry[record_prefix.len..].contains('.') {
				continue
			}
			path := os.join_path(dir, entry)
			request := decode_request(os.read_file(path)!)!
			if !valid_module(request.module) {
				return error('invalid module in ${path}')
			}
			recorded[request.module] = true
			if request.cwd != '' && !os.is_dir(request.cwd) { continue }
			key := encode_request(request)
			if key !in seen {
				seen[key] = true
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
		legacy_headers << config_headers
	}
	if legacy_headers.len == 0 {
		return requests
	}
	vlib := os.real_path(os.join_path(vroot, 'vlib'))
	mut legacy_modules := map[string]bool{}
	for file in os.walk_ext(vlib, '.v') {
		dir := os.dir(file)
		if dir == vlib { continue }
		suffix := '_${hash_text(os.real_path(dir))}.vh'
		if !legacy_headers.any(it.ends_with(suffix)) {
			continue
		}
		name := dir[vlib.len + 1..].replace(os.path_separator, '.')
		if valid_module(name) {
			legacy_modules[name] = true
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
				request := Request{ args: flags, module: name }
				key := encode_request(request)
				if key !in seen {
					seen[key] = true
					requests << request
				}
			}
		}
	}
	return requests
}

// refresh asks the updated compiler to validate and rebuild each inventoried
// module with its original flags and working directory. Compiler validation also
// covers transitive imports, headers, embedded files and compiler changes. Unchanged entries are
// reused, and each newly compiled module is announced on inherited stdout.
// Records whose original working directory no longer exists are skipped.
pub fn refresh(vexe string, requests []Request) ! {
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
	environment.delete('VTEST_ONLY_FN')
	environment['VEXE'] = vexe
	environment['V3_CACHE_REFRESH'] = '1'
	environment['V_SKIP_VVMRC'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	$if windows {
		// os.system_args preserves redirected stdout on Windows, unlike a
		// Process started without explicit standard handles. Give it the same
		// environment as the Unix child, and restore the caller's afterwards.
		previous := os.environ()
		names := ['VFLAGS', 'CFLAGS', 'LDFLAGS', 'V3_CACHE_FORCE_SOURCE',
			'V3_CACHE_REFRESH_OBJECT_FLAGS', 'VTEST_ONLY_FN', 'VEXE', 'V3_CACHE_REFRESH', 'V_SKIP_VVMRC',
			'V_MACOS_V3_NO_FALLBACK']
		for name in names {
			if name in environment {
				os.setenv(name, environment[name], true)
			} else {
				os.unsetenv(name)
			}
		}
		defer {
			for name in names {
				if name in previous {
					os.setenv(name, previous[name], true)
				} else {
					os.unsetenv(name)
				}
			}
		}
	}
	mut failures := []string{}
	for request in requests {
		if !valid_module(request.module) { return error('invalid module ${request.module}') }
		if request.cwd != '' && !os.is_dir(request.cwd) { continue }
		// An upstream removal leaves no module to warm. The compiler's source
		// validation already makes its old objects unusable.
		module_dir := os.join_path(os.dir(vexe), 'vlib', request.module.replace('.', os.path_separator))
		if !os.is_dir(module_dir) { continue }
		name := if request.module == 'builtin' { 'strconv' } else { request.module }
		source := os.join_path(dir, if request.is_test { 'refresh_test.v' } else { 'refresh.v' })
		body := if request.is_test { 'fn test_cache_refresh() {}' } else { 'fn main() {}' }
		os.write_file(source, 'module main\nimport ${name} as _\n${body}\n')!
		mut command := request.args.clone()
		if request.has_object_flags {
			// Quote argv tokens for the compiler's parser, without invoking a shell.
			environment['V3_CACHE_REFRESH_OBJECT_FLAGS'] = request.object_flags.map('"' +
				it.replace('\\', '\\\\').replace('"', '\\"') + '"').join(' ')
		} else {
			environment.delete('V3_CACHE_REFRESH_OBJECT_FLAGS')
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
		command << ['-usecache', '-skip-running', '-o', os.join_path(dir, 'refresh'), source]
		mut code := -1
		$if windows {
			if value := environment['V3_CACHE_REFRESH_OBJECT_FLAGS'] {
				os.setenv('V3_CACHE_REFRESH_OBJECT_FLAGS', value, true)
			} else {
				os.unsetenv('V3_CACHE_REFRESH_OBJECT_FLAGS')
			}
			if value := environment['VTEST_ONLY_FN'] {
				os.setenv('VTEST_ONLY_FN', value, true)
			} else {
				os.unsetenv('VTEST_ONLY_FN')
			}
			previous_dir := os.getwd()
			if request.cwd != '' { os.chdir(request.cwd)! }
			code = os.system_args([vexe, ...command])
			if request.cwd != '' { os.chdir(previous_dir)! }
		} $else {
			mut process := os.new_process(vexe)
			process.set_args(command)
			if request.cwd != '' { process.set_work_folder(request.cwd) }
			process.set_environment(environment)
			process.wait()
			code = process.code
			process.close()
		}
		if code != 0 {
			failures << '${request.module} (${request.args.join(' ')})'
		}
	}
	if failures.len > 0 { return error(failures.join(', ')) }
}
