module main

import os

// vpbgen generates V protobuf codecs and gRPC service declarations from .proto
// schemas. Run `v help pbgen` for the options, or see cmd/tools/vpbgen/README.md.

fn main() {
	opts := parse_options(os.args[1..]) or {
		eprintln(err.msg())
		exit(1)
	}
	if opts.help {
		println(usage())
		return
	}
	files := load_schemas(opts) or {
		eprintln(err.msg())
		exit(1)
	}
	res := resolve_files(files, opts.module_name) or {
		eprintln(err.msg())
		exit(1)
	}
	// A schema the generator did not fully understand would produce a codec that
	// disagrees with its producer, and nothing downstream would catch it. So it
	// refuses to emit rather than emit something plausible.
	if res.errors.len > 0 {
		for e in res.errors {
			eprintln(e)
		}
		exit(1)
	}
	// Checked before `-check` reports success, because `-check` exists to answer
	// "will this generate something usable", and a name that cannot be used is
	// not usable.
	check_module_name(res.module, res) or {
		eprintln(err.msg())
		exit(1)
	}
	if opts.verbose {
		report(res)
	}
	if opts.check {
		println('pbgen: ${files.len} file(s) resolved, ${res.messages.len} message(s), ${res.enums.len} enum(s), ${res.services.len} service(s); nothing written (-check)')
		return
	}
	write_outputs(opts, res) or {
		eprintln(err.msg())
		exit(1)
	}
}

// load_schemas parses the input files and everything they import.
//
// Imports are followed rather than ignored: a schema that refers to a type
// declared in another file cannot be resolved without reading that file, and
// emitting a codec for a type the generator never saw would be worse than
// failing.
fn load_schemas(opts PbgenOptions) ![]File {
	mut files := []File{}
	mut seen := map[string]bool{}
	for input in opts.inputs {
		load_one(mut files, mut seen, input, opts, 0)!
	}
	return files
}

// max_import_depth bounds import following, so an import cycle reports an error
// instead of recursing until the stack runs out.
const max_import_depth = 32

// load_one parses `path` and, recursively, its imports.
fn load_one(mut files []File, mut seen map[string]bool, path string, opts PbgenOptions, depth int) ! {
	abs := os.abs_path(path)
	if seen[abs] {
		return
	}
	if depth > max_import_depth {
		return error('pbgen: imports nested more than ${max_import_depth} deep at ${path}; is there a cycle?')
	}
	if !os.exists(path) {
		return error('pbgen: no such file: ${path}')
	}
	mut file := parse_file(path)!
	seen[abs] = true
	// A well-known import is not resolvable without the well-known-types
	// schemas, which are not bundled. It is reported rather than silently
	// dropped, because a field of a well-known type would then be emitted as an
	// unknown message.
	for imp in file.imports {
		if is_well_known(imp.path) {
			return error('pbgen: ${path} imports the well-known type `${imp.path}`, which this generator does not bundle. Handle that type by hand, or remove the dependency.')
		}
	}
	for imp in file.imports {
		imported := resolve_import_path(imp.path, path)
		load_one(mut files, mut seen, imported, opts, depth + 1)!
	}
	files << file
	return
}

// is_well_known reports whether `path` names one of the schemas protoc ships
// under google/protobuf, which are compiled into every runtime rather than
// carried by a project.
pub fn is_well_known(path string) bool {
	return path.starts_with('google/protobuf/')
}

// resolve_import_path resolves an import against the importing file's directory,
// falling back to the process's working directory. An import path is always
// relative to the root of the import tree in the spec, but resolving against the
// importing file is what makes a schema work from a subdirectory, which is how
// most projects lay them out.
pub fn resolve_import_path(import_path string, from_file string) string {
	dir := os.dir(from_file)
	joined := os.join_path(dir, os.from_slash(import_path))
	if os.exists(joined) {
		return joined
	}
	converted := os.from_slash(import_path)
	if os.exists(converted) {
		return converted
	}
	return joined
}

// report prints what was resolved, for `-v`.
fn report(res &ResolvedFile) {
	println('pbgen: module `${res.module}`')
	for en in res.enums {
		println('  enum ${en.v_name} (${en.values.len} value(s))')
	}
	for m in res.messages {
		println('  message ${m.v_name} (${m.fields.len} field(s))')
		for f in m.fields {
			println('    ${f.number} ${f.name} ${f.v_type} [${f.kind}]${if f.label == .repeated {
				' repeated'
			} else {
				''
			}}')
		}
	}
	for s in res.services {
		println('  service ${s.name} (${s.rpcs.len} method(s))')
		for r in s.rpcs {
			println('    ${r.name} ${r.request_type} -> ${r.response_type}')
		}
	}
}

// write_outputs writes the codec and, if asked, the service declarations.
fn write_outputs(opts PbgenOptions, res &ResolvedFile) ! {
	codec := emit_codec(res)
	if opts.codec_out == '' {
		print(codec)
	} else {
		write_generated(opts.codec_out, codec)!
		if opts.verbose {
			println('pbgen: wrote ${opts.codec_out}')
		}
	}
	// The service output is only written when there is a service to describe and
	// a place to put it, so a schema with no service does not leave an empty file
	// behind.
	if res.services.len == 0 {
		return
	}
	grpc_source := emit_grpc(res)
	// The service half defaults to the codec file, so the common case is one file
	// holding both.
	mut target := opts.grpc_out
	if target == '' {
		target = opts.codec_out
	}
	if target == '' {
		// Both would go to stdout, and two files cannot share it. Appending is
		// the only way to keep one invocation useful.
		print(grpc_source)
		return
	}
	if target == opts.codec_out && opts.codec_out != '' {
		// One file holds both halves, which is the common case and what the
		// gRPC example does. The service half is emitted without its own
		// `module` line, since a file has exactly one.
		write_generated(target, codec + '\n' + emit_grpc_body(res))!
	} else {
		write_generated(target, grpc_source)!
	}
	if opts.verbose {
		println('pbgen: wrote ${target}')
	}
}

// write_generated writes `content` to `path`, creating the directory if needed
// and refusing to overwrite a file that is not marked as generated.
//
// Overwriting is the one destructive thing a code generator does, so a target
// that exists and lacks the "Code generated by" header is reported instead of
// replaced. Without that check, a mistyped `-o` would delete someone's source.
fn write_generated(path string, content string) ! {
	dir := os.dir(path)
	if dir != '' && !os.exists(dir) {
		os.mkdir_all(dir)!
	}
	if os.exists(path) {
		existing := os.read_file(path)!
		if !existing.contains('Code generated by `v pbgen`') {
			return error('pbgen: ${path} exists and is not generated by this tool; refusing to overwrite it')
		}
	}
	os.write_file(path, content) or { return error('pbgen: cannot write ${path}: ${err.msg()}') }
}
