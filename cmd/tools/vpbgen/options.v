module main

// PbgenOptions is what the command line asked for.
pub struct PbgenOptions {
pub mut:
	// inputs are the .proto files to read, in the order given.
	inputs []string
	// module_name is the V module the generated files declare. Empty means derive
	// it from the first file's package.
	module_name string
	// codec_out is where the codec is written. Empty means stdout.
	codec_out string
	// grpc_out is where the service declarations are written. Empty means the
	// codec output, so a schema with no service produces no extra file.
	grpc_out string
	// check parses and resolves without writing anything, which is what a CI
	// step wants.
	check bool
	// verbose reports each declaration as it is resolved.
	verbose bool
	// help asks for the usage text.
	help bool
}

// usage_lines is the tool's help text, one entry per output line.
//
// It is a list rather than one long literal for a reason that is worth knowing:
// a single multi-line literal in this compiler is easy to get subtly wrong when
// the prose contains quotes or backticks, and the failure shows up as a parse
// error far from the line that caused it. A list makes every line independent and
// the whole thing reviewable line by line.
//
// The text is also what `v help pbgen` shows, so it has to stand on its own.
const usage_lines = [
	'usage: v pbgen [options] <file.proto>...'
	''
	'Generate V protobuf codecs and gRPC service declarations from .proto schemas.'
	''
	'The generator writes explicit calls into the Packer and Unpacker of'
	'`encoding.protobuf` rather than reflecting over a struct at run time. The'
	'generated file is the artefact a reader debugs, and a frame in a call such as'
	'`packer.write_int32(7, msg.written)` names the field, where a frame inside a'
	'generic encoder does not.'
	''
	'Options:'
	'  -m <name>        V module name for the generated files. Defaults to the'
	'                   package of the first file, underscored: package `google.rpc`'
	'                   gives `google_rpc`, which is also the directory layout that V'
	'                   import rules expect.'
	'  -o <file>        Write the codec here. Defaults to stdout.'
	'  -grpc <file>     Write the service paths and interfaces here. Defaults to the'
	'                   codec output.'
	'  -check           Parse and resolve only, writing nothing. Use it to validate a'
	'                   schema, including its imports, without producing files.'
	'  -v               Report each declaration as it is resolved.'
	'  -h, --help       Print this help.'
	''
	'Examples:'
	'  v pbgen -m kv -o kv/codec.v kv.proto'
	'  v pbgen -m kv -o kv/codec.v -grpc kv/service.v kv.proto'
	'  v pbgen -check -m mypkg api/v1/*.proto'
	''
	'Only proto3 is supported. Groups, `extend`, and `required` are reported'
	'rather than accepted: a `required` field changes what a decoder must accept,'
	'and quietly treating it as optional would hide that.',
]

// usage returns the tool's help text.
pub fn usage() string {
	return usage_lines.join_lines()
}

// parse_options turns the tool's arguments into PbgenOptions.
//
// The command token itself is dropped when present, because the launcher passes
// `v pbgen ...` through with the subcommand still in `os.args`.
pub fn parse_options(raw []string) !PbgenOptions {
	mut opts := PbgenOptions{}
	mut args := raw.clone()
	// Two leading tokens are dropped, in either order: the command name, which
	// the launcher passes through, and a `--`, which is how a caller tells the
	// launcher that the flags after it belong to the tool rather than to the
	// compiler.
	if args.len > 0 && (args[0] == 'pbgen' || args[0] == 'vpbgen') {
		args = args[1..]
	}
	if args.len > 0 && args[0] == '--' {
		args = args[1..]
	}
	mut i := 0
	for i < args.len {
		arg := args[i]
		// A flag that takes a value advances the index past that value, or the
		// value would be read again as if it were a positional argument.
		mut consumed_value := false
		match arg {
			'-h', '--help' {
				opts.help = true
			}
			'-v' {
				opts.verbose = true
			}
			'-check' {
				opts.check = true
			}
			'-m', '-module' {
				opts.module_name = take_value(args, i, '-m')
				consumed_value = true
			}
			'-o', '-out' {
				opts.codec_out = take_value(args, i, '-o')
				consumed_value = true
			}
			'-grpc' {
				opts.grpc_out = take_value(args, i, '-grpc')
				consumed_value = true
			}
			else {
				if arg.starts_with('-') {
					return error('unknown option `${arg}`\n\n${usage()}')
				}
				opts.inputs << arg
			}
		}
		if consumed_value {
			i += 2
		} else {
			i++
		}
	}
	if !opts.help && opts.inputs.len == 0 {
		return error('no .proto file given\n\n${usage()}')
	}
	if opts.module_name != '' && !is_v_module_name(opts.module_name) {
		return error('`${opts.module_name}` cannot be a V module name: a module name must be a plain identifier and must not be a V keyword')
	}
	return opts
}

// take_value returns the argument after a flag, exiting if it is missing.
pub fn take_value(args []string, i int, flag string) string {
	next := i + 1
	if next >= args.len {
		eprintln('pbgen: ${flag} needs a value\n\n${usage()}')
		exit(1)
	}
	return args[next]
}

// is_v_module_name reports whether `name` could be a V module name. V's import
// rules require the module to match its directory, and a name that is a keyword
// or contains a path separator would produce a file that cannot be imported.
pub fn is_v_module_name(name string) bool {
	if name == '' || is_v_keyword(name) {
		return false
	}
	for c in name {
		if !(c == `_` || c.is_alnum()) {
			return false
		}
	}
	return true
}
