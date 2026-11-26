// Execute WebAssembly with V's own interpreter.
module main

import os
import strconv
import wasm.runtime
import wasm.runtime.wasi

fn main() {
	run_wasm(os.args[1..]) or {
		if err is wasi.Exit { exit(err.status) }
		eprintln('wasm: ${err.msg()}')
		exit(1)
	}
}

fn run_wasm(args []string) ! {
	if args.len == 0 || args[0] in ['-h', '--help'] {
		println('Usage: v wasm [--invoke FUNCTION] [--fuel N] FILE.wasm [ARGS...]')
		println('Runs _start by default. --invoke parses scalar arguments and prints results.')
		return
	}
	mut name := '_start'
	mut invoke := false
	mut fuel := u64(100000000)
	mut i := 0
	for i < args.len && args[i].starts_with('--') {
		option := args[i]
		if option == '--' {
			i++
			break
		}
		if option !in ['--invoke', '--fuel'] { return error('unknown option `${option}`') }
		if i + 1 >= args.len { return error('missing value for `${option}`') }
		if option == '--invoke' {
			name = args[i + 1]
			invoke = true
		} else {
			fuel = strconv.parse_uint(args[i + 1], 10, 64)!
		}
		i += 2
	}
	if i >= args.len { return error('missing .wasm file') }
	mod := runtime.load(os.read_bytes(args[i])!)!
	mut vm := runtime.instantiate(mod, wasi.imports(args[i..]), fuel: fuel)!
	signature := mod.signature(name)!
	mut values := []runtime.Value{}
	if invoke {
		if args.len - i - 1 != signature.parameters.len {
			return error('`${name}` expects ${signature.parameters.len} arguments')
		}
		for j, typ in signature.parameters {
			text := args[i + j + 1]
			values << match typ {
				.i32 { runtime.Value.i32(int(strconv.parse_int(text, 0, 32)!)) }
				.i64 { runtime.Value.i64(strconv.parse_int(text, 0, 64)!) }
				.f32 { runtime.Value.f32(f32(strconv.atof64(text)!)) }
				.f64 { runtime.Value.f64(strconv.atof64(text)!) }
				else { return error('reference arguments require the embedding API') }
			}
		}
	}
	results := vm.call(name, values)!
	if invoke {
		for value in results {
			match value.typ {
				.i32 { println(value.as_i32()) }
				.i64 { println(value.as_i64()) }
				.f32 { println(value.as_f32()) }
				.f64 { println(value.as_f64()) }
				else { println(value.bits) }
			}
		}
	}
}
