// Minimal WASI Preview 1 host services for command-line WebAssembly programs.
module wasi

import os
import wasm.runtime

// Exit is returned by proc_exit without terminating the embedding application.
pub struct Exit {
pub:
	status int
}

// msg describes the guest exit.
pub fn (e Exit) msg() string { return 'WebAssembly exited with status ${e.status}' }

// code returns the guest exit status.
pub fn (e Exit) code() int { return e.status }

fn put_u32(mut vm runtime.Instance, address u32, value u32) ! {
	vm.write_memory(address, [u8(value), u8(value >> 8), u8(value >> 16), u8(value >> 24)])!
}

fn get_u32(data []u8, offset int) u32 {
	return u32(data[offset]) | (u32(data[offset + 1]) << 8) | (u32(data[offset + 2]) << 16) | (u32(data[offset + 3]) << 24)
}

fn fd_write(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	fd := args[0].as_i32()
	if fd != 1 && fd != 2 { return [runtime.Value.i32(8)] }
	iovs := u32(args[1].bits)
	count := u32(args[2].bits)
	if count > 0x1fffffff { return [runtime.Value.i32(21)] }
	result := u32(args[3].bits)
	vm.read_memory(result, 4)!
	vectors := vm.read_memory(iovs, count * 8)!
	mut output := if fd == 1 { os.stdout() } else { os.stderr() }
	mut written := u32(0)
	for i in 0 .. int(count) {
		address := get_u32(vectors, i * 8)
		length := get_u32(vectors, i * 8 + 4)
		data := vm.read_memory(address, length)!
		n := output.write(data) or { return [runtime.Value.i32(29)] }
		written += u32(n)
		if n < data.len { break }
	}
	put_u32(mut vm, result, written)!
	return [runtime.Value.i32(0)]
}

fn fd_read(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	if args[0].as_i32() != 0 { return [runtime.Value.i32(8)] }
	return read_input(mut vm, args, os.stdin())
}

fn read_input(mut vm runtime.Instance, args []runtime.Value, input os.File) ![]runtime.Value {
	count := u32(args[2].bits)
	if count > 0x1fffffff { return [runtime.Value.i32(21)] }
	result := u32(args[3].bits)
	vm.read_memory(result, 4)!
	vectors := vm.read_memory(u32(args[1].bits), count * 8)!
	mut total := u32(0)
	for i in 0 .. int(count) {
		address := get_u32(vectors, i * 8)
		length := get_u32(vectors, i * 8 + 4)
		if length == 0 { continue }
		mut data := vm.read_memory(address, length)!
		n := input.read(mut data) or {
			if err is os.Eof { break }
			return [runtime.Value.i32(29)]
		}
		vm.write_memory(address, data[..n])!
		total += u32(n)
		if n < data.len { break }
	}
	put_u32(mut vm, result, total)!
	return [runtime.Value.i32(0)]
}

fn proc_exit(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	_ = vm
	return Exit{ status: int(u32(args[0].bits)) }
}

fn empty_sizes(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	put_u32(mut vm, u32(args[0].bits), 0)!
	put_u32(mut vm, u32(args[1].bits), 0)!
	return [runtime.Value.i32(0)]
}

fn empty_get(mut vm runtime.Instance, args []runtime.Value) ![]runtime.Value {
	_ = vm
	_ = args
	return [runtime.Value.i32(0)]
}

fn host(name string, parameters []runtime.ValueType, results []runtime.ValueType, callback runtime.Callback) runtime.HostFunction {
	return runtime.HostFunction{
		module:    'wasi_snapshot_preview1'
		name:      name
		signature: runtime.Signature{parameters, results}
		callback:  callback
	}
}

// imports supplies stdio, process exit, arguments, and an empty environment.
// No filesystem or network access is provided. Include argv[0] in args.
pub fn imports(args []string) []runtime.HostFunction {
	guest_args := args.clone()
	return [
		host('fd_write', [.i32, .i32, .i32, .i32], [.i32], fd_write),
		host('fd_read', [.i32, .i32, .i32, .i32], [.i32], fd_read),
		host('proc_exit', [.i32], [], proc_exit),
		host('environ_sizes_get', [.i32, .i32], [.i32], empty_sizes),
		host('environ_get', [.i32, .i32], [.i32], empty_get),
		host('args_sizes_get', [.i32, .i32], [.i32], fn [guest_args] (mut vm runtime.Instance, values []runtime.Value) ![]runtime.Value {
			mut size := u64(0)
			for arg in guest_args { size += u64(arg.len) + 1 }
			if size > 0xffffffff { return error('WASI arguments too large') }
			put_u32(mut vm, u32(values[0].bits), u32(guest_args.len))!
			put_u32(mut vm, u32(values[1].bits), u32(size))!
			return [runtime.Value.i32(0)]
		}),
		host('args_get', [.i32, .i32], [.i32], fn [guest_args] (mut vm runtime.Instance, values []runtime.Value) ![]runtime.Value {
			mut pointers := u64(u32(values[0].bits))
			mut address := u64(u32(values[1].bits))
			for arg in guest_args {
				if pointers + 4 > 0x100000000 || address + u64(arg.len) + 1 > 0x100000000 {
					return error('WASI arguments out of bounds')
				}
				put_u32(mut vm, u32(pointers), u32(address))!
				vm.write_memory(u32(address), arg.bytes())!
				vm.write_memory(u32(address + u64(arg.len)), [u8(0)])!
				pointers += 4
				address += u64(arg.len) + 1
			}
			return [runtime.Value.i32(0)]
		}),
	]
}
