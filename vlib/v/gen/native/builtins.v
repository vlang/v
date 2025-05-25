// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import term

enum Builtin {
	int_to_string
	bool_to_string
	reverse_string
	compare_strings
}

struct BuiltinFn {
	body     fn (builtin BuiltinFn, mut g Gen) = unsafe { nil }
	arg_regs []Register
mut:
	calls []i64 // call addresses
}

pub const inline_builtins = ['print', 'eprint', 'println', 'eprintln', 'C.syscall'] // classic V builtin functions accessible to the user get inlined

pub fn (mut g Gen) init_builtins() {
	g.builtins = {
		// longer algorithms and internal functions inaccessible to the user
		// used to keep executable size small and the bytecode distraction-free
		.int_to_string:  BuiltinFn{
			// 32-bit signed integer to string conversion
			body:     fn (builtin BuiltinFn, mut g Gen) {
				g.code_gen.convert_int_to_string(builtin.arg_regs[0], builtin.arg_regs[1])
			}
			arg_regs: [Amd64Register.rcx, Amd64Register.rdi] // rcx: int to convert, rdi: end of 32byte buffer (with 4 bytes at the beggining for len) see allocate_array
		}
		.bool_to_string: BuiltinFn{
			body:     fn (builtin BuiltinFn, mut g Gen) {
				g.code_gen.convert_bool_to_string(builtin.arg_regs[0])
			}
			arg_regs: [Amd64Register.rax]
		}
		.reverse_string: BuiltinFn{
			body:     fn (builtin BuiltinFn, mut g Gen) {
				g.code_gen.reverse_string(builtin.arg_regs[0])
			}
			arg_regs: [Amd64Register.rdi]
		}
	}
}

pub fn (mut g Gen) generate_builtins() {
	for name, builtin in g.builtins {
		if builtin.calls.len == 0 { // if a builtin does not get called, do not emit it
			continue
		}

		if g.pref.is_verbose {
			println(term.green('\n(builtin) ${name}:'))
		}

		g.stack_var_pos = 0
		call_addr := g.pos()
		g.defer_stmts.clear()
		g.labels = &LabelTable{}

		g.code_gen.builtin_decl(builtin)

		g.patch_labels()

		// patch all call addresses where this builtin gets called
		for call in builtin.calls {
			rel := g.code_gen.call_addr_at(i32(call_addr), call)
			g.write32_at(call + 1, i32(rel))
		}
	}
}

pub fn (mut g Gen) get_builtin_arg_reg(name Builtin, index i32) Register {
	builtin := g.builtins[name] or { panic('undefined builtin function ${name}') }
	if index >= builtin.arg_regs.len {
		g.n_error('builtin ${name} does only have ${builtin.arg_regs.len} arguments, requested ${index}')
	}
	return builtin.arg_regs[index]
}

pub fn (mut g Gen) call_builtin(name Builtin) {
	g.builtins[name].calls << g.code_gen.call_builtin(name)
}
