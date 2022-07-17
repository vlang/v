// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

import os
import strings
import v.ast
import v.util
import v.mathutil as mu
import v.token
import v.errors
import v.pref
import term

struct BuiltinFn {
	body     fn (builtin BuiltinFn, mut g Gen)
	arg_regs []Register
}

pub const inline_builtins = ['assert', 'print', 'eprint', 'println', 'eprintln', 'exit', 'C.syscall'] // classic V builtin functios accessible to the user get inlined

pub const builtins = {
	// longer algorithms and internal functions inaccessible to the user
	// used to keep executable size small and the bytecode distraction-free
	'reverse_string': BuiltinFn{
		body: fn (builtin BuiltinFn, mut g Gen) {
			g.reverse_string(builtin.arg_regs[0])
		}
		arg_regs: [.rdi]
	}
	'int_to_string':  BuiltinFn{
		body: fn (builtin BuiltinFn, mut g Gen) {
			g.convert_int_to_string(builtin.arg_regs[0], builtin.arg_regs[1])
		}
		arg_regs: [.rcx, .rdi]
	}
}

pub fn (mut g Gen) register_builtin_address(name string) {
	g.builtin_addr[name] = g.pos()
}

pub fn (mut g Gen) generate_builtins() {
	for name, builtin in native.builtins {
		if g.pref.is_verbose {
			println(term.green('\n(builtin) $name:'))
		}

		g.stack_var_pos = 0
		g.register_builtin_address(name)
		g.defer_stmts.clear()
		g.labels = &LabelTable{}

		if g.pref.arch == .arm64 {
			g.n_error('builtins are not implemented for arm64')
		} else {
			g.builtin_decl_amd64(builtin)
		}

		g.patch_labels()
	}
}

pub fn (mut g Gen) get_builtin_arg_reg(name string, index int) Register {
	builtin := native.builtins[name] or { panic('undefined builtin function $name') }
	if index >= builtin.arg_regs.len {
		g.n_error('builtin $name does only have $builtin.arg_regs.len arguments, wanted $index')
	}
	return builtin.arg_regs[index]
}

pub fn (mut g Gen) call_builtin(name string) {
	if g.pref.arch == .arm64 {
		g.n_error('builtin calls are not implemented for amd64')
	} else {
		g.call_builtin_amd64(name)
	}
}
