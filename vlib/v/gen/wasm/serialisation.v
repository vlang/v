module wasm

import v.ast
import v.gen.wasm.binaryen
import encoding.binary as bin
import math.bits

fn (mut g Gen) bake_constants_plus_initialisers() []GlobalData {
	mut initialisers := []GlobalData{}

	for _, global in g.globals {
		match global.init {
			/*
			ast.ArrayInit {
				// TODO: call a seraliser recursively over all elements
				
				if !global.init.is_fixed {
					g.w_error('wasm backend does not support non fixed arrays yet')
				}
				for global.init
			}*/
			ast.BoolLiteral {
				g.constant_data << ConstantData{
					offset: global.abs_address
					data: [u8(global.init.val)]
				}
			}
			ast.FloatLiteral {
				mut buf := []u8{len: 8}
				wtyp := g.get_wasm_type(global.ast_typ)
				match wtyp {
					type_f32 {
						bin.little_endian_put_u32(mut buf, bits.f32_bits(global.init.val.f32()))
					}
					type_f64 {
						bin.little_endian_put_u64(mut buf, bits.f64_bits(global.init.val.f64()))
						unsafe {
							buf.len = 4
						}
					}
					else {}
				}

				g.constant_data << ConstantData{
					offset: global.abs_address
					data: buf
				}
			}
			ast.StringLiteral {
				offset, len := g.allocate_string(global.init)

				if g.table.sym(global.ast_typ).info !is ast.Struct {
					mut buf := []u8{len: 4}
					bin.little_endian_put_u32(mut buf, u32(offset))
					g.constant_data << ConstantData{
						offset: global.abs_address
						data: buf
					}
				} else {
					mut buf := []u8{len: 8}
					bin.little_endian_put_u32(mut buf, u32(offset))
					bin.little_endian_put_u32_at(mut buf, u32(len), 4)

					g.constant_data << ConstantData{
						offset: global.abs_address
						data: buf
					}
				}
			}
			ast.IntegerLiteral {
				mut buf := []u8{len: 8}
				wtyp := g.get_wasm_type(global.ast_typ)
				match wtyp {
					type_i32 {
						bin.little_endian_put_u32(mut buf, u32(global.init.val.int()))
					}
					type_i64 {
						bin.little_endian_put_u64(mut buf, u64(global.init.val.i64()))
						unsafe {
							buf.len = 4
						}
					}
					else {}
				}

				g.constant_data << ConstantData{
					offset: global.abs_address
					data: buf
				}
			}
			else {
				initialisers << global
			}
		}
	}

	return initialisers
}

fn round_up_to_multiple(val int, multiple int) int {
	return val + (multiple - val % multiple) % multiple
}

fn (mut g Gen) make_vinit() binaryen.Function {
	runtime_inits := g.bake_constants_plus_initialisers()

	g.bare_function_start()

	mut body := runtime_inits.map(g.set_var_v(it.to_var(''), g.expr(it.init, it.ast_typ)))

	for mod_name in g.table.modules {
		if mod_name == 'v.reflection' {
			g.w_error('the wasm backend does not implement `v.reflection` yet')
		}

		init_fn_name := if mod_name != 'builtin' { '${mod_name}.init' } else { 'init' }
		if _ := g.table.find_fn(init_fn_name) {
			body << binaryen.call(g.mod, init_fn_name.str, unsafe { nil }, 0, type_none)
		}
		cleanup_fn_name := if mod_name != 'builtin' { '${mod_name}.cleanup' } else { 'cleanup' }
		if _ := g.table.find_fn(cleanup_fn_name) {
			body << binaryen.call(g.mod, cleanup_fn_name.str, unsafe { nil }, 0, type_none)
		}
	}

	return g.bare_function('_vinit', g.mkblock(body))
}

fn (mut g Gen) housekeeping() {
	// `_vinit` should be used to initialise the WASM module,
	// then `main.main` can be called safely.
	vinit := g.make_vinit()
	stack_base := round_up_to_multiple(g.constant_data_offset, 1024)
	heap_base := if g.needs_stack {
		stack_base + 1024 * 16 // 16KiB of stack
	} else {
		stack_base
	}
	pages_needed := heap_base / (1024 * 64) + 1

	if g.needs_stack || g.constant_data.len != 0 {
		data := g.constant_data.map(it.data.data)
		data_len := g.constant_data.map(it.data.len)
		data_offsets := g.constant_data.map(binaryen.constant(g.mod, binaryen.literalint32(it.offset)))
		passive := []bool{len: g.constant_data.len, init: false}

		binaryen.setmemory(g.mod, pages_needed, pages_needed + 4, c'memory', data.data,
			passive.data, data_offsets.data, data_len.data, data.len, false, false, c'memory')
		binaryen.addglobal(g.mod, c'__heap_base', type_i32, false, g.literalint(heap_base,
			ast.int_type))
	}
	if g.needs_stack {
		// `g.constant_data_offset` rounded up to a multiple of 1024
		binaryen.addglobal(g.mod, c'__vsp', type_i32, true, g.literalint(stack_base, ast.int_type))
	}
	if g.pref.os == .wasi {
		main_expr := g.mkblock([binaryen.call(g.mod, c'_vinit', unsafe { nil }, 0, type_none),
			binaryen.call(g.mod, c'main.main', unsafe { nil }, 0, type_none)])
		binaryen.addfunction(g.mod, c'_start', type_none, type_none, unsafe { nil }, 0,
			main_expr)
		binaryen.addfunctionexport(g.mod, c'_start', c'_start')
	} else {
		// In `browser` mode, and function can be exported and called regardless.
		// To avoid uninitialised data, `_vinit` is set to be ran immediately on
		// WASM module creation.
		binaryen.setstart(g.mod, vinit)
	}
}
