import os
import wasm

const runtime_page = 1024 * 64 // 64 KiBs

fn generate_ciovec(mut start wasm.Function, sp wasm.LocalIndex) {
	// construct struct __wasi_ciovec_t
	//
	// field, `const uint8_t *buf`
	start.i32_const(runtime_page)
	start.local_get(sp)
	start.store(.i32_t, 2, 0)
	// field, `__wasi_size_t buf_len`
	start.i32_const(runtime_page)
	start.i32_const(1) // len
	start.store(.i32_t, 2, 4)
}

fn generate_code(mut start wasm.Function, bf_expr string) {
	// locals are initialised to zero, by spec
	sp := start.new_local_named(.i32_t, 'sp')

	mut loop_labels := []wasm.LabelIndex{}
	mut block_labels := []wasm.LabelIndex{}

	// our page, the second one

	for ch in bf_expr {
		match ch {
			`>` {
				start.local_get(sp)
				start.i32_const(1)
				start.add(.i32_t)
				start.local_set(sp)
			}
			`<` {
				start.local_get(sp)
				start.i32_const(1)
				start.sub(.i32_t)
				start.local_set(sp)
			}
			`+` {
				start.local_get(sp)
				{
					start.local_get(sp)
					start.load8(.i32_t, false, 0, 0)
					start.i32_const(1)
					start.add(.i32_t)
				}
				start.store8(.i32_t, 0, 0)
			}
			`-` {
				start.local_get(sp)
				{
					start.local_get(sp)
					start.load8(.i32_t, false, 0, 0)
					start.i32_const(1)
					start.sub(.i32_t)
				}
				start.store8(.i32_t, 0, 0)
			}
			`.` {
				generate_ciovec(mut start, sp)

				start.i32_const(1) // stdout
				start.i32_const(runtime_page) // *iovs
				start.i32_const(1) // iovs_len
				start.i32_const(runtime_page + 1024) // *nwritten
				start.call_import('wasi_unstable', 'fd_write')
				start.drop() // ignore errno
			}
			`,` {
				generate_ciovec(mut start, sp)

				start.i32_const(0) // stdin
				start.i32_const(runtime_page) // *iovs
				start.i32_const(1) // iovs_len
				start.i32_const(runtime_page + 1024) // *nwritten
				start.call_import('wasi_unstable', 'fd_read')
				start.drop() // ignore errno
			}
			`[` {
				block_lbl := start.c_block([], [])
				loop_lbl := start.c_loop([], [])
				{
					start.local_get(sp)
					start.load8(.i32_t, false, 0, 0)
					start.eqz(.i32_t)
					start.c_br_if(block_lbl)
				}
				loop_labels << loop_lbl
				block_labels << block_lbl
			}
			`]` {
				loop_lbl := loop_labels.pop()
				start.c_br(loop_lbl) // jump back to top
				start.c_end(loop_lbl)
				start.c_end(block_labels.pop())
			}
			else {}
		}
	}
}

@[noreturn]
fn usage() {
	eprintln('Usage: bf <expr> <outfile.wasm>')
	eprintln('   or: bf <path/input.b> <outfile.wasm> (note the `.b` extension)')
	exit(1)
}

fn main() {
	mut bf_expr := os.args[1] or { usage() }
	if bf_expr.ends_with('.b') {
		bf_expr = os.read_file(bf_expr) or {
			eprintln('file ${bf_expr} could not be read, error: ${err}')
			usage()
		}
	}
	outfile := os.args[2] or { usage() }

	mut m := wasm.Module{}
	m.enable_debug('wasm bf')
	m.new_function_import('wasi_unstable', 'fd_write', [.i32_t, .i32_t, .i32_t, .i32_t],
		[.i32_t])
	m.new_function_import('wasi_unstable', 'fd_read', [.i32_t, .i32_t, .i32_t, .i32_t],
		[.i32_t])
	m.assign_memory('memory', true, 2, none)

	mut start := m.new_function('_start', [], [])
	{
		generate_code(mut start, bf_expr)
	}
	m.commit(start, true)

	bytes := m.compile()
	os.write_file_array(outfile, bytes)!
}
