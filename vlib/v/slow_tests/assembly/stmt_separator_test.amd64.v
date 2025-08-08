import os
import time

const asm_file_content = '
fn asm_fn() {
	mut x := u64(0)
	mut y := u64(0)
	mut z := u64(0)
	mut hi := u64(0)
	mut lo := u64(0)
	asm amd64 {
		mulq rdx
		addq rax, z
		adcq rdx, 0
		; =a (lo)
		=d (hi)
		; a (x)
		  d (y)
		  r (z)
		; cc
	}
}

fn main() {
	asm_fn()
}
'

const vexe = os.getenv('VEXE')
const v_file = os.join_path(os.vtmp_dir(), 'generated_stmt_separator.amd64.v')
const genexe_file = os.join_path(os.vtmp_dir(), 'generated_stmt_separator.exe')
const c_file = os.join_path(os.vtmp_dir(), 'generated_stmt_separator.exe.tmp.c')

fn test_stmt_separator() {
	os.write_file(v_file, asm_file_content)!
	eprintln('Compiling...')
	compile_cmd := '${os.quoted_path(vexe)} -cg -skip-running -no-rsp -keepc -o ${os.quoted_path(genexe_file)} ${os.quoted_path(v_file)}'
	eprintln('> compile_cmd: ${compile_cmd}')
	time.sleep(1000 * time.millisecond) // improve chances of working on windows
	compile_res := os.system(compile_cmd)
	assert compile_res == 0

	content := os.read_file(c_file)!
	os.rm(v_file)!
	os.rm(genexe_file)!
	os.rm(c_file)!
	assert content.contains(r'addq %[z], %%rax\n\t')
}
