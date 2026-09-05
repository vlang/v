import os

const vexe = @VEXE

// A `raw` template has to reach the C backend untouched, including AVX-512 mask
// syntax (`%{%%k1%}%{z%}`) and `k`/`zmm` register clobbers.
//
// Assembling that requires the C compiler to accept a `k` register clobber for the
// *default* target, which older compilers refuse - FreeBSD's clang 19 reports
// `error: the register 'k1' cannot be clobbered in 'asm' for the current target` -
// so this checks what V emits rather than handing it to the assembler. Only C is
// generated, so it also runs on hosts that are not amd64.
fn test_raw_avx512_mask_syntax_is_emitted_verbatim() {
	dir := os.join_path(os.vtmp_dir(), 'asm_raw_avx512_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'mask.v')
	out_c := os.join_path(dir, 'mask.c')
	os.write_file(source, r'fn masked() {
	if never() {
		asm amd64 raw {
			"vpxord %%zmm0, %%zmm0, %%zmm0%{%%k1%}%{z%}\n\t"
			; ; ; zmm0
			  k1
		}
	}
}

@[noinline]
fn never() bool {
	return false
}

fn main() {
	masked()
}
')!
	res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out_c)} ${os.quoted_path(source)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(out_c)!
	assert generated.contains(r'"vpxord %%zmm0, %%zmm0, %%zmm0%{%%k1%}%{z%}\n\t"'), generated
	assert generated.contains('"zmm0"'), generated
	assert generated.contains('"k1"'), generated
}
