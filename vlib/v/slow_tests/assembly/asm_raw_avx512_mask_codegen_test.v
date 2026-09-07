import os

const vexe = @VEXE

// A `raw` template has to reach the C backend untouched, including AVX-512 mask
// syntax (`%{%%k1%}%{z%}`) and the `zmm` register clobber.
//
// Only C is generated, so this also checks the raw template on hosts that are not
// amd64. `k1` is read as a write mask and must not be emitted as a clobber.
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
	res := os.execute('${os.quoted_path(vexe)} -cross -o ${os.quoted_path(out_c)} ${os.quoted_path(source)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(out_c)!
	assert generated.contains(r'"vpxord %%zmm0, %%zmm0, %%zmm0%{%%k1%}%{z%}\n\t"'), generated
	assert generated.contains('"zmm0"'), generated
	assert !generated.contains('"k1"'), generated
}
