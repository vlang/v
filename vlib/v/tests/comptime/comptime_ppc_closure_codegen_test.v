import os
import rand

const vexe = @VEXE

fn test_cross_arch_ppc_closure_codegen() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'issue_20507_${rand.ulid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source := os.join_path(tmp_dir, 'main.v')
	output := os.join_path(tmp_dir, 'main.c')
	os.write_file(source, 'fn main() {
	x := 123
	f := fn [x] () int {
		return x
	}
	println(f())
}
') or {
		panic(err)
	}
	res :=
		os.execute('${os.quoted_path(vexe)} -gc none -arch ppc -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
	assert res.exit_code == 0, res.output
	csrc := os.read_file(output) or { panic(err) }
	assert csrc.contains('__V_ppc')
	assert csrc.contains('0x7c)), 0x08, 0x02, 0xa6')
	assert csrc.contains('0x94)), 0x21, 0xff, 0xf0')
	assert !csrc.contains('0xF3)), 0x44, 0x0F, 0x7E')
}
