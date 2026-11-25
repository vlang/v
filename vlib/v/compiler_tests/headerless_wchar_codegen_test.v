import os
import v.tempname

fn test_headerless_wchar_prefers_compiler_type() {
	root := os.join_path(os.vtmp_dir(), 'v3_headerless_wchar_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_headerless_wchar.v')
	c_out := os.join_path(root, 'v3_headerless_wchar.c')
	os.write_file(src, "fn main() {\n\tmessage := 'ok'\n\tprintln(message)\n}\n") or { panic(err) }
	// Windows normally uses SDK headers. Select a headerless target on every host.
	gen_c := os.exec([@VEXE, '-os', 'linux', '-b', 'c', '-o', '${c_out}', '${src}'])
	assert gen_c.exit_code == 0, gen_c.output

	c_code := (os.read_file(c_out) or { panic(err) }).replace('\r\n', '\n')
	wchar_block := '#ifdef __WCHAR_TYPE__
typedef __WCHAR_TYPE__ wchar_t;
#elif defined(_WIN32)
typedef unsigned short wchar_t;
#else
typedef unsigned int wchar_t;'
	assert c_code.contains(wchar_block), c_code
}
