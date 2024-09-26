import os
import rand
import v.ast
import v.parser
import v.fmt
import v.pref
import v.util.diff

const vexe = os.getenv('VEXE')
const tmpfolder = os.join_path(os.temp_dir(), 'fmt_bin2v_test')
const b2v_keep_path = os.join_path(tmpfolder, rand.ulid() + '.vv')
const ifilename = os.file_name(b2v_keep_path)
const fpref = &pref.Preferences{
	is_fmt: true
}

fn test_bin2v_formatting() {
	os.mkdir_all(tmpfolder)!
	defer {
		os.rmdir_all(tmpfolder) or {}
	}
	prepare_bin2v_file()

	mut table := ast.new_table()
	file_ast := parser.parse_file(b2v_keep_path, mut table, .parse_comments, fpref)
	result_ocontent := fmt.fmt(file_ast, mut table, fpref, false)
	eprintln('> the file ${b2v_keep_path} can be formatted.')
	expected_ocontent := os.read_file(b2v_keep_path)!
	if expected_ocontent != result_ocontent {
		vfmt_result_file := os.join_path(tmpfolder, 'vfmt_run_over_${ifilename}')
		os.write_file(vfmt_result_file, result_ocontent)!
		println(diff.compare_files(b2v_keep_path, vfmt_result_file)!)
		exit(1)
	} else {
		assert true
	}
	println('> vfmt: `v bin2v file.v` produces fully formatted code.')
}

fn prepare_bin2v_file() {
	write_bin2v_keep_content() or {
		eprintln('Failed preparing bin2v_keep.vv: ${err.msg()}')
		return
	}
	eprintln('> prepared ${b2v_keep_path} .')
}

fn write_bin2v_keep_content() ! {
	// Note: do not put large files here; the goal of this particular test is
	// just to guarantee that the output of `v bin2v` is invariant to vfmt, not
	// to stress out bin2v or vfmt...
	img0 := os.join_path('vlib', 'v', 'embed_file', 'tests', 'v.png')
	img1 := os.join_path('examples', 'assets', 'logo.png')
	os.rm(b2v_keep_path) or {}
	cmd := '${os.quoted_path(vexe)} bin2v -w ${os.quoted_path(b2v_keep_path)} ${os.quoted_path(img0)} ${os.quoted_path(img1)}'
	eprintln('> running: ${cmd}')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		return error_with_code(res.output.trim_space(), res.exit_code)
	}
}
