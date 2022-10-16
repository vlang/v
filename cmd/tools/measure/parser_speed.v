import os
import time
import v.ast
import v.pref
import v.parser
import v.errors
import v.scanner

fn main() {
	files := os.args#[1..]
	if files.len > 0 && files[0].starts_with('@') {
		lst_path := files[0].all_after('@')
		listed_files := os.read_file(lst_path)!.split('\n')
		process_files(listed_files)!
		return
	}
	process_files(files)!
}

fn process_files(files []string) ! {
	mut table := ast.new_table()
	mut pref := pref.new_preferences()
	pref.is_fmt = true
	pref.skip_warnings = true
	pref.output_mode = .silent
	mut sw := time.new_stopwatch()
	mut total_us := i64(0)
	mut total_bytes := i64(0)
	mut total_tokens := i64(0)
	for f in files {
		if f == '' {
			continue
		}
		if f.ends_with('_test.v') {
			continue
		}
		// do not measure the scanning, but only the parsing:
		mut p := new_parser(f, .skip_comments, table, pref)
		///
		sw.restart()
		_ := p.parse()
		f_us := sw.elapsed().microseconds()
		///
		total_us += f_us
		total_bytes += p.scanner.text.len
		total_tokens += p.scanner.all_tokens.len
		println('${f_us:10}us ${p.scanner.all_tokens.len:10} ${p.scanner.text.len:10} ${(f64(p.scanner.text.len) / p.scanner.all_tokens.len):7.3} ${p.errors.len:4} $f')
	}
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${(f64(total_tokens) / total_bytes):7.3} | speed: ${(f64(total_bytes) / total_us):2.5f} MB/s')
}

fn new_parser(path string, comments_mode scanner.CommentsMode, table &ast.Table, pref &pref.Preferences) &parser.Parser {
	mut p := &parser.Parser{
		scanner: scanner.new_scanner_file(path, comments_mode, pref) or { panic(err) }
		comments_mode: comments_mode
		table: table
		pref: pref
		scope: &ast.Scope{
			start_pos: 0
			parent: table.global_scope
		}
		errors: []errors.Error{}
		warnings: []errors.Warning{}
	}
	p.set_path(path)
	return p
}
