import os
import time
import v.ast
import v.pref
import v.parser
import v.errors
import v.scanner
import term

const skip_tests = os.getenv_opt('SKIP_TESTS') or { '' }.bool()

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

fn hline() {
	println('------------------------------------------------------------------------------------------------------------------------------------')
}

fn theader() {
	println('        Time     Tokens      Bytes      Lines   Bytes/Token     Errors')
}

fn process_files(files []string) ! {
	nthreads := 1 // TODO
	mut pref_ := pref.new_preferences()
	pref_.is_fmt = true
	pref_.skip_warnings = true
	pref_.output_mode = .silent
	mut sw := time.new_stopwatch()
	mut total_us := i64(0)
	mut total_bytes := i64(0)
	mut total_tokens := i64(0)
	mut total_lines := i64(0)
	mut total_errors := i64(0)
	for f in files {
		mut table := ast.new_table()
		if f == '' {
			continue
		}
		if skip_tests && f.ends_with('_test.v') {
			continue
		}
		// do not measure the scanning, but only the parsing:
		mut p := new_parser(f, .skip_comments, table, pref_)
		///
		sw.restart()
		ast_file := p.parse()
		f_us := sw.elapsed().microseconds()
		///
		total_us += f_us
		total_bytes += p.scanner.text.len
		total_tokens += p.scanner.all_tokens.len
		total_lines += ast_file.nr_lines
		total_errors += p.errors.len
		println('${f_us:10}us ${p.scanner.all_tokens.len:10} ${p.scanner.text.len:10} ${ast_file.nr_lines:10} ${(f64(p.scanner.text.len) / p.scanner.all_tokens.len):13.3} ${p.errors.len:10}   ${f}')
	}
	hline()
	theader()
	hline()
	speed_mb_s := term.colorize(term.bright_yellow, '${(f64(total_bytes) / total_us):6.3f} MB/s')
	speed_lines_s := term.colorize(term.bright_yellow, '${(1_000_000 * f64(total_lines) / total_us):10.1f} lines/s')
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${total_lines:10} ${(f64(total_bytes) / total_tokens):13.3} ${total_errors:10}   Parser speed: ${speed_mb_s}, ${speed_lines_s}, ${nthreads} thread(s)')
}

fn new_parser(path string, comments_mode scanner.CommentsMode, table &ast.Table, pref_ &pref.Preferences) &parser.Parser {
	mut p := &parser.Parser{
		scanner: scanner.new_scanner_file(path, comments_mode, pref_) or { panic(err) }
		comments_mode: comments_mode
		table: table
		pref: pref_
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
