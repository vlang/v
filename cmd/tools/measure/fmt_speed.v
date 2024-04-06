import os
import time
import v.ast
import v.fmt
import v.pref
import v.parser
import v.errors
import v.scanner
import term

const skip_tests = os.getenv_opt('SKIP_TESTS') or { '' }.bool()
const comments_mode = scanner.CommentsMode.from(os.getenv('SCANNER_MODE')) or {
	scanner.CommentsMode.parse_comments
}

fn main() {
	dump(comments_mode)
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
	println('----------------------------------------------------------------------------------------------------------------------------------------------------------')
}

fn theader() {
	println('        Time     Tokens      Bytes      Lines   Bytes/Token     Errors   FMT.len')
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
	mut total_files := i64(0)
	mut total_fmt_len := i64(0)
	for f in files {
		mut table := ast.new_table()
		if f == '' {
			continue
		}
		if skip_tests && f.ends_with('_test.v') {
			continue
		}
		total_files++
		mut p := new_parser(f, comments_mode, table, pref_)
		ast_file := p.parse()
		///
		// do not measure the scanning, and parsing, but only the formatting:
		sw.restart()
		formatted_content := fmt.fmt(ast_file, mut table, pref_, false)
		f_us := sw.elapsed().microseconds()
		// eprint(formatted_content) // this should be identical to the output of `v fmt file.v`
		///
		total_us += f_us
		total_bytes += p.scanner.text.len
		total_tokens += p.scanner.all_tokens.len
		total_lines += ast_file.nr_lines
		total_errors += p.errors.len
		total_fmt_len += formatted_content.len
		println('${f_us:10}us ${p.scanner.all_tokens.len:10} ${p.scanner.text.len:10} ${ast_file.nr_lines:10} ${(f64(p.scanner.text.len) / p.scanner.all_tokens.len):13.3} ${p.errors.len:10}  ${formatted_content.len:8}   ${f}')
	}
	hline()
	theader()
	hline()
	speed_mb_s := term.colorize(term.bright_yellow, '${(f64(total_bytes) / total_us):6.3f} MB/s')
	speed_lines_s := term.colorize(term.bright_yellow, '${(1_000_000 * f64(total_lines) / total_us):10.1f} lines/s')
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${total_lines:10} ${(f64(total_bytes) / total_tokens):13.3} ${total_errors:10}   ${total_fmt_len:7}   FMT speed: ${speed_mb_s}, ${speed_lines_s}, ${nthreads:3} thread(s), ${total_files:5} files.')
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
