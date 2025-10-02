import os
import time
import term
import v.scanner
import file_lists
import v.pref

const skip_tests = os.getenv('SKIP_TESTS').bool()
const fuzzer_mode = os.getenv('VFUZZER').bool()
const comments_mode = scanner.CommentsMode.from(os.getenv('SCANNER_MODE')) or {
	scanner.CommentsMode.skip_comments
}

fn main() {
	if !fuzzer_mode {
		dump(comments_mode)
	}
	all_files := file_lists.expand_files(os.args#[1..])!
	process_files(all_files)!
}

fn hline() {
	if fuzzer_mode {
		return
	}
	println('----------------------------------------------------------------------------------------------------------------------------------------------------')
}

fn theader() {
	if fuzzer_mode {
		return
	}
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
	mut total_files := i64(0)
	for f in files {
		if f == '' {
			continue
		}
		if skip_tests && f.ends_with('_test.v') {
			continue
		}
		total_files++
		sw.restart()
		s := scanner.new_scanner_file(f, comments_mode, pref_)!
		f_us := sw.elapsed().microseconds()
		total_us += f_us
		total_bytes += s.text.len
		total_tokens += s.all_tokens.len
		total_lines += s.nr_lines
		total_errors += s.errors.len
		if !fuzzer_mode {
			println('${f_us:10}us ${s.all_tokens.len:10} ${s.text.len:10} ${s.nr_lines:10} ${(f64(s.text.len) / s.all_tokens.len):13.3f} ${s.errors.len:10}   ${f}')
		}
	}
	hline()
	theader()
	hline()
	speed_mb_s := term.colorize(term.bright_yellow, '${(f64(total_bytes) / total_us):6.3f} MB/s')
	speed_lines_s := term.colorize(term.bright_yellow, '${(1_000_000 * f64(total_lines) / total_us):10.1f} lines/s')
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${total_lines:10} ${(f64(total_bytes) / total_tokens):13.3} ${total_errors:10}   Scanner speed: ${speed_mb_s}, ${speed_lines_s}, ${nthreads:3} thread(s), ${total_files:5} files.')
}
