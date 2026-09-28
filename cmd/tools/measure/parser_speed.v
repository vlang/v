import os
import time
import v.pref
import v.parser
import v.scanner
import v.token
import term
import file_lists

const skip_tests = os.getenv('SKIP_TESTS').bool()
const fuzzer_mode = os.getenv('VFUZZER').bool()

fn scanner_mode() scanner.Mode {
	return if os.getenv('SCANNER_MODE') in ['parse_comments', 'scan_comments'] {
		.scan_comments
	} else {
		.normal
	}
}

fn main() {
	if !fuzzer_mode {
		dump(scanner_mode())
	}
	all_files := file_lists.expand_files(os.args#[1..])!
	process_files(all_files)!
}

fn hline() {
	if fuzzer_mode {
		return
	}
	println('---------------------------------------------------------------------------------------------------------------------------------------------------')
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
		source := os.read_file(f)!
		token_count := count_tokens(f, source, pref_)
		// Do not include reading the source in the parser measurement.
		mut p := parser.Parser.new(pref_)

		sw.restart()
		p.parse_file(f)
		f_us := sw.elapsed().microseconds()

		total_us += f_us
		total_bytes += source.len
		total_tokens += token_count
		line_count := source.count('\n') + 1
		total_lines += line_count
		total_errors += p.diagnostics.len
		if !fuzzer_mode {
			println('${f_us:10}us ${token_count:10} ${source.len:10} ${line_count:10} ${(f64(source.len) / token_count):13.3} ${p.diagnostics.len:10}   ${f}')
		}
		total_files++
	}
	hline()
	theader()
	hline()
	speed_mb_s := term.colorize(term.bright_yellow, '${(f64(total_bytes) / total_us):6.3f} MB/s')
	speed_lines_s := term.colorize(term.bright_yellow, '${(1_000_000 * f64(total_lines) / total_us):10.1f} lines/s')
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${total_lines:10} ${(f64(total_bytes) / total_tokens):13.3} ${total_errors:10}   Parser speed: ${speed_mb_s}, ${speed_lines_s}, ${nthreads:3} thread(s), ${total_files:5} files.')
}

fn count_tokens(path string, source string, prefs &pref.Preferences) int {
	mut fileset := token.FileSet.new()
	mut file := fileset.add_file(path, source.len)
	file.index_lines(source)
	mut s := scanner.new_scanner(prefs, scanner_mode())
	s.init(file, source)
	mut count := 0
	for {
		count++
		if s.scan() == .eof {
			break
		}
	}
	return count
}
