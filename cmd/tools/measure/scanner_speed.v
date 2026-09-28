import os
import time
import term
import v.scanner
import v.token
import file_lists
import v.pref

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
	pref_ := pref.new_preferences()
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
		source := os.read_file(f)!
		mut fileset := token.FileSet.new()
		mut file := fileset.add_file(f, source.len)
		file.index_lines(source)
		mut s := scanner.new_scanner(pref_, scanner_mode())
		s.init(file, source)
		mut token_count := 0
		for {
			token_count++
			if s.scan() == .eof {
				break
			}
		}
		f_us := sw.elapsed().microseconds()
		total_us += f_us
		total_bytes += source.len
		total_tokens += token_count
		line_count := source.count('\n') + 1
		total_lines += line_count
		total_errors += s.diagnostics.len
		if !fuzzer_mode {
			println('${f_us:10}us ${token_count:10} ${source.len:10} ${line_count:10} ${(f64(source.len) / token_count):13.3f} ${s.diagnostics.len:10}   ${f}')
		}
	}
	hline()
	theader()
	hline()
	speed_mb_s := term.colorize(term.bright_yellow, '${(f64(total_bytes) / total_us):6.3f} MB/s')
	speed_lines_s := term.colorize(term.bright_yellow, '${(1_000_000 * f64(total_lines) / total_us):10.1f} lines/s')
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${total_lines:10} ${(f64(total_bytes) / total_tokens):13.3} ${total_errors:10}   Scanner speed: ${speed_mb_s}, ${speed_lines_s}, ${nthreads:3} thread(s), ${total_files:5} files.')
}
