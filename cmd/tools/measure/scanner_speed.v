import os
import time
import v.scanner
import v.pref

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
		sw.restart()
		s := scanner.new_scanner_file(f, .skip_comments, pref)!
		f_us := sw.elapsed().microseconds()
		total_us += f_us
		total_bytes += s.text.len
		total_tokens += s.all_tokens.len
		println('${f_us:10}us ${s.all_tokens.len:10} ${s.text.len:10} ${(f64(s.text.len) / s.all_tokens.len):7.3f} $f')
	}
	println('${total_us:10}us ${total_tokens:10} ${total_bytes:10} ${(f64(total_tokens) / total_bytes):7.3f} | speed: ${(f64(total_bytes) / total_us):2.5f} MB/s')
}
