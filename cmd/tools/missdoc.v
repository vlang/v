// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
import os

struct UndocumentedFN {
	line      int
	signature string
}

fn collect(path string, mut l []string, f fn (string, mut []string)) {
	if !os.is_dir(path) {
		return
	}
	mut files := os.ls(path) or { return }
	for file in files {
		p := path + os.path_separator + file
		if os.is_dir(p) && !os.is_link(p) {
			collect(p, mut l, f)
		} else if os.exists(p) {
			f(p, mut l)
		}
	}
	return
}

fn report_undocumented_functions_in_path(path string) {
	mut files := []string{}
	collect_fn := fn (path string, mut l []string) {
		if os.file_ext(path) == '.v' {
			l << os.real_path(path)
		}
	}
	collect(path, mut files, collect_fn)
	for f in files {
		contents := os.read_file(f) or { panic(err) }
		lines := contents.split('\n')
		if f.ends_with('_test.v') {
			continue
		}
		mut info := []UndocumentedFN{}
		for i, line in lines {
			if line.starts_with('pub fn') ||
				(line.starts_with('fn ') && !(line.starts_with('fn C.') || line.starts_with('fn main'))) {
				// println('Match: $line')
				if i > 0 && lines.len > 0 {
					mut line_above := lines[i - 1]
					if !line_above.starts_with('//') {
						mut grab := true
						for j := i - 2; j >= 0; j-- {
							prev_line := lines[j]
							if prev_line.contains('}') {
								break
							} else if prev_line.starts_with('[') {
								continue
							} else if prev_line.starts_with('//') {
								grab = false
								break
							}
						}
						if grab {
							clean_line := line.all_before_last(' {')
							info << UndocumentedFN{i + 1, clean_line}
						}
					}
				}
			}
		}
		if info.len > 0 {
			for undocumented_fn in info {
				println('$f:$undocumented_fn.line:0:$undocumented_fn.signature')
			}
		}
	}
}

fn main() {
	if os.args.len == 1 {
		println('Usage: missdoc PATH \nPrints all V functions in .v files under PATH/, that do not yet have documentation comments.')
		exit(1)
	}
	for path in os.args[1..] {
		report_undocumented_functions_in_path(path)
	}
}
