import os

const vlib_path = './vlib/'

struct LibStats {
	name                 string
	pub_methods          int
	undocumented_count   int
	undocumented_methods []string
}

fn collect_undocumented_functions_in_file(file string) []string {
	contents := os.read_file(file) or { return [] }
	lines := contents.split('\n')
	mut undocumented := []string{}
	mut comments := []string{}
	mut current_fn := ''

	for line in lines {
		line_trimmed := line.trim_space()

		if line_trimmed.starts_with('//') || line_trimmed.starts_with('@[') {
			// Consider these lines as "doc/attribute" lines
			comments << line_trimmed
		} else if line_trimmed.starts_with('pub fn') {
			// Check if no 'doc/attribute' lines were recorded
			current_fn = line_trimmed
			if comments.len == 0 {
				undocumented << current_fn
			}
			comments.clear()
		} else {
			// If it's anything else, reset the recorded lines
			comments.clear()
		}
	}

	return undocumented
}

fn count_pub_methods(file string) int {
	contents := os.read_file(file) or { return 0 }
	lines := contents.split('\n')
	return lines.filter(it.trim_space().starts_with('pub fn')).len
}

fn analyze_libs() ![]LibStats {
	mut stats := []LibStats{}
	if !os.is_dir(vlib_path) {
		return error('vlib directory not found')
	}
	for lib in os.ls(vlib_path)! {
		lib_path := os.join_path(vlib_path, lib)
		if !os.is_dir(lib_path) {
			continue
		}
		mut total_methods := 0
		mut undocumented_methods := []string{}
		for file in os.walk_ext(lib_path, '.v') {
			if file.ends_with('_test.v') {
				continue
			}
			new_undoc := collect_undocumented_functions_in_file(file)
			total_methods += new_undoc.len + (count_pub_methods(file) - new_undoc.len)
			undocumented_methods << new_undoc
		}
		stats << LibStats{
			name:                 lib
			pub_methods:          total_methods
			undocumented_count:   undocumented_methods.len
			undocumented_methods: undocumented_methods
		}
	}
	return stats
}

fn generate_html(stats []LibStats) string {
	html_head := '<!DOCTYPE html>\\n<html>\\n<head><title>VLib Docs Coverage</title></head>\n<body>'
	mut html_body := '<h1>Documentation Coverage</h1><table border="1">\n<tr><th>Library</th><th>Public Methods</th><th>Undocumented</th><th>Coverage %</th></tr>'
	mut rows := ''
	for i, stat in stats {
		total := f64(stat.pub_methods)
		undoc := f64(stat.undocumented_count)
		coverage := if total > 0 { 100.0 - (undoc * 100.0 / total) } else { 100.0 }

		// Create a clickable `<a>` to show the undocumented methods
		click_id := 'undoc_${i}'
		rows += '<tr><td>${stat.name}</td><td>${stat.pub_methods}</td>'
		rows += '<td><a href="javascript:void(0)" onClick="showUndocumented(\'${click_id}\')">${stat.undocumented_count}</a>'
		rows += '<div id="${click_id}" style="display:none"><ul>'
		for meth in stat.undocumented_methods {
			rows += '<li>${meth.replace('<', '&lt;').replace('>', '&gt;')}</li>'
		}
		rows += '</ul></div></td><td>${coverage:.2f}%</td></tr>'
	}
	html_footer := '</table>\n<script>\nfunction showUndocumented(id){\n  var div = document.getElementById(id);\n  div.style.display = (div.style.display==="none")?"block":"none";\n}\n</script>\n</body>\n</html>'
	return html_head + html_body + rows + html_footer
}

fn main() {
	stats := analyze_libs()!
	html := generate_html(stats)
	os.write_file('doc_coverage.html', html) or {
		eprintln('Error writing HTML file')
		exit(1)
	}
	println('Generated doc_coverage.html')
}
