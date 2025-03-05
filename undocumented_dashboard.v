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
			comments << line_trimmed
		} else if line_trimmed.starts_with('pub fn') {
			current_fn = line_trimmed
			if comments.len == 0 {
				undocumented << current_fn
			}
			comments.clear()
		} else {
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
	mut total_methods := 0
	mut total_undoc := 0
	for stat in stats {
		total_methods += stat.pub_methods
		total_undoc += stat.undocumented_count
	}
	overall_coverage := if total_methods > 0 {
		100.0 - (f64(total_undoc) * 100.0 / f64(total_methods))
	} else {
		100.0
	}

	mut html_head :=
		'<!DOCTYPE html>\n<html>\n<head><title>VLib Docs Coverage</title>
	<style>body{font-family:sans-serif;margin:20px;background:#121212;color:#fff}table{width:100%;border-collapse:collapse}th,td{border:1px solid #444;padding:8px;text-align:left}th{background:#1e1e1e;color:#fff}tr:nth-child(even){background:#222}a{color:#62baff;text-decoration:none}a:hover{text-decoration:underline}ul{margin:5px 0;padding-left:20px} .coverage-high{background:#4CAF50;color:#fff} .coverage-medium{background:#FFC107;color:#000} .coverage-low{background:#F44336;color:#fff} .coverage-very-low{background:#D32F2F;color:#fff}</style>
	<style>.progressbar-outer{width:100%;background-color:#333;height:20px;border-radius:4px;margin:10px 0;} @keyframes fillBar {0% {width:0%;}100% {width:' +
		'${overall_coverage}%;}} .progressbar-inner{animation:fillBar 0.7s ease-in-out forwards; background-color:#4CAF50;height:100%;border-radius:4px; display: flex; align-items: center; justify-content: center;} @keyframes fadeIn {0% {opacity: 0;}100% {opacity: 1;}} .progressbar-label{animation:fadeIn 0.7s ease-in-out forwards;}</style>
	</head>'

	mut html_body := '<body><h1>Documentation Coverage</h1>'

	html_body += '<p>Total public methods: <span id="total_methods">0</span></p>
	<p>Total undocumented methods: <span id="total_undoc">0</span></p>
	<p>Overall coverage: </p>
	<div class="progressbar-outer">
	<div class="progressbar-inner"><span class="progressbar-label">${overall_coverage:.2f}%</span></div>
	</div><br>'

	html_body += '<table><tr><th>Library</th><th>Public Methods</th><th>Undocumented</th><th>Coverage %</th></tr>'
	mut rows := ''
	for i, stat in stats {
		coverage := if stat.pub_methods > 0 {
			100.0 - (f64(stat.undocumented_count) * 100.0 / f64(stat.pub_methods))
		} else {
			100.0
		}

		mut coverage_class := 'coverage-high'
		if coverage < 25.0 {
			coverage_class = 'coverage-very-low'
		} else if coverage < 50.0 {
			coverage_class = 'coverage-low'
		} else if coverage < 80.0 {
			coverage_class = 'coverage-medium'
		}

		click_id := 'undoc_${i}'
		rows += '<tr><td>${stat.name}</td><td>${stat.pub_methods}</td>
		<td><a href="javascript:void(0)" onClick="showUndocumented(\'${click_id}\')">${stat.undocumented_count}</a>
		<div id="${click_id}" style="display:none;background:#333;padding:10px;border-radius:5px"><ul>'
		for meth in stat.undocumented_methods {
			rows += '<li>${meth.replace('<', '&lt;').replace('>', '&gt;')}</li>'
		}
		rows += '</ul></div></td><td class="${coverage_class}">${coverage:.2f}%</td></tr>'
	}

	mut html_footer := '</table><script>function showUndocumented(id){var div=document.getElementById(id);div.style.display=(div.style.display==="none")?"block":"none";}</script>'
	html_footer += '<script>
	function animateValue(id, start, end, duration) {
	  let obj = document.getElementById(id);
	  let range = end - start;
	  let startTime = performance.now();

	  function update() {
	    let now = performance.now();
	    let elapsed = now - startTime;
	    if (elapsed > duration) elapsed = duration;
	    let current = start + (range * (elapsed / duration));
	    current = Math.round(current);
	    obj.textContent = current;
	    if (elapsed < duration) {
	      requestAnimationFrame(update);
	    }
	  }
	  requestAnimationFrame(update);
	}
	window.onload=function(){
	  animateValue("total_methods",0,${total_methods},700);
	  animateValue("total_undoc",0,${total_undoc},700);
	};
	</script></body></html>'
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
