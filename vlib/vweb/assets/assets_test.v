import vweb.assets
import os

const base_cache_dir = os.join_path(os.temp_dir(), 'v', 'assets_test_cache')

fn testsuite_begin() {
	os.mkdir_all(base_cache_dir) or {}
}

fn testsuite_end() {
	os.rmdir_all(base_cache_dir) or {}
}

// clean_cache_dir used before and after tests that write to a cache directory.
// Because of parallel compilation and therefore test running,
// unique cache dirs are needed per test function.
fn clean_cache_dir(dir string) {
	os.rmdir_all(dir) or {}
}

fn cache_dir(test_name string) string {
	return os.join_path(base_cache_dir, test_name)
}

fn get_test_file_path(file string) string {
	path := os.join_path(base_cache_dir, file)
	os.rm(path) or {}
	os.write_file(path, get_test_file_contents(file)) or { panic(err) }
	return path
}

fn get_test_file_contents(file string) string {
	contents := match file {
		'test1.js' { '{"one": 1}\n' }
		'test2.js' { '{"two": 2}\n' }
		'test1.css' { '.one {\n\tcolor: #336699;\n}\n' }
		'test2.css' { '.two {\n\tcolor: #996633;\n}\n' }
		else { 'wibble\n' }
	}
	return contents
}

fn test_set_cache() {
	mut am := assets.new_manager()
	am.cache_dir = 'cache'
}

fn test_set_minify() {
	mut am := assets.new_manager()
	am.minify = true
}

fn test_add() {
	mut am := assets.new_manager()
	assert am.add('css', 'testx.css') == false
	assert am.add('css', get_test_file_path('test1.css')) == true
	assert am.add('js', get_test_file_path('test1.js')) == true
	// assert am.add('css', get_test_file_path('test2.js')) == false // TODO: test extension on add
}

fn test_add_css() {
	mut am := assets.new_manager()
	assert am.add_css('testx.css') == false
	assert am.add_css(get_test_file_path('test1.css')) == true
	// assert am.add_css(get_test_file_path('test1.js')) == false // TODO: test extension on add
}

fn test_add_js() {
	mut am := assets.new_manager()
	assert am.add_js('testx.js') == false
	assert am.add_css(get_test_file_path('test1.js')) == true
	// assert am.add_css(get_test_file_path('test1.css')) == false // TODO: test extension on add
}

fn test_combine_css() {
	mut am := assets.new_manager()
	am.cache_dir = cache_dir('test_combine_css')
	clean_cache_dir(am.cache_dir)
	am.add_css(get_test_file_path('test1.css'))
	am.add_css(get_test_file_path('test2.css'))
	// TODO: How do I test non-minified, is there a "here doc" format that keeps formatting?
	am.minify = true
	expected := '.one { color: #336699; } .two { color: #996633; } '
	actual := am.combine_css(false)
	assert actual == expected
	assert actual.contains(expected)
	// Test cache path doesn't change when input files and minify setting do not.
	path1 := am.combine_css(true)
	clean_cache_dir(am.cache_dir)
	path2 := am.combine_css(true)
	assert path1 == path2
	clean_cache_dir(am.cache_dir)
}

fn test_combine_js() {
	mut am := assets.new_manager()
	am.cache_dir = cache_dir('test_combine_js')
	clean_cache_dir(am.cache_dir)
	am.add_js(get_test_file_path('test1.js'))
	am.add_js(get_test_file_path('test2.js'))
	expected1 := '{"one": 1}'
	expected2 := '{"two": 2}'
	expected := expected1 + '\n' + expected2 + '\n'
	actual := am.combine_js(false)
	assert actual == expected
	assert actual.contains(expected)
	assert actual.contains(expected1)
	assert actual.contains(expected2)
	am.minify = true
	clean_cache_dir(am.cache_dir)
	expected3 := expected1 + ' ' + expected2 + ' '
	actual2 := am.combine_js(false)
	assert actual2 == expected3
	assert actual2.contains(expected3)
	// Test cache path doesn't change when input files and minify setting do not.
	path1 := am.combine_js(true)
	clean_cache_dir(am.cache_dir)
	path2 := am.combine_js(true)
	assert path1 == path2
	clean_cache_dir(am.cache_dir)
}

fn test_include_css() {
	mut am := assets.new_manager()
	file1 := get_test_file_path('test1.css')
	am.add_css(file1)
	expected := '<link rel="stylesheet" href="$file1">\n'
	actual := am.include_css(false)
	assert actual == expected
	assert actual.contains(expected)
	// Two lines of output.
	file2 := get_test_file_path('test2.css')
	am.add_css(file2)
	am.cache_dir = cache_dir('test_include_css')
	clean_cache_dir(am.cache_dir)
	expected2 := expected + '<link rel="stylesheet" href="$file2">\n'
	actual2 := am.include_css(false)
	assert actual2 == expected2
	assert actual2.contains(expected2)
	// Combined output.
	clean_cache_dir(am.cache_dir)
	actual3 := am.include_css(true)
	assert actual3.contains(expected2) == false
	assert actual3.starts_with('<link rel="stylesheet" href="$am.cache_dir/') == true
	// Test cache path doesn't change when input files and minify setting do not.
	clean_cache_dir(am.cache_dir)
	actual4 := am.include_css(true)
	assert actual4 == actual3
	clean_cache_dir(am.cache_dir)
}

fn test_include_js() {
	mut am := assets.new_manager()
	file1 := get_test_file_path('test1.js')
	am.add_js(file1)
	expected := '<script type="text/javascript" src="$file1"></script>\n'
	actual := am.include_js(false)
	assert actual == expected
	assert actual.contains(expected)
	// Two lines of output.
	file2 := get_test_file_path('test2.js')
	am.add_js(file2)
	am.cache_dir = cache_dir('test_include_js')
	clean_cache_dir(am.cache_dir)
	expected2 := expected + '<script type="text/javascript" src="$file2"></script>\n'
	actual2 := am.include_js(false)
	assert actual2 == expected2
	assert actual2.contains(expected2)
	// Combined output.
	clean_cache_dir(am.cache_dir)
	actual3 := am.include_js(true)
	assert actual3.contains(expected2) == false
	assert actual3.starts_with('<script type="text/javascript" src="$am.cache_dir/')
	// Test cache path doesn't change when input files and minify setting do not.
	clean_cache_dir(am.cache_dir)
	actual4 := am.include_js(true)
	assert actual4 == actual3
	clean_cache_dir(am.cache_dir)
}
