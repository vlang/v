import veb.assets
import os

const base_cache_dir = os.join_path(os.vtmp_dir(), 'veb_assets_test_cache')

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

fn test_add() {
	mut am := assets.AssetManager{}

	mut errored := false
	am.add(.css, 'test.css', 'test.css') or { errored = true }
	assert errored == true, 'am.add should error'

	errored = false
	am.add(.css, get_test_file_path('test1.css'), 'included.css') or {
		eprintln(err)
		errored = true
	}
	assert errored == false, 'am.add should not error'

	css_assets := am.get_assets(.css)
	assert css_assets.len == 1
	assert css_assets[0].file_path == get_test_file_path('test1.css')
	assert css_assets[0].include_name == 'included.css'
}

fn test_add_minify_missing_cache_dir() {
	mut am := assets.AssetManager{
		minify: true
	}
	mut errored := false
	am.add(.js, get_test_file_path('test1.css'), 'included.js') or {
		assert err.msg() == 'cannot minify asset: cache directory is not valid'
		errored = true
	}

	assert errored == true, 'am.add should return an error'
}

fn test_add_minified() {
	mut am := assets.AssetManager{
		minify:    true
		cache_dir: cache_dir('test_add_minified')
	}
	clean_cache_dir(am.cache_dir)

	am.add(.js, get_test_file_path('test1.js'), 'included.js')!

	js_assets := am.get_assets(.js)
	assert js_assets.len == 1
	assert js_assets[0].file_path.starts_with(am.cache_dir) == true
}

fn test_combine() {
	mut am := assets.AssetManager{
		cache_dir: cache_dir('test_combine')
	}
	clean_cache_dir(am.cache_dir)

	am.add(.css, get_test_file_path('test1.css'), 'test1.css')!
	am.add(.css, get_test_file_path('test2.css'), 'test2.css')!

	combined_path := am.combine(.css)!
	combined := os.read_file(combined_path)!

	expected := get_test_file_contents('test1.css') + '\n' + get_test_file_contents('test2.css') +
		'\n'
	assert combined == expected
}

fn test_combine_minified() {
	// minify test is simple for now, because assets are not properly minified yet
	mut am := assets.AssetManager{
		cache_dir: cache_dir('test_combine_minified')
		minify:    true
	}
	clean_cache_dir(am.cache_dir)

	am.add(.css, get_test_file_path('test1.css'), 'test1.css')!
	am.add(.css, get_test_file_path('test2.css'), 'test2.css')!

	combined_path := am.combine(.css)!
	combined := os.read_file(combined_path)!

	// minified version should be 2 lines + one extra newline
	assert combined.split('\n').len == 3
}

fn test_minify_cache_last_modified() {
	mut am := assets.AssetManager{
		minify:    true
		cache_dir: cache_dir('test_cache_last_modified')
	}
	clean_cache_dir(am.cache_dir)

	// first we write the file and add it
	am.add(.js, get_test_file_path('test1.js'), 'included.js')!
	mut js_assets := am.get_assets(.js)
	assert js_assets.len == 1
	old_cached_path := js_assets[0].file_path

	// then we only add the file, the file is not modified so the "last modified is the same".
	// we expect that the asset manager doesn't cache a minified file if it hasn't been changed
	// the last time it was added
	am.add(.js, os.join_path(base_cache_dir, 'test1.js'), 'included.js')!

	js_assets = am.get_assets(.js)
	// check if the file isn't added twice
	assert js_assets.len == 1
	// if the file path was not modified, veb.assets didn't overwrite the file
	assert js_assets[0].file_path == old_cached_path
}

fn test_cleanup_cache() {
	mut am := assets.AssetManager{
		minify:    true
		cache_dir: cache_dir('test_cleanup_cache')
	}
	clean_cache_dir(am.cache_dir)
	// manually make the cache dir
	os.mkdir_all(am.cache_dir) or {}

	// write a file to the cache dir isn't added to the asset manager to represent
	// a previously cached file
	path1 := os.join_path(am.cache_dir, 'test1.css')
	os.write_file(path1, 'h1 { color: red; }')!
	assert os.exists(path1) == true

	// add a file to the asset manager and write it
	am.add(.css, get_test_file_path('test2.css'), 'test2.css')!
	css_assets := am.get_assets(.css)
	// get the cached path
	assert css_assets.len == 1
	path2 := css_assets[0].file_path
	assert os.exists(path2) == true

	am.cleanup_cache()!

	// the first asset wasn't added to the asset manager, so it should not exist
	assert os.exists(path1) == false
	assert os.exists(path2) == true
}

fn test_include() {
	mut am := assets.AssetManager{}

	css_path := get_test_file_path('test1.css')
	js_path := get_test_file_path('test1.js')
	am.add(.css, css_path, 'other.css')!
	am.add(.js, js_path, 'js/test.js')!

	assert am.include(.css, 'other.css') == '<link rel="stylesheet" href="${css_path}">'
	assert am.include(.js, 'js/test.js') == '<script src="${js_path}"></script>'
}
