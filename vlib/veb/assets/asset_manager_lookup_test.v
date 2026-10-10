// Coverage for the parts of `veb.assets` that `assets_test.v` does not reach.
//
// `assets_test.v` drives `add`, `combine`, `include` and `cleanup_cache` through
// real files. What it never touches directly are the two minifiers — they are
// only observed as "the combined file has three lines" — and `exists`, which
// decides whether `add` should replace an entry.
//
// `minify_css` and `minify_js` are pure string functions, so their behaviour
// is asserted exactly. Both are marked `TODO: implement proper minification`
// upstream: they only drop blank lines, and the note at the end records what
// that does to the text.
import os
import veb.assets

const base_cache_dir = os.join_path(os.temp_dir(), 'veb_assets_lookup_test')

fn testsuite_begin() {
	os.mkdir_all(base_cache_dir) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(base_cache_dir) or {}
}

// unique_dir returns a cache directory owned by one test, so a stray file from
// another test cannot be mistaken for this one's.
fn unique_dir(name string) string {
	dir := os.join_path(base_cache_dir, name)
	os.rmdir_all(dir) or {}
	return dir
}

// manager_with writes `css` and `js` fixtures into a fresh cache directory and
// registers both, returning the asset manager and the two source paths.
fn manager_with(name string) !(assets.AssetManager, string, string) {
	dir := unique_dir(name)
	os.mkdir_all(dir) or { panic(err) }
	css_path := os.join_path(dir, 'in.css')
	os.write_file(css_path, '.a { color: red; }') or { panic(err) }
	js_path := os.join_path(dir, 'in.js')
	os.write_file(js_path, 'var a = 1;') or { panic(err) }
	mut am := assets.AssetManager{
		cache_dir: dir
	}
	am.add(.css, css_path, 'a.css')!
	am.add(.js, js_path, 'a.js')!
	return am, css_path, js_path
}

// ---------------------------------------------------------------------
// exists
// ---------------------------------------------------------------------

fn test_exists_finds_a_registered_include_name() {
	am, _, _ := manager_with('test_exists')!
	assert am.exists(.css, 'a.css') == true
	assert am.exists(.js, 'a.js') == true
}

fn test_exists_rejects_an_unregistered_include_name() {
	am, _, _ := manager_with('test_exists_missing')!
	assert am.exists(.css, 'a.js') == false
	assert am.exists(.js, 'a.css') == false
	assert am.exists(.css, 'not-registered.css') == false
	assert am.exists(.css, '') == false
}

fn test_exists_is_keyed_by_asset_type() {
	am, _, _ := manager_with('test_exists_type')!
	// The same include name under a different type is a different asset.
	assert am.exists(.css, 'a.css') == true
	assert am.exists(.js, 'a.css') == false
	assert am.exists(.css, 'a.js') == false
	// `.all` is not a type anything is registered under, but `get_assets(.all)`
	// returns the css and js lists concatenated, so an include name that
	// exists under either type answers true.
	assert am.exists(.all, 'a.css') == true
	assert am.exists(.all, 'a.js') == true
	assert am.exists(.all, 'not-registered.css') == false
}

// ---------------------------------------------------------------------
// get_assets
// ---------------------------------------------------------------------

fn test_get_assets_returns_only_the_requested_type() {
	am, css_path, js_path := manager_with('test_get_assets')!
	css_assets := am.get_assets(.css)
	assert css_assets.len == 1
	assert css_assets[0].include_name == 'a.css'
	js_assets := am.get_assets(.js)
	assert js_assets.len == 1
	assert js_assets[0].include_name == 'a.js'
	// Without `minify` set, `add` registers the path it was given, so the
	// cache directory is not involved at all.
	assert css_assets[0].file_path == css_path
	assert js_assets[0].file_path == js_path
}

fn test_get_assets_of_all_is_css_then_js() {
	am, _, _ := manager_with('test_get_assets_all')!
	every := am.get_assets(.all)
	assert every.len == 2
	assert every[0].kind == .css
	assert every[1].kind == .js
}

fn test_get_assets_of_an_empty_manager_is_empty() {
	am := assets.AssetManager{}
	assert am.get_assets(.css).len == 0
	assert am.get_assets(.js).len == 0
	assert am.get_assets(.all).len == 0
}

// ---------------------------------------------------------------------
// minify_css and minify_js
// ---------------------------------------------------------------------

fn test_minify_css_trims_each_line() {
	// Every line is trimmed and the non-empty ones are concatenated with no
	// separator at all, not even a newline.
	assert assets.minify_css('.one {\n\tcolor: #336699;\n}\n') == '.one {color: #336699;}'
}

fn test_minify_css_drops_blank_lines() {
	assert assets.minify_css('a { b: c }\n\n\nd { e: f }\n') == 'a { b: c }d { e: f }'
	assert assets.minify_css('   \n\t\n  \n') == ''
	assert assets.minify_css('\n\n') == ''
	assert assets.minify_css('') == ''
}

fn test_minify_css_keeps_a_single_line_as_is() {
	assert assets.minify_css('no newline') == 'no newline'
	// `trim_space` removes the carriage return as well, so a CRLF document
	// minifies to the same as an LF one.
	assert assets.minify_css('\r\n a \r\n b \r\n') == 'ab'
}

fn test_minify_js_joins_lines_with_a_space() {
	assert assets.minify_js('var a = 1;\nvar b = 2;\n') == 'var a = 1; var b = 2; '
	assert assets.minify_js('') == ''
}

fn test_minifiers_only_drop_blank_lines() {
	// NOTE: neither minifier touches anything else. A `//` comment survives, a
	// `/* */` comment survives, and attribute order is preserved: both are
	// still `TODO: implement proper minification` upstream. This asserts what
	// the code does today rather than what a minifier should do.
	css := 'a {\n  /* keep */\n  color: red;\n}\n'
	assert assets.minify_css(css) == 'a {/* keep */color: red;}'
	js := '// note\nvar a = 1;\n'
	assert assets.minify_js(js) == '// note var a = 1; '
}
