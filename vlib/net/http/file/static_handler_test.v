module file

import os
import net.http
import time

const probe_root = os.join_path(os.vtmp_dir(), 'v_testcov_file_handler_${os.getpid()}')

// A fixed tree: a text file, a css file, an index.html and a subfolder holding
// one file. Nothing here listens on a socket - the handler is called directly.
fn fixture_folder() string {
	os.rmdir_all(probe_root) or {}
	os.mkdir_all(probe_root) or { panic(err) }
	os.mkdir_all(os.join_path(probe_root, 'sub')) or { panic(err) }
	os.write_file(os.join_path(probe_root, 'hello.txt'), 'hello world') or { panic(err) }
	os.write_file(os.join_path(probe_root, 'page.css'), 'body{}') or { panic(err) }
	os.write_file(os.join_path(probe_root, 'index.html'), '<h1>index</h1>') or { panic(err) }
	os.write_file(os.join_path(probe_root, 'sub', 'deep.txt'), 'deep') or { panic(err) }
	return os.norm_path(os.real_path(probe_root))
}

fn handler(folder string, index_file string, auto_index bool) StaticHttpHandler {
	return StaticHttpHandler{
		params: StaticServeParams{
			folder:       folder
			index_file:   index_file
			auto_index:   auto_index
			filter_myexe: false
		}
	}
}

fn fetch(mut h StaticHttpHandler, url string) http.Response {
	return h.handle(http.Request{
		url: url
	})
}

fn test_a_regular_file_is_served_with_its_mime_type() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	txt := fetch(mut h, '/hello.txt')
	assert txt.status_code == 200
	assert txt.header.get(.content_type)? == 'text/plain; charset=utf-8'
	assert txt.body == 'hello world'
	css := fetch(mut h, '/page.css')
	assert css.status_code == 200
	assert css.header.get(.content_type)? == 'text/css; charset=utf-8'
	assert css.body == 'body{}'
	deep := fetch(mut h, '/sub/deep.txt')
	assert deep.status_code == 200
	assert deep.body == 'deep'
}

fn test_a_missing_file_yields_the_no_such_file_document() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	for path in ['/missing.txt', '/nope/', '/sub/missing.txt'] {
		r := fetch(mut h, path)
		assert r.status_code == 404
		assert r.header.get(.content_type)? == 'text/html; charset=utf-8'
		assert r.body == '<!DOCTYPE html><h1>no such file</h1>'
	}
}

fn test_the_index_file_answers_the_folder_root() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	for path in ['/', '/index.html'] {
		r := fetch(mut h, path)
		assert r.status_code == 200
		assert r.body == '<h1>index</h1>'
	}
	// with auto_index off the root is still answerable, because index.html is
	// there to be read directly
	mut plain := handler(folder, 'index.html', false)
	plain_root := fetch(mut plain, '/')
	assert plain_root.status_code == 200
	assert plain_root.body == '<h1>index</h1>'
}

fn test_auto_index_lists_the_folder_when_no_index_file_is_configured() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, '', true)
	r := fetch(mut h, '/')
	assert r.status_code == 200
	assert r.body.contains('<title>Index of local folder </title>')
	// uri_path is empty here, so the table starts with the "no entries" row
	assert r.body.contains('<td>---</td>')
	// directories are listed before files, and directories get no size cell
	sub_row := r.body.index('<a href="/sub">sub/</a>') or { -1 }
	file_row := r.body.index('<a href="/hello.txt">hello.txt</a>') or { -1 }
	assert sub_row > 0 && file_row > sub_row
}

fn test_a_directory_without_an_index_file_is_listed() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	// the trailing slash is trimmed, so both spellings land on the same folder.
	// the listing embeds its own generation time, so compare everything but that
	a := fetch(mut h, '/sub')
	b := fetch(mut h, '/sub/')
	assert a.status_code == 200
	assert a.body.all_before('<p>Server time:') == b.body.all_before('<p>Server time:')
	assert a.body.contains('<title>Index of local folder sub</title>')
	assert a.body.contains('<a href="/sub/deep.txt">deep.txt</a>')
	assert a.body.contains('<td>4</td>')
	// with auto_index off, a directory that has no index file is a 404
	mut plain := handler(folder, 'index.html', false)
	missing := fetch(mut plain, '/sub')
	assert missing.status_code == 404
	assert missing.body == '<!DOCTYPE html><h1>no such file</h1>'
}

fn test_the_query_string_and_percent_escapes_are_stripped() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	assert fetch(mut h, '/hello.txt?x=1').body == 'hello world'
	// %68 is 'h'; the URL is decoded before the path is built
	assert fetch(mut h, '/%68ello.txt').body == 'hello world'
	// an escape that decodes to nothing useful just misses the file
	assert fetch(mut h, '/%zz').status_code == 404
}

fn test_a_path_escaping_the_base_folder_is_forbidden() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	r := fetch(mut h, '/../hello.txt')
	assert r.status_code == 403
	assert r.body == '<h1>forbidden</h1>'
	// a `..` that stays inside the folder is still allowed
	inside := fetch(mut h, '/sub/../hello.txt')
	assert inside.status_code == 200
	assert inside.body == 'hello world'
}

fn test_a_sibling_with_a_shared_name_prefix_is_forbidden() {
	os.rmdir_all(probe_root) or {}
	os.mkdir_all(probe_root) or { panic(err) }
	sibling := '${probe_root}_secret'
	os.mkdir_all(sibling) or { panic(err) }
	os.write_file(os.join_path(sibling, 'leak.txt'), 'leaked') or { panic(err) }
	defer {
		os.rmdir_all(probe_root) or {}
		os.rmdir_all(sibling) or {}
	}
	folder := os.norm_path(os.real_path(probe_root))
	mut h := handler(folder, 'index.html', true)
	for parent in ['..', '%2e%2e'] {
		r := fetch(mut h, '/${parent}/${os.file_name(sibling)}/leak.txt')
		assert r.status_code == 403
		assert r.body == '<h1>forbidden</h1>'
	}
}

fn test_symlinks_cannot_escape_the_served_folder() {
	$if windows {
		return
	}
	folder := fixture_folder()
	sibling := '${folder}_secret'
	os.mkdir_all(sibling)!
	os.write_file(os.join_path(sibling, 'leak.txt'), 'leaked')!
	defer {
		os.rmdir_all(folder) or {}
		os.rmdir_all(sibling) or {}
	}
	os.symlink(sibling, os.join_path(folder, 'outside'))!
	os.symlink(os.join_path(folder, 'sub'), os.join_path(folder, 'inside'))!
	mut h := handler(folder, 'index.html', true)
	outside := fetch(mut h, '/outside/leak.txt')
	assert outside.status_code == 403
	assert outside.body == '<h1>forbidden</h1>'
	inside := fetch(mut h, '/inside/deep.txt')
	assert inside.status_code == 200
	assert inside.body == 'deep'
	os.rm(os.join_path(folder, 'index.html'))!
	os.symlink(os.join_path(sibling, 'leak.txt'), os.join_path(folder, 'index.html'))!
	assert fetch(mut h, '/index.html').status_code == 403
	for auto_index in [true, false] {
		mut indexed := handler(folder, 'index.html', auto_index)
		response := fetch(mut indexed, '/')
		assert response.status_code == 403
		assert response.body == '<h1>forbidden</h1>'
	}
	os.rm(os.join_path(folder, 'index.html'))!
	os.symlink(os.join_path(folder, 'sub', 'deep.txt'), os.join_path(folder, 'index.html'))!
	for auto_index in [true, false] {
		mut indexed := handler(folder, 'index.html', auto_index)
		response := fetch(mut indexed, '/')
		assert response.status_code == 200
		assert response.body == 'deep'
	}
}

fn test_serving_the_filesystem_root_preserves_descendant_access() {
	$if windows {
		return
	}
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(os.path_separator, 'index.html', false)
	r := fetch(mut h, '${folder}/hello.txt')
	assert r.status_code == 200
	assert r.body == 'hello world'
}

// NOTE: the parent-directory cell is emitted as
// `<td><a href="/sub/..">..</td>` - the anchor and the cell are both left
// unclosed. Current markup is asserted verbatim.
fn test_the_parent_row_keeps_its_unclosed_markup() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	mut h := handler(folder, 'index.html', true)
	r := fetch(mut h, '/sub')
	assert r.body.contains('<td><a href="/sub/..">..</td>')
}

fn test_path_to_entity_builds_uri_relative_urls() {
	folder := fixture_folder()
	defer {
		os.rmdir_all(folder) or {}
	}
	inner := path_to_entity(os.join_path(folder, 'sub', 'deep.txt'), 'sub')
	assert inner.fname == 'deep.txt'
	assert inner.url == '/sub/deep.txt'
	assert inner.typ == .regular
	root := path_to_entity(os.join_path(folder, 'hello.txt'), '')
	assert root.url == '/hello.txt'
	// the modification time is taken from the inode, so it is not the zero time
	assert root.mod_time.unix() > 0
	dir := path_to_entity(os.join_path(folder, 'sub'), '')
	assert dir.typ == .directory
	assert dir.url == '/sub'
	// an unreadable path still produces an entity, with a zero mtime
	ghost := path_to_entity(os.join_path(folder, 'nope.txt'), 'x')
	assert ghost.fname == 'nope.txt'
	assert ghost.url == '/x/nope.txt'
	assert ghost.mod_time == time.unix(0)
}
