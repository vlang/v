module dtm2

import os
import time

const test_root_name = 'dtm2_test'

fn dtm2_test_root() string {
	return os.join_path(os.vtmp_dir(), '${test_root_name}_${os.getpid()}')
}

fn dtm2_outside_template_path() string {
	return os.join_path(os.vtmp_dir(), '${test_root_name}_${os.getpid()}_outside.html')
}

fn testsuite_begin() {
	root := dtm2_test_root()
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'partials'))!
	outside_path := dtm2_outside_template_path()
	os.rm(outside_path) or {}
	os.write_file(outside_path, '<p>@title outside</p>')!
	os.write_file(os.join_path(root, 'page.html'), '<main>@title @body @missing</main>')!
	os.write_file(os.join_path(root, 'invalid.css'), 'body { color: red; }')!
	os.write_file(os.join_path(root, 'raw.txt'), 'Raw: @body')!
	os.write_file(os.join_path(root, 'with_include.html'),
		'<header>@include "partials/nav"</header><main>@title</main>')!
	os.write_file(os.join_path(root, 'partials', 'nav.html'), '<nav>@title</nav>')!
	os.write_file(os.join_path(root, 'include_outside_relative.html'),
		'@include "../${os.base(outside_path)}"')!
	os.write_file(os.join_path(root, 'include_outside_absolute.html'), '@include "${outside_path}"')!
	os.write_file(os.join_path(root, 'reload.html'), '<p>@title</p>')!
	os.write_file(os.join_path(root, 'pinned.html'), '<em>@title</em>')!
	os.write_file(os.join_path(root, 'path_alias.html'), '<section>@title</section>')!
	os.write_file(os.join_path(root, 'prefix.html'), '<p>@unknown</p>')!
	os.write_file(os.join_path(root, 'recursive.html'), '@include "recursive"')!
	os.write_file(os.join_path(root, 'compress.html'), '<div>\n <span>@title</span>\n</div>')!
	os.write_file(os.join_path(root, 'feed.xml'), '<feed><title>@title</title></feed>')!
	os.write_file(os.join_path(root, 'custom.page'), '<article>@title</article>')!
	os.write_file(os.join_path(root, 'custom.note'), 'Note: @body')!
	os.write_file(os.join_path(root, 'custom.view'), '<section>@title</section>')!
	os.write_file(os.join_path(root, 'custom.mail'), 'Subject: @title')!
	os.write_file(os.join_path(root, 'auto.fragment'), '<aside>@title</aside>')!
	os.write_file(os.join_path(root, 'auto.message'), 'Auto: @title')!
	os.write_file(os.join_path(root, 'bad.path'), '<p>@title</p>')!
	os.write_file(os.join_path(root, 'extensions.json'), '{"html":[".view"],"text":["mail"]}')!
	os.write_file(os.join_path(root, default_extension_config_filename),
		'{"html":[".fragment"],"text":[".message"]}')!
	os.write_file(os.join_path(root, 'invalid_extensions.json'),
		'{"html":["../bad"],"text":["bad/value",".message"]}')!
}

fn testsuite_end() {
	os.rmdir_all(dtm2_test_root()) or {}
	os.rm(dtm2_outside_template_path()) or {}
}

fn new_test_manager(compress_html bool) &Manager {
	return initialize(
		template_dir:  dtm2_test_root()
		compress_html: compress_html
	)
}

fn test_render_placeholders_escape_and_preserve_missing() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'Hello'
		'body':  '<script>alert(1)</script>'
	}
	rendered := manager.expand('page.html', placeholders: &placeholders)
	assert rendered == '<main>Hello &lt;script&gt;alert(1)&lt;/script&gt; @missing</main>\n'
	assert manager.compiled_template_count() == 1
}

fn test_include_and_compiled_template_registry() {
	mut manager := new_test_manager(true)
	placeholders := {
		'title': 'Home'
	}
	rendered := manager.expand('with_include.html', placeholders: &placeholders)
	assert rendered == '<header><nav>Home</nav></header><main>Home</main>'
	assert manager.compiled_template_count() == 1
	_ = manager.expand('with_include.html', placeholders: &placeholders)
	assert manager.compiled_template_count() == 1
}

fn test_include_html_suffix_keeps_only_allowed_tags() {
	mut manager := new_test_manager(false)
	placeholders := {
		'body_#includehtml': '<thead><tr><td>safe</td></tr></thead><script>bad</script>'
		'title':             'HTML'
	}
	rendered := manager.expand('page.html', placeholders: &placeholders)
	assert rendered.contains('<thead><tr><td>safe</td></tr></thead>')
	assert rendered.contains('&lt;script&gt;bad&lt;/script&gt;')
}

fn test_text_mode_always_escapes_html() {
	mut manager := new_test_manager(false)
	placeholders := {
		'body_#includehtml': '<span>text</span>'
	}
	rendered := manager.expand('raw.txt', placeholders: &placeholders)
	assert rendered == 'Raw: &lt;span&gt;text&lt;/span&gt;\n'
}

fn test_reload_modified_template_without_render_cache() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'One'
	}
	first := manager.expand('reload.html', placeholders: &placeholders)
	assert first == '<p>One</p>\n'
	os.write_file(os.join_path(dtm2_test_root(), 'reload.html'), '<strong>@title</strong>')!
	second := manager.expand('reload.html', placeholders: &placeholders)
	assert second == '<strong>One</strong>\n'
	assert manager.compiled_template_count() == 1
}

fn test_reload_same_second_same_size_template_content_change() {
	path := os.join_path(dtm2_test_root(), 'same_size.html')
	fixed_time := i64(2_306_102_495)
	os.write_file(path, '<p>@title A</p>')!
	os.utime(path, fixed_time, fixed_time)!
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'One'
	}
	first := manager.expand('same_size.html', placeholders: &placeholders)
	assert first == '<p>One A</p>\n'
	os.write_file(path, '<b>@title B</b>')!
	os.utime(path, fixed_time, fixed_time)!
	second := manager.expand('same_size.html', placeholders: &placeholders)
	assert second == '<b>One B</b>\n'
	assert manager.compiled_template_count() == 1
}

fn test_reload_modified_include_without_render_cache() {
	mut manager := new_test_manager(true)
	placeholders := {
		'title': 'Home'
	}
	first := manager.expand('with_include.html', placeholders: &placeholders)
	assert first == '<header><nav>Home</nav></header><main>Home</main>'
	time.sleep(1100 * time.millisecond)
	os.write_file(os.join_path(dtm2_test_root(), 'partials', 'nav.html'),
		'<nav class="updated">@title</nav>')!
	second := manager.expand('with_include.html', placeholders: &placeholders)
	assert second == '<header><nav class="updated">Home</nav></header><main>Home</main>'
	assert manager.compiled_template_count() == 1
}

fn test_reload_disabled_keeps_cached_template_tree() {
	mut manager := initialize(
		template_dir:              dtm2_test_root()
		compress_html:             false
		reload_modified_templates: false
	)
	placeholders := {
		'title': 'Pinned'
	}
	first := manager.expand('pinned.html', placeholders: &placeholders)
	assert first == '<em>Pinned</em>\n'
	os.write_file(os.join_path(dtm2_test_root(), 'pinned.html'), '<strong>@title</strong>')!
	second := manager.expand('pinned.html', placeholders: &placeholders)
	assert second == '<em>Pinned</em>\n'
	assert manager.compiled_template_count() == 1
}

fn test_relative_and_absolute_paths_share_compiled_template() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'Alias'
	}
	relative := manager.expand('path_alias.html', placeholders: &placeholders)
	absolute := manager.expand(os.join_path(dtm2_test_root(), 'path_alias.html'),
		placeholders: &placeholders
	)
	assert relative == '<section>Alias</section>\n'
	assert absolute == relative
	assert manager.compiled_template_count() == 1
}

fn test_template_path_cannot_escape_template_dir() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'Escape'
	}
	relative_escape := manager.expand('../${os.base(dtm2_outside_template_path())}',
		placeholders: &placeholders
	)
	absolute_escape := manager.expand(dtm2_outside_template_path(), placeholders: &placeholders)
	assert relative_escape == internal_server_error
	assert absolute_escape == internal_server_error
	assert manager.compiled_template_count() == 0
}

fn test_cached_template_path_revalidates_symlink_escape() {
	$if windows {
		return
	}
	path := os.join_path(dtm2_test_root(), 'symlink_swap.html')
	os.write_file(path, '<p>@title safe</p>')!
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'Link'
	}
	first := manager.expand('symlink_swap.html', placeholders: &placeholders)
	assert first == '<p>Link safe</p>\n'
	os.rm(path)!
	os.symlink(dtm2_outside_template_path(), path) or { return }
	escaped := manager.expand('symlink_swap.html', placeholders: &placeholders)
	assert escaped == internal_server_error
}

fn test_include_path_cannot_escape_template_dir() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': 'Escape'
	}
	relative_include := manager.expand('include_outside_relative.html', placeholders: &placeholders)
	absolute_include := manager.expand('include_outside_absolute.html', placeholders: &placeholders)
	assert relative_include == internal_server_error
	assert absolute_include == internal_server_error
	assert manager.compiled_template_count() == 0
}

fn test_custom_missing_placeholder_prefix() {
	mut manager := new_test_manager(false)
	placeholders := map[string]string{}
	rendered := manager.expand('prefix.html',
		placeholders:               &placeholders
		missing_placeholder_prefix: '?'
	)
	assert rendered == '<p>?unknown</p>\n'
}

fn test_recursive_include_returns_internal_server_error() {
	mut manager := new_test_manager(false)
	placeholders := map[string]string{}
	rendered := manager.expand('recursive.html', placeholders: &placeholders)
	assert rendered == internal_server_error
	assert manager.compiled_template_count() == 0
}

fn test_html_compression_is_deterministic() {
	mut manager := new_test_manager(true)
	placeholders := {
		'title': 'Compact'
	}
	rendered := manager.expand('compress.html', placeholders: &placeholders)
	assert rendered == '<div><span>Compact</span></div>'
}

fn test_xml_template_uses_html_rendering_mode() {
	mut manager := new_test_manager(false)
	placeholders := {
		'title': '<escaped>'
	}
	rendered := manager.expand('feed.xml', placeholders: &placeholders)
	assert rendered == '<feed><title>&lt;escaped&gt;</title></feed>\n'
}

fn test_custom_template_extension_map() {
	mut manager := initialize(
		template_dir:        dtm2_test_root()
		template_extensions: {
			'.page': TemplateType.html
			'note':  TemplateType.text
		}
	)
	html_placeholders := {
		'title_#includehtml': '<span>custom</span><script>blocked</script>'
	}
	text_placeholders := {
		'body_#includehtml': '<span>custom</span>'
	}
	html := manager.expand('custom.page', placeholders: &html_placeholders)
	text := manager.expand('custom.note', placeholders: &text_placeholders)
	assert html.contains('<span>custom</span>&lt;script&gt;blocked&lt;/script&gt;')
	assert text == 'Note: &lt;span&gt;custom&lt;/span&gt;\n'
}

fn test_json_extension_config_file() {
	mut manager := initialize(
		template_dir:          dtm2_test_root()
		extension_config_file: os.join_path(dtm2_test_root(), 'extensions.json')
	)
	placeholders := {
		'title': '<Config>'
	}
	html := manager.expand('custom.view', placeholders: &placeholders)
	text := manager.expand('custom.mail', placeholders: &placeholders)
	assert html == '<section>&lt;Config&gt;</section>'
	assert text == 'Subject: &lt;Config&gt;\n'
}

fn test_default_json_extension_config_file_is_loaded_from_template_dir() {
	mut manager := initialize(
		template_dir:  dtm2_test_root()
		compress_html: false
	)
	placeholders := {
		'title': '<Auto>'
	}
	html := manager.expand('auto.fragment', placeholders: &placeholders)
	text := manager.expand('auto.message', placeholders: &placeholders)
	assert html == '<aside>&lt;Auto&gt;</aside>\n'
	assert text == 'Auto: &lt;Auto&gt;\n'
}

fn test_invalid_json_extension_entries_are_ignored() {
	mut manager := initialize(
		template_dir:          dtm2_test_root()
		extension_config_file: os.join_path(dtm2_test_root(), 'invalid_extensions.json')
		compress_html:         false
	)
	placeholders := {
		'title': '<Safe>'
	}
	ignored := manager.expand('bad.path', placeholders: &placeholders)
	loaded := manager.expand('auto.message', placeholders: &placeholders)
	assert ignored == internal_server_error
	assert loaded == 'Auto: &lt;Safe&gt;\n'
}

fn test_missing_template_returns_internal_server_error() {
	mut manager := new_test_manager(false)
	placeholders := map[string]string{}
	rendered := manager.expand('missing.html', placeholders: &placeholders)
	assert rendered == internal_server_error
	assert manager.compiled_template_count() == 0
}

fn test_invalid_template_extension_returns_internal_server_error() {
	mut manager := new_test_manager(false)
	placeholders := map[string]string{}
	rendered := manager.expand('invalid.css', placeholders: &placeholders)
	assert rendered == internal_server_error
	assert manager.compiled_template_count() == 0
}
