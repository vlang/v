// vtest retry: 3
module dtm

import os
import time

const behavior_temp_dtm_dir = 'dynamic_template_manager_behavior_test'
const behavior_temp_cache_dir = 'vcache_dtm'
const behavior_temp_templates_dir = 'templates'
const behavior_vtmp_dir = os.vtmp_dir()

fn behavior_test_root_dir() string {
	return os.join_path(behavior_vtmp_dir, '${behavior_temp_dtm_dir}_${os.getpid()}')
}

fn testsuite_begin() {
	temp_folder := behavior_test_root_dir()
	os.rmdir_all(temp_folder) or {}
	os.mkdir_all(os.join_path(temp_folder, behavior_temp_cache_dir))!
	os.mkdir_all(os.join_path(temp_folder, behavior_temp_templates_dir))!

	templates_path := os.join_path(temp_folder, behavior_temp_templates_dir)
	os.write_file(os.join_path(templates_path, 'page.html'), [
		'<!doctype html>',
		'<title>@title</title>',
		'<p>@body</p>',
		'<p>@count</p>',
	].join('\n'))!
	os.write_file(os.join_path(templates_path, 'page.txt'), [
		'Title: @title',
		'Body: @body',
	].join('\n'))!
	os.write_file(os.join_path(templates_path, 'layout.html'), [
		'<header>Start</header>',
		"@include 'partial'",
		'<footer>End</footer>',
	].join('\n'))!
	os.write_file(os.join_path(templates_path, 'partial.html'), '<section>@title</section>')!
	os.write_file(os.join_path(templates_path, 'cache.html'), '<main>@title</main>')!
	os.write_file(os.join_path(templates_path, 'cache_file_update.html'), '<main>@title</main>')!
	os.write_file(os.join_path(templates_path, 'no_store.html'), '<main>@title</main>')!
	os.write_file(os.join_path(templates_path, 'forever.html'), '<main>@title</main>')!
	os.write_file(os.join_path(templates_path, 'disk_cache.html'), '<main>@title</main>')!
	os.write_file(os.join_path(templates_path, 'include_html.html'), '<article>@content</article>')!
	os.write_file(os.join_path(templates_path, 'include_text.txt'), 'Text: @content')!
	os.write_file(os.join_path(templates_path, 'invalid.css'), 'body { color: red; }')!
}

fn testsuite_end() {
	os.rmdir_all(behavior_test_root_dir()) or {}
}

fn new_behavior_dtm(active_cache_server bool, compress_html bool, max_size_data_in_mem int) &DynamicTemplateManager {
	temp_folder := behavior_test_root_dir()
	return initialize(
		active_cache_server:  active_cache_server
		compress_html:        compress_html
		max_size_data_in_mem: max_size_data_in_mem
		test_cache_dir:       os.join_path(temp_folder, behavior_temp_cache_dir)
		test_template_dir:    os.join_path(temp_folder, behavior_temp_templates_dir)
	)
}

fn behavior_template_path(file_name string) string {
	return os.join_path(behavior_test_root_dir(), behavior_temp_templates_dir, file_name)
}

fn rendered_cache_count(dtmi &DynamicTemplateManager) int {
	rlock dtmi.template_caches {
		return dtmi.template_caches.len
	}
}

fn test_expand_escapes_html_placeholders_by_default() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('Hello <V>')
		'body':  DtmMultiTypeMap('<script>alert(1)</script>')
		'count': DtmMultiTypeMap(7)
	}

	rendered := dtmi.expand(behavior_template_path('page.html'), placeholders: &placeholders)

	assert rendered.contains('<title>Hello &lt;V&gt;</title>')
	assert rendered.contains('<p>&lt;script&gt;alert(1)&lt;/script&gt;</p>')
	assert rendered.contains('<p>7</p>')
}

fn test_expand_does_not_double_escape_placeholder_values_containing_dollar() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('Cost $5 <V>')
		'body':  DtmMultiTypeMap('Body')
		'count': DtmMultiTypeMap(1)
	}

	rendered := dtmi.expand(behavior_template_path('page.html'), placeholders: &placeholders)

	assert rendered.contains('<title>Cost $5 &lt;V&gt;</title>')
}

fn test_expand_escapes_html_entities_and_quotes() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('A & B "quoted" \'single\'')
		'body':  DtmMultiTypeMap('Body')
		'count': DtmMultiTypeMap(1)
	}

	rendered := dtmi.expand(behavior_template_path('page.html'), placeholders: &placeholders)

	assert rendered.contains('<title>A &amp; B &#34;quoted&#34; &#39;single&#39;</title>')
}

fn test_expand_escapes_text_templates() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('Plain <Title>')
		'body':  DtmMultiTypeMap('<strong>raw text</strong>')
	}

	rendered := dtmi.expand(behavior_template_path('page.txt'), placeholders: &placeholders)

	assert rendered.contains('Title: Plain &lt;Title&gt;')
	assert rendered.contains('Body: &lt;strong&gt;raw text&lt;/strong&gt;')
}

fn test_expand_compresses_html_without_regex_failure() {
	mut dtmi := new_behavior_dtm(false, true, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('Compressed')
		'body':  DtmMultiTypeMap('Body')
		'count': DtmMultiTypeMap(1)
	}

	rendered := dtmi.expand(behavior_template_path('page.html'), placeholders: &placeholders)

	assert rendered != internat_server_error
	assert !rendered.contains('\n')
	assert rendered.contains('<!doctype html><title>Compressed</title><p>Body</p><p>1</p>')
}

fn test_expand_with_empty_placeholders_keeps_template_renderable() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := map[string]DtmMultiTypeMap{}

	rendered := dtmi.expand(behavior_template_path('page.html'), placeholders: &placeholders)

	assert rendered.contains('<title>$title</title>')
	assert rendered.contains('<p>$body</p>')
	assert rendered.contains('<p>$count</p>')
}

fn test_expand_resolves_relative_includes() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'title': DtmMultiTypeMap('Included <Title>')
	}

	rendered := dtmi.expand(behavior_template_path('layout.html'), placeholders: &placeholders)

	assert rendered.contains('<header>Start</header>')
	assert rendered.contains('<section>Included &lt;Title&gt;</section>')
	assert rendered.contains('<footer>End</footer>')
}

fn test_includehtml_preserves_allowed_tags_and_escapes_unlisted_tags() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'content_#includehtml': DtmMultiTypeMap('<span>allowed</span><script>blocked()</script>')
	}

	rendered :=
		dtmi.expand(behavior_template_path('include_html.html'), placeholders: &placeholders)

	assert rendered.contains('<article><span>allowed</span>&lt;script&gt;blocked()&lt;/script&gt;</article>')
}

fn test_includehtml_is_still_escaped_in_text_templates() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := {
		'content_#includehtml': DtmMultiTypeMap('<span>text</span>')
	}

	rendered := dtmi.expand(behavior_template_path('include_text.txt'), placeholders: &placeholders)

	assert rendered.contains('Text: &lt;span&gt;text&lt;/span&gt;')
}

fn test_expand_returns_internal_error_for_missing_template() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := map[string]DtmMultiTypeMap{}

	rendered := dtmi.expand(behavior_template_path('missing.html'), placeholders: &placeholders)

	assert rendered == internat_server_error
}

fn test_expand_returns_internal_error_for_invalid_template_extension() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	placeholders := map[string]DtmMultiTypeMap{}

	rendered := dtmi.expand(behavior_template_path('invalid.css'), placeholders: &placeholders)

	assert rendered == internat_server_error
}

fn test_expand_without_cache_renders_fresh_placeholder_content() {
	mut dtmi := new_behavior_dtm(false, false, max_size_data_in_memory)
	first_placeholders := {
		'title': DtmMultiTypeMap('first')
	}
	second_placeholders := {
		'title': DtmMultiTypeMap('second')
	}

	first := dtmi.expand(behavior_template_path('cache.html'), placeholders: &first_placeholders)
	second := dtmi.expand(behavior_template_path('cache.html'), placeholders: &second_placeholders)

	assert first.contains('<main>first</main>')
	assert second.contains('<main>second</main>')
}

fn test_expand_with_cache_updates_when_placeholder_content_changes() {
	mut dtmi := new_behavior_dtm(true, false, max_size_data_in_memory)
	defer {
		dtmi.disable_cache()
	}
	first_placeholders := {
		'title': DtmMultiTypeMap('cached first')
	}
	second_placeholders := {
		'title': DtmMultiTypeMap('cached second')
	}

	first := dtmi.expand(behavior_template_path('cache.html'), placeholders: &first_placeholders)
	second := dtmi.expand(behavior_template_path('cache.html'), placeholders: &first_placeholders)
	third := dtmi.expand(behavior_template_path('cache.html'), placeholders: &second_placeholders)

	assert first.contains('<main>cached first</main>')
	assert second == first
	assert third.contains('<main>cached second</main>')
}

fn test_expand_with_legacy_cache_enabled_uses_new_parsed_template_engine() {
	mut dtmi := new_behavior_dtm(true, false, max_size_data_in_memory)
	defer {
		dtmi.disable_cache()
	}
	placeholders := {
		'title': DtmMultiTypeMap('stable cached value')
	}

	first := dtmi.expand(behavior_template_path('cache.html'), placeholders: &placeholders)
	second := dtmi.expand(behavior_template_path('cache.html'), placeholders: &placeholders)

	assert first == second
	assert rendered_cache_count(dtmi) == 0
	assert dtmi.render_engine.compiled_template_count() == 1
}

fn test_expand_with_cache_updates_when_template_file_changes() {
	mut dtmi := new_behavior_dtm(true, false, max_size_data_in_memory)
	defer {
		dtmi.disable_cache()
	}
	template_path := behavior_template_path('cache_file_update.html')
	placeholders := {
		'title': DtmMultiTypeMap('file driven')
	}

	first := dtmi.expand(template_path, placeholders: &placeholders)
	time.sleep(1100 * time.millisecond)
	os.write_file(template_path, '<section>@title</section>')!
	second := dtmi.expand(template_path, placeholders: &placeholders)

	assert first.contains('<main>file driven</main>')
	assert second.contains('<section>file driven</section>')
}

fn test_cache_delay_minus_one_skips_rendered_cache_storage() {
	mut dtmi := new_behavior_dtm(true, false, max_size_data_in_memory)
	defer {
		dtmi.disable_cache()
	}
	placeholders := {
		'title': DtmMultiTypeMap('no store')
	}

	rendered := dtmi.expand(behavior_template_path('no_store.html'),
		placeholders:           &placeholders
		cache_delay_expiration: -1
	)

	assert rendered.contains('<main>no store</main>')
	assert rendered_cache_count(dtmi) == 0
}

fn test_cache_delay_zero_is_accepted_without_rendered_cache_storage() {
	mut dtmi := new_behavior_dtm(true, false, max_size_data_in_memory)
	defer {
		dtmi.disable_cache()
	}
	placeholders := {
		'title': DtmMultiTypeMap('forever')
	}

	rendered := dtmi.expand(behavior_template_path('forever.html'),
		placeholders:           &placeholders
		cache_delay_expiration: 0
	)

	assert rendered.contains('<main>forever</main>')
	assert rendered_cache_count(dtmi) == 0
}

fn test_memory_limit_zero_does_not_create_rendered_disk_cache() {
	mut dtmi := new_behavior_dtm(true, false, 0)
	defer {
		dtmi.disable_cache()
	}
	placeholders := {
		'title': DtmMultiTypeMap('disk mode')
	}

	rendered := dtmi.expand(behavior_template_path('disk_cache.html'), placeholders: &placeholders)

	assert rendered.contains('<main>disk mode</main>')
	assert rendered_cache_count(dtmi) == 0
	for file_name in os.ls(dtmi.template_cache_folder)! {
		assert !file_name.ends_with('.cache')
	}
}
