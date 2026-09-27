module i18n

import os

fn test_load_tr_map_from_dir() {
	translations := load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))

	assert 'en' in translations
	assert 'pt-br' in translations
	assert translations['en']['msg_hello'] == 'Hello'
	assert translations['pt-br']['msg_hello'] == 'Ola'
}

fn test_tr_from_map_returns_key_for_missing_translation() {
	translations := load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))

	assert tr_from_map(translations, 'en', 'missing_key') == 'missing_key'
}

fn test_tr_plural_from_map() {
	translations := {
		'ru': {
			'goods': 'товар|а|ов'
		}
	}

	assert tr_plural_from_map(translations, 'ru', 'goods', 1) == 'товар'
	assert tr_plural_from_map(translations, 'ru', 'goods', 2) == 'товара'
	assert tr_plural_from_map(translations, 'ru', 'goods', 5) == 'товаров'
}

fn test_load_tr_map_from_dir_reads_json_files() {
	translations := load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))

	// keys from a `<lang>.json` file next to the `.tr` files
	assert translations['en']['msg_bye'] == 'Bye'
	// nested objects are flattened with `.`
	assert translations['en']['menu.file'] == 'File'
	assert translations['en']['menu.edit.undo'] == 'Undo'
	// non string scalars are usable as well
	assert translations['en']['answer'] == '42'
	assert translations['en']['missing'] == 'null'
}

fn test_tr_files_win_over_json_for_the_same_key() {
	translations := load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))

	// en.json defines msg_hello as 'Hello from JSON', en.tr as 'Hello'
	assert translations['en']['msg_hello'] == 'Hello'
}

fn test_json_in_a_language_subdirectory_is_namespaced_by_file_name() {
	translations := load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))

	assert 'zh' in translations
	assert translations['zh']['dashboard.title'] == '仪表板'
	assert translations['zh']['dashboard.widgets.clock'] == '时钟'
}

fn test_load_tr_map_from_files_reads_texts_like_a_directory() {
	translations := load_tr_map_from_files({
		'en.tr':             'msg_hello\nHello\n-----\ngoods\ngoods|item|items\n'
		'en.json':           '{"msg_hello": "Hello from JSON", "menu": {"file": "File"}}'
		'zh/dashboard.json': '{"title": "仪表板"}'
		'notes.txt':         'not a translation'
	})

	assert translations['en']['msg_hello'] == 'Hello'
	assert translations['en']['goods'] == 'goods|item|items'
	assert translations['en']['menu.file'] == 'File'
	assert translations['zh']['dashboard.title'] == '仪表板'
	assert translations.len == 2
}

fn test_load_tr_map_from_embedded_matches_the_directory() {
	embedded := load_tr_map_from_embedded('testdata/translations', [
		$embed_file('testdata/translations/en.tr'),
		$embed_file('testdata/translations/en.json'),
		$embed_file('testdata/translations/pt-br.tr'),
		$embed_file('./testdata/translations/zh/dashboard.json'),
	])

	assert embedded == load_tr_map_from_dir(os.join_path(os.dir(@FILE), 'testdata', 'translations'))
	assert tr_from_map(embedded, 'pt-br', 'msg_hello') == 'Ola'
}

fn test_embedded_relative_path_is_taken_inside_the_translations_directory() {
	assert embedded_relative_path('translations', 'translations/en.tr') == 'en.tr'
	assert embedded_relative_path('../../translations', '../../translations/zh/dashboard.json') == 'zh/dashboard.json'
	assert embedded_relative_path('./translations/', 'translations/zh/dashboard.json') == 'zh/dashboard.json'
	assert embedded_relative_path('', 'en.tr') == 'en.tr'
	// a file outside the directory is read as if it were directly in it
	assert embedded_relative_path('translations', 'other/en.json') == 'en.json'
}

fn test_load_tr_map_from_files_accepts_either_path_separator() {
	translations := load_tr_map_from_files({
		'zh\\dashboard.json':                 '{"title": "仪表板"}'
		os.join_path('ja', 'dashboard.json'): '{"title": "ダッシュボード"}'
		'legacy\\ru.tr':                      'msg_hello\nПривет\n'
		'mixed/pt\\pt-br.tr':                 'msg_hello\nOla\n'
	})

	assert translations['zh']['dashboard.title'] == '仪表板'
	assert translations['ja']['dashboard.title'] == 'ダッシュボード'
	assert translations['ru']['msg_hello'] == 'Привет'
	assert translations['pt-br']['msg_hello'] == 'Ola'
	assert translations.len == 4
}

fn test_load_tr_map_from_files_resolves_dot_segments() {
	translations := load_tr_map_from_files({
		os.join_path('.', 'en.json'):     '{"title": "Dashboard"}'
		'locales/../ru.json':             '{"title": "Панель"}'
		'zh/./widgets/../dashboard.json': '{"title": "仪表板"}'
		'..\\ja.tr':                      'msg_hello\nこんにちは\n'
	})

	// each resolves to where the directory loader would find it
	assert translations['en']['title'] == 'Dashboard'
	assert translations['ru']['title'] == 'Панель'
	assert translations['zh']['dashboard.title'] == '仪表板'
	assert translations['ja']['msg_hello'] == 'こんにちは'
	assert translations.len == 4
}

fn test_clean_slash_path_folds_dot_segments_on_every_platform() {
	assert clean_slash_path('./zh\\dashboard.json') == 'zh/dashboard.json'
	assert clean_slash_path('locales/../en.json') == 'en.json'
	assert clean_slash_path('../../translations/./en.tr') == '../../translations/en.tr'
	assert clean_slash_path('a/b/../../../en.tr') == '../en.tr'
	assert embedded_relative_path('./translations', 'translations/./zh/../en.tr') == 'en.tr'
}
