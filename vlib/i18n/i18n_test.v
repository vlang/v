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
