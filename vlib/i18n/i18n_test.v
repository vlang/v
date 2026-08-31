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
