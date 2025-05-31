module veb

import os

fn test_load_files_translations() {
	os.chdir(os.dir(@FILE))!

	translations := load_tr_map()

	assert 'pt-br' in translations
	assert 'en' in translations

	assert 'msg_hello' in translations['pt-br']
	assert 'msg_hello' in translations['en']

	assert translations['pt-br']['msg_hello'] == 'Olá'
	assert translations['en']['msg_hello'] == 'Hello'
}
