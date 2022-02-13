// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module urllib

fn test_add_key_val() {
	mut values := Values{}
	values.add('value', 'key')
	val := values.get('value')
	assert val == 'key'
}

fn test_get_all_with_key() {
	mut values := Values{}
	values.add('value', 'key1')
	values.add('value', 'key2')
	key_values := values.get_all('value')
	assert key_values == ['key1', 'key2']
}

fn test_set_with_key() {
	mut values := Values{}
	values.add('value', 'key1')
	values.add('value', 'key2')
	values.add('value1', 'key2')
	values.set('value1', 'vlang')
	key_values := values.get_all('value')
	key2_values := values.get_all('value1')
	assert key_values == ['key1', 'key2']
	assert key2_values == ['vlang']
}

fn test_set_missing_key() {
	mut values := Values{}
	values.set('err', 'err1')
	vals := values.get_all('err')
	assert vals == ['err1']
}
