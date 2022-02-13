// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module urllib

fn test_add_key_val() {
	mut values := Values{}
	values.add('key', 'value')
	val := values.get('key')
	assert val == 'value'
}

fn test_get_all_with_key() {
	mut values := Values{}
	values.add('key', 'value1')
	values.add('key', 'value2')
	key_values := values.get_all('key')
	assert key_values == ['value1', 'value2']
}

fn test_set_with_key() {
	mut values := Values{}
	values.add('key', 'value1')
	values.add('key', 'value2')
	values.add('key1', 'value2')
	values.set('key1', 'vlang')
	key_values := values.get_all('key')
	key2_values := values.get_all('key1')
	assert key_values == ['value1', 'value2']
	assert key2_values == ['vlang']
}

fn test_set_missing_key() {
	mut values := Values{}
	values.set('err', 'err1')
	vals := values.get_all('err')
	assert vals == ['err1']
}
