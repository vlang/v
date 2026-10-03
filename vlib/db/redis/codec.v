module redis

import json2

// set_json stores a value encoded as JSON. Read it back with get_json using the same type.
pub fn (mut db DB) set_json[T](key string, value T) !string {
	return db.execute_string(['SET', key, json2.encode(value)])
}

// get_json decodes a JSON value stored with set_json. Missing keys return NilError,
// and values that are not valid JSON for T return the json2 decoding error.
pub fn (mut db DB) get_json[T](key string) !T {
	resp := db.cmd('GET', key)!
	if db.pipeline_mode || db.transaction_mode {
		return T{}
	}
	if resp is RedisNull {
		return NilError{ message: '`get_json()`: key ${key} not found' }
	}
	return json2.decode[T](bulk_value[string](resp, 'get_json')!)
}
