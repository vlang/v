module gg

import os
import time

struct SaveTestData {
mut:
	score int
	name  string
}

fn test_save_primitives() {
	mut s := new_save_store()
	s.set_string('name', 'hero')
	s.set_int('score', 42)
	s.set_float('ratio', 0.75)
	s.set_bool('hard', true)
	assert s.get_string('name') == 'hero'
	assert s.get_int('score', 0) == 42
	assert s.get_float('ratio', 0) == 0.75
	assert s.get_bool('hard', false)
	assert s.get_int('missing', 7) == 7
	assert s.has('score')
	s.remove('score')
	assert !s.has('score')
}

fn test_save_json() {
	mut s := new_save_store()
	s.set_json('data', SaveTestData{12, 'v'})
	data := s.get_json[SaveTestData]('data') or { panic('decode failed') }
	assert data.score == 12 && data.name == 'v'
}

fn test_save_roundtrip() {
	mut s := new_save_store()
	s.set_string('a', 'b')
	s.set_int('n', 9)
	restored := save_store_from_json(s.to_json()) or { panic('parse failed') }
	assert restored.get_string('a') == 'b'
	assert restored.get_int('n', 0) == 9
}

fn test_save_file() {
	path := os.join_path(os.temp_dir(), 'gg_save_test_${time.now().unix_nano()}.json')
	mut s := new_save_store()
	s.set_int('level', 3)
	s.save(path) or { panic('write failed') }
	loaded := load_save_store(path) or { panic('read failed') }
	assert loaded.get_int('level', 0) == 3
	os.rm(path) or {}
}
