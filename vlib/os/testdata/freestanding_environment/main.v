module main

import arena
import os

fn require(ok bool, message string) {
	if !ok {
		eprintln(message)
		exit(95)
	}
}

fn expect(name string, value string) {
	actual := os.getenv_opt(name) or { panic('missing test variable ${name}') }
	require(actual == value, 'wrong test environment value for ${name}')
}

fn check_startup() {
	expect('FENV_EMPTY', '')
	expect('FENV_UNICODE', 'λ🙂')
	expect('FENV_EQUALS', 'left=right')
	if _ := os.getenv_opt('FENV_MISSING') { panic('invented missing variable') }
	all := os.environ()
	require('FENV_EMPTY' in all && all['FENV_EMPTY'] == '', 'snapshot dropped empty value')
	require(all['FENV_UNICODE'] == 'λ🙂', 'snapshot lost Unicode')
	$if freestanding || syscalls {
		_, malformed := bf_getenv('FENV_PREFIX')
		require(!malformed, 'startup walker accepted an entry without equals')
		bare := bf_environ()
		require('FENV_EMPTY' in bare && bare['FENV_EMPTY'] == '', 'bare snapshot dropped empty value')
		require('FENV_PREFIX' !in bare, 'bare snapshot accepted malformed entry')
	}
}

fn check_mutation() {
	require(os.unsetenv('FENV_NEW') == 0, 'unset missing failed')
	require(os.setenv('FENV_NEW', 'before', false) == 0, 'initial set failed')
	retained := os.getenv('FENV_NEW')
	snapshot := os.environ()
	require(os.setenv('FENV_NEW', 'ignored', false) == 0, 'ignored set failed')
	expect('FENV_NEW', 'before')
	require(os.setenv('FENV_NEW', 'after', true) == 0, 'overwrite failed')
	expect('FENV_NEW', 'after')
	require(retained == 'before' && snapshot['FENV_NEW'] == 'before', 'environment snapshots alias mutable state')
	require(os.setenv('FENV_EMPTY', 'ignored', false) == 0, 'empty value counted as missing')
	expect('FENV_EMPTY', '')
	require(os.unsetenv('FENV_EMPTY') == 0, 'unset startup value failed')
	if _ := os.getenv_opt('FENV_EMPTY') { panic('startup value reappeared after unset') }
	require('FENV_EMPTY' !in os.environ(), 'unset value remained in snapshot')
	require(os.setenv('FENV_EMPTY', '', false) == 0, 'restore empty value failed')
	expect('FENV_EMPTY', '')
	require(os.setenv('Büro', '한국어', true) == 0, 'Unicode set failed')
	expect('Büro', '한국어')
	require(os.unsetenv('Büro') == 0, 'Unicode unset failed')
	for invalid in ['', 'FENV_BAD=NAME'] {
		require(os.setenv(invalid, 'x', true) == -1, 'accepted invalid set name')
		require(os.unsetenv(invalid) == -1, 'accepted invalid unset name')
	}
	require(os.setenv('FENV_NUL\x00suffix', 'prefix\x00suffix', true) == 0, 'C-string prefix set failed')
	expect('FENV_NUL', 'prefix')
	require(os.unsetenv('FENV_NUL\x00suffix') == 0, 'C-string prefix unset failed')
	if _ := os.getenv_opt('FENV_NUL') { panic('prefix unset missed variable') }
}

fn check_arena() {
	mut region := arena.new()
	for index in 0 .. 8 {
		region.push()
		value := 'temporary-${index}-λ🙂'
		require(os.setenv('FENV_ARENA', value, true) == 0, 'arena set failed')
		owned := os.getenv('FENV_ARENA')
		require(owned == value && region.owns(owned.str), 'getenv did not use the caller allocator')
		snapshot := os.environ()
		require(snapshot['FENV_ARENA'] == value && region.owns(snapshot['FENV_ARENA'].str), 'snapshot value escaped the caller allocator')
		region.pop()
		region.reset()
		expect('FENV_ARENA', 'temporary-${index}-λ🙂')
	}
	region.free()
	expect('FENV_ARENA', 'temporary-7-λ🙂')
	require(os.unsetenv('FENV_ARENA') == 0, 'arena-owned input left an invalid global entry')
}

fn environment_worker(index int) bool {
	name := 'FENV_THREAD_${index}'
	mut region := arena.new()
	for iteration in 0 .. 128 {
		region.push()
		value := 'value-${index}-${iteration}-λ'
		mut ok := os.setenv(name, value, true) == 0 && os.getenv(name) == value
		if ok && iteration % 16 == 0 {
			all := os.environ()
			ok = all[name] == value
		}
		region.pop()
		region.reset()
		if !ok {
			region.free()
			return false
		}
	}
	region.free()
	return os.getenv(name) == 'value-${index}-127-λ' && os.unsetenv(name) == 0
}

fn check_threads() {
	mut threads := []thread bool{cap: 4}
	for index in 0 .. 4 { threads << spawn environment_worker(index) }
	for worker in threads { require(worker.wait(), 'concurrent environment lost an update') }
}

fn check_clear() {
	require(os.setenv('FENV_CLEAR', 'temporary', true) == 0, 'clear setup failed')
	retained := os.environ()
	os.clearenv()
	require(os.environ().len == 0, 'clearenv retained entries')
	if _ := os.getenv_opt('FENV_EMPTY') { panic('clearenv retained startup value') }
	if _ := os.getenv_opt('FENV_CLEAR') { panic('clearenv retained changed value') }
	require(retained['FENV_CLEAR'] == 'temporary', 'clearenv invalidated retained snapshot')
	os.clearenv()
	require(os.setenv('FENV_REUSED', 'λ', false) == 0, 'set after clear failed')
	expect('FENV_REUSED', 'λ')
	require(os.environ().len == 1, 'set after clear revived startup entries')
	os.clearenv()
	require(os.environ().len == 0, 'repeated clearenv failed')
}

fn main() {
	require(os.args.len == 2, 'pass a validation mode')
	mode := os.args[1]
	match mode {
		'startup' { check_startup() }
		'mutation' { check_mutation() }
		'arena' { check_arena() }
		'threads' { check_threads() }
		'clear' { check_clear() }
		else { panic('unknown validation mode') }
	}
	println('environment ${mode} passed')
}
