module main

import os

#flag @DIR/hooks.c
#include "@DIR/hooks.h"

fn C.environment_tracking_begin()
fn C.environment_tracking_end()
fn C.environment_tracking_allocations() i32
fn C.environment_tracking_frees() i32

fn require(ok bool, message string) {
	if !ok { panic(message) }
}

fn main() {
	require(os.setenv('FENV_TRACK', 'before', true) == 0, 'tracking setup failed')
	C.environment_tracking_begin()
	ignored := os.setenv('FENV_TRACK', 'ignored', false)
	unchanged := os.setenv('FENV_TRACK', 'before', true)
	missing := os.unsetenv('FENV_TRACK_MISSING')
	startup := os.setenv('FENV_EMPTY', 'ignored', false)
	unchanged_startup := os.setenv('FENV_EQUALS', 'left=right', true)
	C.environment_tracking_end()
	require(ignored == 0 && unchanged == 0 && missing == 0 && startup == 0 && unchanged_startup == 0, 'nonmutating environment operations failed')
	require(C.environment_tracking_allocations() == 0 && C.environment_tracking_frees() == 0,
		'nonmutating environment operations allocated')
	C.environment_tracking_begin()
	changed := os.setenv('FENV_TRACK', 'a longer replacement value', true)
	C.environment_tracking_end()
	require(changed == 0, 'replacement failed')
	require(C.environment_tracking_allocations() == 1 && C.environment_tracking_frees() == 1,
		'replacement must use one owned block and retire one block')
	require(os.getenv('FENV_TRACK') == 'a longer replacement value', 'replacement value lost')
	C.environment_tracking_begin()
	removed := os.unsetenv('FENV_TRACK')
	C.environment_tracking_end()
	require(removed == 0, 'remove owned entry failed')
	require(C.environment_tracking_allocations() == 0 && C.environment_tracking_frees() == 1,
		'removing an entry absent from startup must not allocate a tombstone')
	C.environment_tracking_begin()
	os.clearenv()
	os.clearenv()
	C.environment_tracking_end()
	require(C.environment_tracking_allocations() == 0, 'clearing the environment allocated')
	C.environment_tracking_begin()
	reference := map[string]string{}
	C.environment_tracking_end()
	reference_allocations := C.environment_tracking_allocations()
	require(reference.len == 0, 'reference map must be empty')
	C.environment_tracking_begin()
	snapshot := os.environ()
	C.environment_tracking_end()
	snapshot_allocations := C.environment_tracking_allocations()
	require(snapshot.len == 0, 'snapshot must be empty after clearenv')
	require(snapshot_allocations <= reference_allocations,
		'empty snapshot allocates ${snapshot_allocations} blocks; one map uses ${reference_allocations}')
	println('environment allocation checks passed')
}
