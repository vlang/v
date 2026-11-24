@[has_globals]
module main

#include "@DIR/sdk.h"

fn C.sdk_macro_calls() i32
fn C.sdk_renamed(value f64) f64
fn C.sdk_consume_exports() i32

__global macro_init_count i32

@[export: 'init']
fn init() { macro_init_count++ }

@[export: 'cleanup']
fn cleanup() {
	assert C.sdk_macro_calls() == 3
}

@[export: 'v_macro_init_count']
fn initializations() i32 { return macro_init_count }

@[export: 'sdk_natural']
fn sdk_natural(value f64) f64 {
	return value + 10
}

@[export: 'sdk_renamed']
fn renamed_implementation(value f64) f64 {
	return value + 20
}

const natural_callback = sdk_natural

struct Multiplier {
	factor f64
}

@[export: 'Multiplier__apply']
fn (m &Multiplier) apply(value f64) f64 { return m.factor * value }

@[export: 'v_macro_probe']
fn probe() i32 {
	if macro_init_count != 1 { return -6 }
	if sdk_natural(1) != 11 || renamed_implementation(2) != 22 { return -1 }
	callback := sdk_natural
	if callback(3) != 13 || natural_callback(4) != 14 { return -2 }
	m := Multiplier{ factor: 4 }
	if m.apply(2) != 8 { return -7 }
	// V calls use V's function, while the C client retains its SDK macro.
	if C.sdk_macro_calls() != 0 { return -3 }
	if C.sdk_consume_exports() != 2 { return -4 }
	if C.sdk_renamed(6) != 26 { return -5 }
	return C.sdk_macro_calls()
}

$if !export_macro_object ? {
	fn main() {
		assert probe() == 3
		assert initializations() == 1
		println('macro-safe C exports passed')
	}
}
