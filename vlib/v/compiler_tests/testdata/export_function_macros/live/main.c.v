module main

#include "@DIR/../sdk.h"

fn C.sdk_macro_calls() i32

@[live; export: 'sdk_natural']
fn sdk_natural(value f64) f64 { return value + 10 }

fn main() {
	assert sdk_natural(7) == 17
	assert C.sdk_macro_calls() == 0
	println('macro-safe live export passed')
}
