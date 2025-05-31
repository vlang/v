module main

import flag
import os

pub enum Number as u8 {
	zero = 0
	one  = 1
	six  = 6
}

fn test_main() {
	mut fp := flag.new_flag_parser(os.args)
	_ := fp.finalize()!
	mut numbers := map[Number]string{}
	numbers[.zero] = '0'
	numbers[.one] = '1'
	assert numbers.str() == "{zero: '0', one: '1'}"
}
