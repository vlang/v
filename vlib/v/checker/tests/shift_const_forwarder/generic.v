module shift_const_forwarder

import v.checker.tests.shift_const_values

pub fn first() u8 {
	return shift_const_values.forwarded[u8]()
}
