module forwarding

import decay

// forward preserves retention when the translated callee is hidden by an import.
pub fn forward(values [3]int) &int {
	return decay.pick(values)
}

// forward_generic exercises the retention metadata of a specialized wrapper.
pub fn forward_generic[T](marker T, values [3]int) &int {
	_ = marker
	return forward(values)
}

pub struct Forwarder {}

// forward preserves retention through an ordinary receiver method.
pub fn (_ Forwarder) forward(values [3]int) &int {
	return forward(values)
}

pub interface ArrayPicker {
	pick(values [3]int) &int
}

// forward_interface preserves retention through an imported interface signature.
pub fn forward_interface(picker ArrayPicker, values [3]int) &int {
	return picker.pick(values)
}
