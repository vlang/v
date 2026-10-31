module scanner

// A byte lookup avoids repeating the ASCII identifier ranges for every tail byte.
const identifier_tail_bytes = new_identifier_tail_bytes()

fn new_identifier_tail_bytes() [256]bool {
	mut bytes := [256]bool{}
	for i in 0 .. 128 {
		c := u8(i)
		bytes[i] = (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
			|| (c >= `0` && c <= `9`) || c == `_`
	}
	return bytes
}
