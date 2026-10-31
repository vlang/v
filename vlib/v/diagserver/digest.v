module diagserver

const quick_digest_prefix = 'quick:'

// quick_sum_digest writes a token.quick_sum of a source of `size` bytes as a
// digest of the inputs a child keeps (see keep_inputs), where it stands for a
// SHA-256.
pub fn quick_sum_digest(sum u64, size int) string {
	return '${quick_digest_prefix}${sum:016x}:${size}'
}

// quick_sum_of returns the sum and the size a quick_sum_digest holds.
fn quick_sum_of(digest string) ?(u64, int) {
	if !digest.starts_with(quick_digest_prefix) {
		return none
	}
	fields := digest[quick_digest_prefix.len..].split(':')
	if fields.len != 2 || fields[0].len != 16 || !fields[1].is_int() {
		return none
	}
	return fields[0].parse_uint(16, 64) or { return none }, fields[1].int()
}
