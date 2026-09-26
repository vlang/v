module diagserver

const quick_digest_prefix = 'quick:'

// quick_sum_digest writes a token.quick_sum as a digest of the inputs a child
// keeps (see keep_inputs), where it stands for a SHA-256.
pub fn quick_sum_digest(sum u64) string {
	return '${quick_digest_prefix}${sum:016x}'
}
