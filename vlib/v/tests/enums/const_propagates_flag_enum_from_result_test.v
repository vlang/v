@[flag]
pub enum TransFlags {
	no_deps
	no_save
	recurse
}

const trans_flags_none = TransFlags.from(0)!

fn test_main() {
	_ := trans_flags_none
	assert true
}
