module fixed_array_default_mod

pub struct Response {
pub mut:
	revision u64
}

// The declared default matters only for a struct initialized from *another*
// module: within one module the field resolves through a path that already
// copies it after the literal.
pub struct Request {
pub mut:
	id       [4]u64 = [
		u64(0xc7b1dd30df4c8b88),
		0x0a82e883a194f07b,
		0x48dcf1cb8ad2b852,
		0x63984e959a98244b,
	]!
	tags     [3]int = [7, 8, 9]!
	revision u64
	response &Response
}
