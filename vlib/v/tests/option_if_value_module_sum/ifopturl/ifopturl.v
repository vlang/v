module ifopturl

// Origin is a sum type whose short name matches the `ifoptdom.Origin` struct.
pub type Origin = OpaqueOrigin | TupleOrigin

pub struct TupleOrigin {
pub:
	host string
}

pub struct OpaqueOrigin {
pub:
	id int
}
