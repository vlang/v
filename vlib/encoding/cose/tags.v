// CBOR tag numbers reserved for COSE message types by RFC 9052 §2.
module cose

// CBOR tag numbers identifying COSE message types. A COSE message MAY be
// emitted untagged (the recipient already knows the structure) or tagged
// with one of these values to make the message self-describing.
pub const tag_sign = u64(98) // COSE_Sign  — RFC 9052 §4
pub const tag_sign1 = u64(18) // COSE_Sign1 — RFC 9052 §4.2
pub const tag_mac = u64(97) // COSE_Mac   — RFC 9052 §6
pub const tag_mac0 = u64(17) // COSE_Mac0  — RFC 9052 §6.2
