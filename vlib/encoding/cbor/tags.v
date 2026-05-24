module cbor

// Well-known CBOR tag numbers from the IANA registry. These are not the
// only valid tags; users may emit any u64 tag via `Packer.pack_tag`.

pub const tag_date_time = u64(0) // RFC 3339 date/time text string
pub const tag_epoch = u64(1) // POSIX epoch seconds (int or float)
pub const tag_unsigned_bignum = u64(2) // byte string, big-endian magnitude
pub const tag_negative_bignum = u64(3) // byte string, -(1 + n)
pub const tag_decimal_fraction = u64(4) // [exponent, mantissa] with base 10
pub const tag_bigfloat = u64(5) // [exponent, mantissa] with base 2
pub const tag_base64url_hint = u64(21)
pub const tag_base64_hint = u64(22)
pub const tag_base16_hint = u64(23)
pub const tag_embedded_cbor = u64(24) // byte string holding well-formed CBOR
pub const tag_uri = u64(32) // RFC 3986 URI as text string
pub const tag_base64url = u64(33)
pub const tag_base64 = u64(34)
pub const tag_self_describe = u64(55799) // CBOR magic prefix

// Magic prefix `d9d9f7` produced when wrapping any value in tag 55799.
pub const self_describe_prefix = [u8(0xd9), 0xd9, 0xf7]
