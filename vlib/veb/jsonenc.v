module veb

import json2 as json

// json_encode_into appends the JSON encoding of `val` to `buf`, growing it as
// needed. It is the buffer-reusing counterpart of `Context.json`: `json2.encode`
// builds a fresh `strings.Builder` (2048 bytes) on every call and copies it out
// with `Builder.str()`, so a hot response path pays a Builder buffer plus an
// intermediate string on top of the unavoidable copy into `res.body`. Encoding
// into a caller-owned buffer that is reused across requests removes both of
// those per-request allocations; only the single copy into `res.body` remains.
// The caller owns `buf`: clear it (or let `json_buf` do so) before reuse, and
// do not share it across threads while encoding.
//
// Output is byte-identical to `json2.encode` for the same value, including
// struct field order, `json:` names, `omitempty`, enum/string/float formatting
// (shortest round-trip via `strlong`), and the `{"error": msg}` envelope that
// `Context.error_with` sends; see `jsonenc_test.v`.
pub fn json_encode_into[T](val T, mut buf []u8) {
	json.encode_append[T](val, mut buf)
}

// json_buf responds exactly like `Context.json` — status 200 unless already
// set, `Content-Type: application/json` — but encodes into the caller-provided
// `buf` instead of allocating a fresh intermediate string per request. Keep
// `buf` on the app (or in a pool) and reuse it across requests; it is cleared
// on entry. The response body still owns its own copy, so `buf` may be reused
// immediately after this call returns.
pub fn (mut ctx Context) json_buf[T](val T, mut buf []u8) Result {
	buf.clear()
	json.encode_append[T](val, mut buf)
	// Alias, do not copy: `send_response_to_client` clones into `res.body`,
	// which is the single remaining copy on this path. `buf` is untouched
	// during the send, so the alias is safe here.
	s := unsafe { tos(buf.data, buf.len) }
	return ctx.send_response_to_client('application/json', s)
}
