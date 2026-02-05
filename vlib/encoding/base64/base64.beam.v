// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend base64 module
// On BEAM, V base64 operations map to Erlang's base64 module
module base64

// encode encodes the `[]u8` value passed in `data` to base64.
// On BEAM: base64:encode(Binary)
// Codegen: base64:encode(list_to_binary(Data))
// Example: assert base64.encode('V in base 64'.bytes()) == 'ViBpbiBiYXNlIDY0'
pub fn encode(data []u8) string {
	// Compiler implementation - actual codegen generates Erlang code:
	//   base64:encode(list_to_binary(Data))
	// Returns the base64 encoded binary as a V string
	return ''
}

// encode_str is the string variant of encode
// On BEAM: base64:encode(Binary)
// Codegen: base64:encode(Data)
pub fn encode_str(data string) string {
	// Compiler implementation - actual codegen generates Erlang code:
	//   base64:encode(Data)
	// V strings are binaries on BEAM, so no conversion needed
	return ''
}

// decode decodes the base64 encoded `string` value passed in `data`.
// On BEAM: base64:decode(Binary)
// Codegen: binary_to_list(base64:decode(Data))
// Example: assert base64.decode('ViBpbiBiYXNlIDY0') == 'V in base 64'.bytes()
pub fn decode(data string) []u8 {
	// Compiler implementation - actual codegen generates Erlang code:
	//   binary_to_list(base64:decode(Data))
	// Returns decoded bytes as a V []u8 (Erlang list of integers)
	return []
}

// decode_str is the string variant of decode
// On BEAM: base64:decode(Binary)
// Codegen: base64:decode(Data)
pub fn decode_str(data string) string {
	// Compiler implementation - actual codegen generates Erlang code:
	//   base64:decode(Data)
	// Returns decoded binary as a V string
	return ''
}

// encode_in_buffer base64 encodes the `[]u8` passed in `data` into `buffer`.
// On BEAM: This function is less relevant since BEAM handles memory automatically,
// but we provide it for API compatibility.
// Codegen: vbeam_base64:encode_in_buffer(Data, Buffer)
// Returns the size of the encoded data in the buffer.
pub fn encode_in_buffer(data []u8, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	// Actual codegen would copy base64:encode result into the buffer
	return 0
}

// decode_in_buffer decodes the base64 encoded `string` reference passed in `data` into `buffer`.
// On BEAM: Provided for API compatibility.
// Codegen: vbeam_base64:decode_in_buffer(Data, Buffer)
// Returns the size of the decoded data in the buffer.
pub fn decode_in_buffer(data &string, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// decode_in_buffer_bytes decodes the base64 encoded ASCII bytes from `data` into `buffer`.
// On BEAM: Provided for API compatibility.
// Codegen: vbeam_base64:decode_in_buffer_bytes(Data, Buffer)
// Returns the size of the decoded data in the buffer.
pub fn decode_in_buffer_bytes(data []u8, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// encode_from_buffer will perform encoding from any type of src buffer
// and write the bytes into `dest`.
// On BEAM: Internal function, provided for API compatibility.
// Codegen: vbeam_base64:encode_from_buffer(Dest, Src, SrcLen)
fn encode_from_buffer(dest &u8, src &u8, src_len int) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `src` into `dest`.
// On BEAM: Internal function, provided for API compatibility.
// Codegen: vbeam_base64:decode_from_buffer(Dest, Src, SrcLen)
fn decode_from_buffer(dest &u8, src &u8, src_len int) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// alloc_and_encode is a private function that allocates and encodes data into a string
// On BEAM: Internal function used by encode and encode_str
// Codegen: base64:encode(...)
fn alloc_and_encode(src &u8, len int) string {
	// On BEAM, this is simplified since memory allocation is automatic
	// The actual encoding is done by Erlang's base64:encode
	return ''
}

// B64_64_datablock is a union for optimized decoding (C backend specific)
// On BEAM: Not used, but defined for type compatibility
union B64_64_datablock {
mut:
	data      u64
	data_byte [8]u8
}

// B64_32_datablock is a union for optimized decoding (C backend specific)
// On BEAM: Not used, but defined for type compatibility
union B64_32_datablock {
mut:
	data      u32
	data_byte [4]u8
}
