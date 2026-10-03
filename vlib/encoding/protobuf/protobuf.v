// Package protobuf implements the Protocol Buffers binary wire format in pure
// V, with no C dependency and no `protoc` step at build time.
//
// It is two things. The runtime is a Packer you append fields to and an Unpacker
// you pull them off, plus the varint and fixed-width primitives they are built
// from. The schema vocabulary, `ProtoScalar` and `scalar_by_name`, is what lets
// one description of a message say which of the several protobuf types a V `i32`
// should be treated as.
//
// The usual way in is `v pbgen`, which writes the encode and decode functions for
// a `.proto` file. Hand-writing them is the fallback. See README.md for the full
// documentation and the generated shape.
module protobuf
