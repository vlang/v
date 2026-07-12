module kdl

import os
import x.kdl.document
import x.kdl.tokenizer
import x.kdl.parser
import x.kdl.generator
import x.kdl.marshaler
import x.kdl.coerce
import x.kdl.relaxed

pub type Document = document.Document
pub type Node = document.Node
pub type Entry = document.Entry
pub type Argument = document.Argument
pub type Property = document.Property
pub type Value = document.Value
pub type StringVal = document.StringVal
pub type IntVal = document.IntVal
pub type FloatVal = document.FloatVal
pub type BoolVal = document.BoolVal
pub type NullVal = document.NullVal
pub type ValueFlag = document.ValueFlag
pub type Comment = document.Comment
pub type KdlParseError = document.KdlParseError
pub type RelaxedNonCompliant = relaxed.RelaxedNonCompliant

pub const nginx_syntax = relaxed.nginx_syntax
pub const yaml_toml_assignments = relaxed.yaml_toml_assignments
pub const multiplier_suffixes = relaxed.multiplier_suffixes

pub struct ParseOpts {
pub mut:
	relaxed        relaxed.RelaxedNonCompliant
	parse_comments bool
}

pub type TokenKind = tokenizer.TokenKind
pub type Token = tokenizer.Token
pub type Scanner = tokenizer.Scanner

pub type Marshaler = marshaler.Marshaler
pub type Unmarshaler = marshaler.Unmarshaler
pub type EncodeOpts = marshaler.EncodeOpts
pub type DecodeOpts = marshaler.DecodeOpts
pub type Encoder = marshaler.Encoder

pub fn parse(text string) !Document {
	return parser.parse(text)
}

pub fn parse_opts(text string, opts ParseOpts) !Document {
	return parser.parse_opts(text, opts.relaxed, opts.parse_comments)
}

pub fn parse_file(path string) !Document {
	content := os.read_file(path)!
	return parser.parse(content)
}

pub fn parse_file_opts(path string, opts ParseOpts) !Document {
	content := os.read_file(path)!
	return parser.parse_opts(content, opts.relaxed, opts.parse_comments)
}

pub fn format(doc Document) !string {
	return generator.format(document.Document(doc))
}

pub fn encode[T](val T) string {
	return marshaler.encode(val)
}

pub fn encode_opts[T](val T, opts EncodeOpts) string {
	return marshaler.encode_opts(val, opts)
}

pub fn decode[T](data string) !T {
	return marshaler.decode[T](data)
}

pub fn decode_opts[T](data string, opts DecodeOpts) !T {
	return marshaler.decode_opts[T](data, opts)
}

pub fn new_encoder() Encoder {
	return marshaler.new_encoder()
}

pub fn as_string(v Value) string {
	return coerce.as_string(v)
}

pub fn as_int(v Value) int {
	return coerce.as_int(v)
}

pub fn as_i64(v Value) i64 {
	return coerce.as_i64(v)
}

pub fn as_f64(v Value) f64 {
	return coerce.as_f64(v)
}

pub fn as_bool(v Value) bool {
	return coerce.as_bool(v)
}

pub fn as_u64(v Value) u64 {
	return coerce.as_u64(v)
}

pub fn as_numeric(v Value) (i64, f64, bool) {
	return coerce.as_numeric(v)
}

pub fn is_null(v Value) bool {
	return coerce.is_null(v)
}

pub fn apply_rename(name string, strategy string) string {
	return marshaler.apply_rename(name, strategy)
}

pub fn can_be_bare_identifier(s string) bool {
	return document.can_be_bare_identifier(s)
}

pub fn quote_string(s string) string {
	return document.quote_string(s)
}

pub fn raw_string(s string) string {
	return document.raw_string(s)
}

pub fn unquote_string(s string) !string {
	return document.unquote_string(s)
}

pub fn property_exists(node &Node, key string) bool {
	return document.property_exists(&document.Node(node), key)
}

pub fn property_get(node &Node, key string) ?Value {
	return document.property_get(&document.Node(node), key)
}

pub fn property_has(node &Node) bool {
	return document.property_has(&document.Node(node))
}
