module mongo

[inline]
pub fn parse_json(json_doc string) &C.bson_t//TODO return optional?
{
	/*bson := C.bson_new_from_json(json_doc.str, json_doc.len, C.NULL)
	if isnil(bson) {
		error('Can\'t parse Json')
		return C.bson_new()//TODO no return new bson
	}*/
	return C.bson_new_from_json(json_doc.str, json_doc.len, C.NULL)
}

[inline]
pub fn (bson &C.bson_t) append_doc(key string, doc &C.bson_t)
{
	C.bson_append_document(bson, key.str, key.len, doc)
}

[inline]
pub fn (bson &C.bson_t) append_str(key string, value string)
{
	C.bson_append_utf8(bson, key.str, key.len, value.str, value.len)
}

[inline]
pub fn (bson &C.bson_t) append_int(key string, value int)
{
	C.bson_append_int32(bson, key.str, key.len, value)
}

[inline]
pub fn (bson &C.bson_t) append_oid(key string, oid Oid)
{
	C.bson_append_oid(bson, key.str, key.len, oid.oid)
}

[inline]
pub fn (bson &C.bson_t) append_null(key string)
{
	C.bson_append_null(bson, key.str, key.len)
}