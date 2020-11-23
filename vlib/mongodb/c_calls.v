module mongo

#flag linux -lpthread
#flag linux -lmongoc-1.0
#flag linux -lbson-1.0
#flag linux -I /usr/local/include/libmongoc-1.0
#flag linux -I /usr/local/include/libbson-1.0
#include <mongoc/mongoc.h>
#include <bson/bson.h>

/**		MONGOC		**/
fn C.mongoc_init() voidptr
fn C.mongoc_client_new() &C.mongoc_client_t
fn C.mongoc_client_set_appname() voidptr
fn C.mongoc_client_get_collection() &C.mongoc_collection_t
fn C.mongoc_client_get_database() &C.mongoc_database_t
fn C.mongoc_collection_find_with_opts() &C.mongoc_cursor_t
fn C.mongoc_collection_count_documents() i64
fn C.mongoc_collection_insert_one() bool
fn C.mongoc_cursor_next() bool
fn C.mongoc_collection_destroy() voidptr
fn C.mongoc_database_destroy() voidptr
fn C.mongoc_client_destroy() voidptr
fn C.mongoc_cleanup() voidptr

/**		BSON		**/
fn C.bson_new() &C.bson_t
fn C.bcon_new() &C.bson_t
fn C.bcon_utf8() int
fn C.bson_new_from_json() &C.bson_t
fn C.bson_append_document() bool
fn C.bson_append_utf8() bool
fn C.bson_append_int32() bool
fn C.bson_append_oid() bool
fn C.bson_append_null() bool
fn C.bson_append_document_begin() bool
fn C.bson_append_document_end() bool
fn C.bson_destroy() voidptr
