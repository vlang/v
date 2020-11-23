module mongo

struct C.ssize_t

struct C.bson_t
struct C.bson_oid_t
struct C.bson_error_t

struct C.mongoc_client_t
struct C.mongoc_cursor_t
struct C.mongoc_collection_t
struct C.mongoc_database_t
struct C.mongoc_read_prefs_t

struct Database {
	db   &C.mongoc_database_t
mut:
	cols map[string]Collection
}

struct Connection {
	uri    string
	client &C.mongoc_client_t
mut:
	dbs    map[string]Database
}

struct Collection {
	name       string
mut:
	collection &C.mongoc_collection_t
}

struct Cursor {
	cursor &C.mongoc_cursor_t
mut:
	next   bool
}

struct Bson {
mut:
	bson &C.bson_t
}

struct Oid {
mut:
	oid &C.bson_oid_t
}