module mongo

pub fn mongo_db(uri string) Connection {
	C.mongoc_init()
	db := Connection{
		uri: uri
		client: C.mongoc_client_new(uri.str)
		dbs: map[string]Database{}
	}
	C.mongoc_client_set_appname(db.client, 'mongov')
	return db
}

pub fn (mut con Connection) get_collection(database string, collection string) Collection {
	mut db := con.get_db(database)
	if collection in db.cols {
		return db.cols[collection]
	}
	col := Collection{
		name: collection
		collection: C.mongoc_client_get_collection(con.client, database.str, collection.str)
	}
	db.cols[collection] = col
	return col
}

// TODO check if is not usefull store it and use directly get_collection
pub fn (mut con Connection) get_db(database string) Database {
	if database in con.dbs {
		return con.dbs[database]
	}
	db := Database{
		db: C.mongoc_client_get_database(con.client, database.str)
		cols: map[string]Collection{}
	}
	con.dbs[database] = db
	return db
}

pub fn (col Collection) find(query &C.bson_t) Cursor {
	cursor := Cursor{C.mongoc_collection_find_with_opts(col.collection, query, C.NULL,
		C.NULL), true}
	C.bson_destroy(query)
	return cursor
}

pub fn (mut cursor Cursor) next() &C.bson_t { // TODO refactor
	doc := C.bson_new()
	if !C.mongoc_cursor_next(cursor, &doc) {
		cursor.next = false
		error('No more documents')
	}
	return doc // TODO not return doc if no have next
}

pub fn (col Collection) count(query &C.bson_t) i64 {
	count := C.mongoc_collection_count_documents(col.collection, query, C.NULL, C.NULL,
		C.NULL, C.NULL)
	C.bson_destroy(query)
	return count
}

pub fn (col Collection) add_document() {
	json := '{"hello": "world"}'
	doc := C.bson_new_from_json(json.str, json.len, C.NULL)
	if !C.mongoc_collection_insert_one(col.collection, doc, C.NULL, C.NULL, C.NULL) {
		println('Error inserting document')
	}
	C.bson_destroy(doc)
}

pub fn (col Collection) insert_one(doc &C.bson_t) {
	if !C.mongoc_collection_insert_one(col.collection, doc, C.NULL, C.NULL, C.NULL) {
		println('Error inserting document')
	}
	C.bson_destroy(doc)
}

pub fn (con Connection) disconnect() {
	for db_key, db in con.dbs {
		for col_key, col in db.cols {
			println('removing in memory $col_key collection of $db_key database')
			C.mongoc_collection_destroy(col.collection)
		}
		println('removing in memory $db_key database')
		C.mongoc_database_destroy(db.db)
	}
	C.mongoc_client_destroy(con.client)
	C.mongoc_cleanup()
}
