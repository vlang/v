#include "@VMODROOT/opaque_pgconn.h"

@[typedef]
struct C.PGconn {}

struct DB {
	conn &C.PGconn = unsafe { nil }
}

struct OptionalDB {
	conn ?&C.PGconn
}

fn test_auto_str_for_opaque_c_typedef_ptr_field() {
	db := DB{
		conn: unsafe { &C.PGconn(malloc(1)) }
	}
	assert dump('${db}') == 'DB{
    conn: &C.PGconn{}
}'
}

fn test_auto_str_for_optional_opaque_c_typedef_ptr_field() {
	db := OptionalDB{
		conn: unsafe { &C.PGconn(malloc(1)) }
	}
	assert dump('${db}') == 'OptionalDB{
    conn: &Option(C.PGconn{})
}'
}
