import sqlite

fn ret() !(int, sqlite.DB) {
	db := sqlite.connect(':memory:')!

	return 0, db
}

fn test_multiret_with_result() {
	_, db := ret()!
	println(db)
	assert true
}
