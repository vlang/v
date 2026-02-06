struct NotFoundErr {
	Error
	id string
}

struct User {}

fn find_by_id(id string) !User {
	return NotFoundErr{
		id: id
	}
}

fn test_embed_error_in_returning_result() {
	find_by_id('id123') or {
		println(err)
		assert true
		return
	}
	assert false
}
