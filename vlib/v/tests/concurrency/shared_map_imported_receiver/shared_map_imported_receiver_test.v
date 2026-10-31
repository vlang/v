import taglib

fn add_imported_tag(shared boxes map[string]taglib.Box) {
	lock boxes {
		boxes.add_tag('a', 'x')
	}
}

fn test_imported_mutable_map_receiver_uses_shared_map_payload() {
	shared boxes := map[string]taglib.Box{}
	lock boxes {
		boxes['a'] = taglib.Box{}
		boxes.add_tag('a', 'first')
		assert boxes.tags_for('a') == ['first']
	}
	task := spawn add_imported_tag(shared boxes)
	task.wait()
	rlock boxes {
		assert boxes.tags_for('a') == ['first', 'x']
		assert boxes['a'].tags == ['first', 'x']
	}
}
