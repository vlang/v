module loaders

fn test_manager_progress() {
	mut m := new_loading_manager()
	m.begin(4)
	assert m.items_total == 4
	assert m.fraction() == 0
	m.progress('a.glb')
	m.progress('b.obj')
	assert m.items_loaded == 2
	assert m.fraction() == 0.5
	m.progress('c.gltf')
	m.progress('d.obj')
	assert m.fraction() == 1
	m.finish()
	m.fail('missing', 'not found')
}

fn test_manager_cache() {
	mut m := new_loading_manager()
	assert !m.has('a')
	assert m.get('a') == ''
	m.put('a', 'data')
	assert m.has('a')
	assert m.get('a') == 'data'
	m.clear_cache()
	assert !m.has('a')
}

fn test_manager_negative_total() {
	mut m := new_loading_manager()
	m.begin(-2)
	assert m.items_total == 0
	assert m.fraction() == 0
}
