module scene

import gg

fn test_node_hierarchy() {
	mut root := new_node('root')
	mut child := new_node('child')
	mut grand := new_node('grand')
	root.add(mut child)
	child.add(mut grand)
	assert root.children.len == 1
	assert child.parent == root
	assert root.find('grand') != none
	assert root.find('missing') == none
	root.remove(mut child)
	assert root.children.len == 0
	assert isnil(child.parent)
}

fn test_world_position() {
	mut root := new_node('root')
	root.position = gg.Vec3{10, 0, 0}
	mut child := new_node('child')
	child.position = gg.Vec3{2, 3, 0}
	root.add(mut child)
	p := child.world_position()
	assert p.x == 12 && p.y == 3 && p.z == 0
}

fn test_add_rejects_ancestor_cycle() {
	mut a := new_node('a')
	mut b := new_node('b')
	a.add(mut b)
	// b.add(a) would close the cycle a -> b -> a. It must be rejected.
	b.add(mut a)
	assert isnil(a.parent)
	assert a.children.len == 1
	assert b.children.len == 0
	// These would hang forever if the cycle had been created.
	assert a.find('a') != none
	assert a.find('b') != none
}

fn test_visibility() {
	mut root := new_node('root')
	mut child := new_node('child')
	root.add(mut child)
	assert child.is_visible()
	root.visible = false
	assert !child.is_visible()
}

fn test_signal() {
	mut sig := new_signal[string]()
	assert sig.len() == 0
	sig.listen(fn (msg string) {
		assert msg == 'hi'
	})
	assert sig.len() == 1
	sig.emit('hi')
}
