module gpu

import gg

fn test_material_slots() {
	mut m := new_material('lit')
	m.set_float('time', 1.5)
	m.set_vec3('light', gg.Vec3{1, 2, 3})
	m.set_color('tint', gg.rgba(255, 0, 0, 255))
	assert m.slots.len == 3
	assert m.slot_index('light') == 1
	assert m.slot_index('missing') == -1
	block := m.uniform_block()
	assert block[0] == 1.5
	assert block[1] == 1 && block[2] == 2 && block[3] == 3
	assert block[4] == 1 && block[5] == 0 && block[6] == 0
	m.set_float('time', 2.5)
	assert m.uniform_block()[0] == 2.5
}

fn test_default_source() {
	s := default_source()
	assert s.vs.len > 0 && s.fs.len > 0
	assert s.label == 'gg.default'
}
