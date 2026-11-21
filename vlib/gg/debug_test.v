module gg

fn test_debug_log() {
	mut log := new_debug_log()
	log.max = 3
	assert log.visible
	log.add('a')
	log.add('b')
	log.add('c')
	log.add('d')
	assert log.lines.len == 3
	recent := log.recent(2)
	assert recent.len == 2 && recent[0] == 'c' && recent[1] == 'd'
	assert log.recent(0).len == 0
	log.clear()
	assert log.lines.len == 0
}

fn test_frame_graph() {
	mut g := FrameGraph{ max: 3 }
	g.push(0.01)
	g.push(0.03)
	g.push(0.02)
	g.push(0.04)
	assert g.samples.len == 3
	assert (g.average() - 0.03) < 0.0001
	assert g.worst() == 0.04
	assert FrameGraph{}.average() == 0
}

fn test_gizmos() {
	mut g := new_gizmos()
	g.line(Vec2{0, 0}, Vec2{1, 1}, red)
	g.circle(Vec2{2, 2}, 5, blue)
	g.rect(Vec2{0, 0}, Vec2{3, 3}, green)
	g.cross(Vec2{4, 4}, 2, white)
	assert g.items.len == 4
	assert g.items[0].kind == .line
	assert g.items[1].kind == .circle
	g.clear()
	assert g.items.len == 0
}
