module gg

import math

fn test_shadow_map_size_validation() {
	sm := new_shadow_map3d(2, Vec3{0, -1, 0}) or {
		assert err.msg().contains('gg.E0032')
		return
	}
	assert false
	_ = sm
}

fn test_shadow_map_zero_direction() {
	sm := new_shadow_map3d(64, Vec3{}) or {
		assert err.msg().contains('gg.E0033')
		return
	}
	assert false
	_ = sm
}

fn test_shadow_map_sample_outside_is_lit() {
	mut sm := new_shadow_map3d(16, Vec3{0, -1, 0})!
	sm.light_dir = Vec3{0, -1, 0}
	sm.view_proj = sm.light_view_proj(Vec3{}, 10, 0.1, 40)
	// A point far outside the light's frustum must be fully lit.
	assert math.abs(sm.sample(Vec3{1000, 1000, 1000}) - 1.0) < 1e-5
}

fn test_shadow_map_sample_inside_can_be_shadowed() {
	mut sm := new_shadow_map3d(16, Vec3{0, -1, 0})!
	sm.view_proj = sm.light_view_proj(Vec3{}, 10, 0.1, 40)
	sm.strength = 1.0
	sm.bias = 0.0
	// Fill the map with a near depth; a point at far depth is in shadow.
	for i in 0 .. sm.depth.len {
		sm.depth[i] = -1.0
	}
	assert sm.sample(Vec3{}) < 0.5
}

fn test_shadow_color_multiplies_channels() {
	c := shadow_color(Color{200, 100, 50, 255}, 0.5)
	assert c.r == 100 && c.g == 50 && c.b == 25 && c.a == 255
}
