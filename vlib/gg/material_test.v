module gg

import math

fn test_shade_color_clamps() {
	c := gg.shade_color(gg.Color{200, 100, 50, 128}, 0.5)
	assert c.r == 100 && c.g == 50 && c.b == 25 && c.a == 128
	bright := gg.shade_color(gg.white, 2.0)
	assert bright.r == 255
}

fn test_mix_color_interpolates() {
	c := gg.mix_color(gg.black, gg.white, 0.5)
	assert c.r >= 127 && c.r <= 128 && c.g >= 127 && c.b >= 127
}

fn test_material_lighting_faces_light() {
	mat := gg.Material3D{
		ambient: 0
		light:   gg.Light3D{ direction: gg.Vec3{0, -1, 0} }
	}
	cam := gg.Camera3D{}
	lit := gg.material_color(&mat, gg.Vec3{}, gg.Vec3{0, 1, 0}, cam)
	dark := gg.material_color(&mat, gg.Vec3{}, gg.Vec3{0, -1, 0}, cam)
	assert lit.r > dark.r
}

fn test_material_fog_fades_with_distance() {
	mat := gg.Material3D{
		color:     gg.white
		fog_color: gg.black
		fog_near:  1
		fog_far:   11
	}
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 0} }
	near := gg.material_color(&mat, gg.Vec3{0, 0, 2}, gg.Vec3{0, 1, 0}, cam)
	far := gg.material_color(&mat, gg.Vec3{0, 0, 10}, gg.Vec3{0, 1, 0}, cam)
	assert near.r > far.r
	assert math.abs(f32(far.r) - 25.0) < 2.0
}
