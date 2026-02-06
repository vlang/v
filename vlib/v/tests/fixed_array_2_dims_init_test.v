module main

struct Uniforms {
	lights [2][4]f32
}

fn r() [4]f32 {
	return [f32(1.1), 1.2, 1.3, 1.4]!
}

fn test_main() {
	v := Uniforms{
		lights: [
			r(),
			r(),
		]!
	}
	assert v.lights[0][0] == f32(1.1)
	assert v.lights[0][1] == f32(1.2)
	assert v.lights[0][2] == f32(1.3)
	assert v.lights[0][3] == f32(1.4)
}
