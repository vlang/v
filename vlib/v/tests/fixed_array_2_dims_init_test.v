module main

struct Uniforms {
	lights [2][4]f32
}

fn r() [4]f32 {
	return [4]f32{}
}

fn test_main() {
	_ := Uniforms{
		lights: [
			r(),
			r(),
		]!
	}
}
