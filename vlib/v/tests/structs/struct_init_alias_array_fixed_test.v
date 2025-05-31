module main

type Mat4 = [16]f32

struct VsParams {
	mv  Mat4
	mvp Mat4
}

struct Camera {
	mv  Mat4
	mvp Mat4
}

fn test_main() {
	camera := Camera{}
	vs_uniforms := VsParams{
		mv:  camera.mv
		mvp: camera.mvp
	}
	assert vs_uniforms.mv.len == 16
	assert vs_uniforms.mvp.len == 16
}
