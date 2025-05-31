#insert "@VMODROOT/cstruct_alias.h"

@[typedef]
pub struct C.Camera3D {
}

pub type Camera3D = C.Camera3D
pub type Camera = C.Camera3D

// Update camera position for selected mode
pub fn C.UpdateCamera(camera &Camera, mode int)

enum CameraMode {
	camera_free
}

fn test_main() {
	mut camera := &Camera{}
	C.UpdateCamera(camera, int(CameraMode.camera_free))

	mut camera3d := &Camera3D{}
	C.UpdateCamera(camera3d, int(CameraMode.camera_free))
}
