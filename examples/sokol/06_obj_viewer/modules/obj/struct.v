/**********************************************************************
*
* .obj loader
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
module obj

import gg.m4
import sokol.gfx

// part struct mantain the fae indexes list
pub struct Part {
pub mut:
	faces    [][][3]int // v n t index order, if -1 not available
	name     string
	material string
}

// materias struct, all Ks and Ns are stored as maps of string
pub struct Material {
pub mut:
	name string
	ks   map[string]m4.Vec4
	ns   map[string]f32
	maps map[string]string
}

// render data used for the rendering
pub struct Render_data {
pub mut:
	pipeline gfx.Pipeline
	bind     gfx.Bindings
	n_vert   u32
	material string
}

// base object parts struct
pub struct ObjPart {
pub mut:
	v  []m4.Vec4 // position
	vn []m4.Vec4 // normals
	vp []m4.Vec4 // vertex params
	vt []m4.Vec4 // textures

	name          string
	part          []Part               // parts of the ObjPart
	mat           []Material           // list of the materials of the ObjPart
	mat_map       map[string]int       // maping material name to its material index
	texture       map[string]gfx.Image // GPU loaded texture map
	material_file string // .mtl file name for the .obj

	rend_data []Render_data // render data used for the rendering

	t_m m4.Mat4 = m4.unit_m4() // transform matrix for this ObjPart
	// child []ObjPart           // childs
	// stats
	min    m4.Vec4 // min 3d position in the ObjPart
	max    m4.Vec4 // max 3d position in the ObjPart
	radius f32     // bounding circle radius of the ObjPart
}

// used in to pass the matrices to the shader
pub struct Mats {
pub mut:
	mv  m4.Mat4
	mvp m4.Mat4
	nm  m4.Mat4
}

// data passed to the vertex shader
pub struct Tmp_vs_param {
pub mut:
	mv  m4.Mat4
	mvp m4.Mat4
	nm  m4.Mat4
}

// data passed to the pixel shader
pub struct Tmp_fs_param {
pub mut:
	ligth m4.Vec4
	ka    m4.Vec4 = m4.Vec4{
		e: [f32(0.1), 0.0, 0.0, 1.0]!
	}
	kd m4.Vec4 = m4.Vec4{
		e: [f32(0.5), 0.5, 0.5, 1.0]!
	}
	ks m4.Vec4 = m4.Vec4{
		e: [f32(1.0), 1.0, 1.0, 1.0]!
	}
}

// shader data for the rendering
pub struct Shader_data {
pub mut:
	vs_data &Tmp_vs_param = unsafe { nil }
	vs_len  int
	fs_data &Tmp_fs_param = unsafe { nil }
	fs_len  int
}
