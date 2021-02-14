/**********************************************************************
*
* Simply vector/matrix graphic utility
*
* Copyright (c) 2021 Dario Deledda. All rights reserved.
* Use of this source code is governed by an MIT license
* that can be found in the LICENSE file.
*
* TODO:
**********************************************************************/
module m4
import math 

// Translate degrees to radians
[inline]
pub
fn rad(deg f32) f32 {
	return (math.pi / 180.0) * deg
}

// Translate radians to degrees
[inline]
pub
fn deg(grad f32) f32 {
	return (180.0 / math.pi) * grad
}

// Calculate the perspective matrix
[direct_array_access]
pub
fn perspective(fov f32, ar f32, n f32, f f32) Mat4 { unsafe{
	ctan := f32(1.0 / math.tan(fov * (f32(math.pi) / 360.0)))  // for the FOV we use 360 instead 180
	return Mat4{e:[
			ctan / ar , 0            , 0                     , 0 
			0         , ctan         , 0                     , 0 
			0         , 0            , (n + f) / (n - f)     , -1.0
			0         , 0            , (2.0 * n * f)/ (n - f), 0
	]!}
}}

// Calculate the look-at matrix
[direct_array_access]
pub
fn look_at(eye Vec4, center Vec4, up Vec4) Mat4 { unsafe{
	f := (center - eye).normalize3()
	s := (f % up).normalize3()
	u := (s % f)

	return Mat4{e:[
		/*[0][0]*/ s.e[0],
		/*[0][1]*/ u.e[0],
		/*[0][2]*/ -f.e[0],
		/*[0][3]*/0,
		
		/*[1][1]*/ s.e[1],
		/*[1][1]*/ u.e[1],
		/*[1][2]*/ -f.e[1],
		/*[1][3]*/0,
		
		/*[2][0]*/ s.e[2],
		/*[2][1]*/ u.e[2],
		/*[2][2]*/ -f.e[2],
		/*[2][3]*/0,
		
		/*[3][0]*/ -(s * eye),
		/*[3][1]*/ -(u * eye),
		/*[3][2]*/ f * eye,
		/*[3][3]*/1
	]!}

}}

/*
	hmm_mat4 proj = HMM_Perspective(60.0f, w/h, 0.01f, 10.0f);
	hmm_mat4 view = HMM_LookAt(HMM_Vec3(0.0f, 1.5f, 6.0f), HMM_Vec3(0.0f, 0.0f, 0.0f), HMM_Vec3(0.0f, 1.0f, 0.0f));
	hmm_mat4 view_proj = HMM_MultiplyMat4(proj, view);
	//state.rx += 1.0f; state.ry += 2.0f;

	
	hmm_mat4 rxm = HMM_Rotate(rx, HMM_Vec3(1.0f, 0.0f, 0.0f));
	hmm_mat4 rym = HMM_Rotate(ry, HMM_Vec3(0.0f, 1.0f, 0.0f));

	hmm_mat4 model = HMM_MultiplyMat4(rxm, rym);
	hmm_mat4 scale_mx = HMM_Scale(HMM_Vec3(scale, scale, scale));
	model = HMM_MultiplyMat4(model, scale_mx);
	hmm_mat4 tmp_res = HMM_MultiplyMat4(view_proj, model);
*/

// Get the complete transformation matrix for GLSL demos
pub
fn calc_tr_matrices(w f32, h f32, rx f32, ry f32, in_scale f32) Mat4{
	proj := perspective(60, w/h, 0.01, 10.0)
	view := look_at(Vec4{e:[f32(0.0),1.5,6,0]!}, Vec4{e:[f32(0),0,0,0]!}, Vec4{e:[f32(0),1.0,0,0]!})
	view_proj := view * proj 
	
	rxm := rotate(m4.rad(rx), Vec4{e:[f32(1),0,0,0]!})
	rym := rotate(m4.rad(ry), Vec4{e:[f32(0),1,0,0]!})
	
	model :=  rym * rxm
	scale_m := scale(Vec4{e:[in_scale, in_scale, in_scale, 1]!})
	
	res :=  (scale_m * model)* view_proj
	return res
}