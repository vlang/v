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
pub fn rad(deg f32) f32 {
	return (math.pi / 180.0) * deg
}

// Translate radians to degrees
[inline]
pub fn deg(grad f32) f32 {
	return (180.0 / math.pi) * grad
}

// calculate the Orthographic projection matrix
pub fn ortho(left f32, right f32, bottom f32, top f32, z_near f32, z_far f32) Mat4 {
	rml := right - left
	rpl := right + left
	tmb := top - bottom
	tpb := top + bottom
	fmn := z_far - z_near
	fpn := z_far + z_near
	if fmn != 0 {
		return Mat4{ e: [
				2 / rml, 0      ,       0, -(rpl / rml),
				0      , 2 / tmb,       0, -(tpb / tmb),
				0      ,       0, 2 / fmn, -(fpn / fmn),
				0      ,       0,       0,            1,
			]!
		}
	}
	return Mat4{ e: [
			2 / rml, 0      ,       0, -(rpl / rml),
			0      , 2 / tmb,       0, -(tpb / tmb),
			0      ,       0,       0,            0,
			0      ,       0,       0,            1,
		]!
	}
}

// Calculate the perspective matrix using (fov:fov, ar:aspect_ratio ,n:near_pane, f:far_plane) as parameters
pub fn perspective(fov f32, ar f32, n f32, f f32) Mat4 {
	ctan := f32(1.0 / math.tan(fov * (f32(math.pi) / 360.0))) // for the FOV we use 360 instead 180
	return Mat4{ e: [
		  ctan / ar, 	  0,		                   0, 							0,
			0,		     ctan, 	                     0, 							0,
			0,		        0,		   (n + f) / (n - f), 			     -1.0,
			0,		        0, (2.0 * n * f) / (n - f), 	            0,
		]!
	}
}

// Calculate the look-at matrix
pub fn look_at(eye Vec4, center Vec4, up Vec4) Mat4 {
	f := (center - eye).normalize3()
	s := (f % up).normalize3()
	u := (s % f)

	return Mat4{ e: [
			/* [0][0] */ s.e[0],
			/* [0][1] */ u.e[0],
			/* [0][2] */ - f.e[0],
			/* [0][3] */ 0,

			/* [1][1] */ s.e[1],
			/* [1][1] */ u.e[1],
			/* [1][2] */ - f.e[1],
			/* [1][3] */ 0,

			/* [2][0] */ s.e[2],
			/* [2][1] */ u.e[2],
			/* [2][2] */ - f.e[2],
			/* [2][3] */ 0,

			/* [3][0] */ - (s * eye),
			/* [3][1] */ - (u * eye),
			/* [3][2] */ f * eye,
			/* [3][3] */ 1,
		]!
	}
}


// Get the complete transformation matrix for GLSL demos
pub fn calc_tr_matrices(w f32, h f32, rx f32, ry f32, in_scale f32) Mat4 {
	proj := perspective(60, w / h, 0.01, 10.0)
	view := look_at(Vec4{ e: [f32(0.0), 1.5, 6, 0]! }, Vec4{ e: [f32(0), 0, 0, 0]! }, Vec4{ e: [f32(0), 1.0, 0, 0]! })
	view_proj := view * proj

	rxm := rotate(rad(rx), Vec4{ e: [f32(1), 0, 0, 0]! })
	rym := rotate(rad(ry), Vec4{ e: [f32(0), 1, 0, 0]! })

	model := rym * rxm
	scale_m := scale(Vec4{ e: [in_scale, in_scale, in_scale, 1]! })

	res := (scale_m * model) * view_proj
	return res
}
