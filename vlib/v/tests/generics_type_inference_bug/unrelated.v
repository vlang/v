module generics_type_inference_bug

// This file contains an UNRELATED function with Vec3[f32]
// The bug is that this affects type inference in unrelated code!

fn unrelated_function_with_f32(a Vec3[f32]) bool {
	return false
}
