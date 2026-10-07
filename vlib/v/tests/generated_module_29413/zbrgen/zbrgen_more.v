@[generated]
module zbrgen

pub struct _zbr_box[T] {
pub:
	value T
}

// The types are declared in a sibling file of this module.
pub fn _zbr_fn_origin() _zbr_ty_Vec {
	return _zbr_ty_Vec{}
}

pub fn _zbr_fn_describe(v _zbr_ty_Value) string {
	return match v {
		_zbr_ty_Vec { 'vec ${v.xPos}' }
		_zbr_ty_Mode { 'mode ${v}' }
		int { 'int ${v}' }
	}
}
