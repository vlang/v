// Code in the shape a source-to-source compiler emits: camelCase names, a
// leading `_`, and type names that do not begin with a capital letter.
@[generated]
module zbrgen

pub const _zbr_c_Scale = 10

pub const zbrNames = ['a', 'b']

pub struct _zbr_ty_Vec {
pub:
	xPos  int
	_yPos int
}

pub fn _zbr_ty_Vec.make(xPos int, _yPos int) _zbr_ty_Vec {
	return _zbr_ty_Vec{
		xPos:  xPos
		_yPos: _yPos
	}
}

pub fn (v _zbr_ty_Vec) scaledSum() int {
	return (v.xPos + v._yPos) * _zbr_c_Scale
}

pub enum _zbr_ty_Mode {
	slowMode
	fastMode
}

pub type _zbr_ty_Value = _zbr_ty_Vec | _zbr_ty_Mode | int

pub const _zbr_c_Origin = _zbr_ty_Vec{
	xPos:  2
	_yPos: 3
}
