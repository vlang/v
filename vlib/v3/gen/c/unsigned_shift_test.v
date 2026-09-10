module c

import v3.types

fn test_unsigned_shift_alias_uses_underlying_width() {
	small := types.Type(types.Alias{
		name:      'Small'
		base_type: types.Type(types.i8_)
	})
	nested := types.Type(types.Alias{
		name:      'NestedSmall'
		base_type: small
	})
	base := unsigned_shift_unalias_type(nested)
	assert base.name() == 'i8'
	unsigned_type, bits := unsigned_shift_parts(base.name())
	assert unsigned_type == 'u8'
	assert bits == '8'
}
