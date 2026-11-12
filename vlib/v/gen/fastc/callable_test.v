module fastc

import v.flat

fn test_callable_type_roundtrip_retains_parameter_and_option_types() {
	for parameters in [([]u32{}), [flat.source_type_int], [flat.source_type_id('Item*')],
		[flat.source_type_id('Map_string_int')]] {
		info := FastcFunctionTypeInfo{
			return_type:     flat.source_type_void
			parameter_types: flat.TypeTextList{ ids: parameters }
		}
		name := fastc_callable_type_name(info)
		signature := fastc_callable_signature(flat.source_type_id(name)) or { panic('cannot decode ${name}') }
		assert signature.return_type == flat.source_type_void
		assert signature.parameter_types.ids == parameters
	}
}
