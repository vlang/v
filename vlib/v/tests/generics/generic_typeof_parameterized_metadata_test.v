import typeof_parameterized_model as model
import typeof_parameterized_runtime as rt

fn test_parameterized_typeof_index_matches_explicit_type() {
	assert model.outer_idx[int]() == rt.idx[&model.Cell[int]]()
	assert model.outer_idx[string]() == rt.idx[&model.Cell[string]]()
}

fn test_parameterized_typeof_local_names_match_explicit_type() {
	assert model.generic_names[int]() == model.local_names()
	assert model.generic_names[int]() == '&Node Box[int]'
}

fn test_parameterized_typeof_transported_names_match_explicit_type() {
	assert model.outer_name[int]() == model.plain_name()
	assert model.outer_name[int]() == '&typeof_parameterized_model.Wrap[int]'
	assert model.outer_name[string]() == rt.name[&model.Wrap[string]]()
}
