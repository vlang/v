import json

@[heap]
struct Logger {}

struct SaveParams[T] {
	object &T
}

fn (mut l Logger) save[T](p SaveParams[T]) string {
	return json.encode(p.object)
}

struct Object {
	s1 string = 'a string'
}

fn test_generic_fn_short_struct_init_reference_inference() {
	mut logger := &Logger{}
	assert logger.save(object: &Object{}) == '{"s1":"a string"}'
}
