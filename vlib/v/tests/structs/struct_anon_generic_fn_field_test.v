pub struct ChunksArrayDecoder[T] {
	callback fn (t T) = unsafe { nil }
}

pub fn new_chunks_array_decoder[T](callback fn (t T)) &ChunksArrayDecoder[T] {
	return &ChunksArrayDecoder[T]{
		callback: callback
	}
}

struct Person {
	name string
	age  int
	kgs  f32
}

@[heap]
struct People {
mut:
	persons int
	kgs     f32
}

fn (mut people People) callback(person Person) {
	people.persons++
	people.kgs += person.kgs
}

fn module_callback(person Person) {
	println('person: ${person}')
}

fn test_main() {
	mut people := &People{}
	mut decoder := new_chunks_array_decoder[Person](people.callback)
	_ = decoder
}
