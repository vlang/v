module main

interface Getter {
	get() string
}

struct Struct {
	msg string
}

fn (s Struct) get() string {
	return s.msg
}

struct EmbeddingStruct {
	Struct
}

fn (s EmbeddingStruct) get() string {
	return 'embedded: ${s.msg}'
}

fn test_struct_embedding() {
	s1 := EmbeddingStruct{
		msg: '1'
	}
	getter1 := Getter(s1)
	s2 := &EmbeddingStruct{
		msg: '2'
	}
	getter2 := Getter(s2)
	getter3 := Getter(EmbeddingStruct{
		msg: '3'
	})
	getter4 := Getter(&EmbeddingStruct{
		msg: '4'
	})

	assert getter1.get() == 'embedded: 1'
	assert getter2.get() == 'embedded: 2'
	assert getter3.get() == 'embedded: 3'
	assert getter4.get() == 'embedded: 4'
}

struct EmptyStruct {}

fn (s EmptyStruct) get() string {
	return 'empty'
}

struct EmbeddingEmptyStruct {
	EmptyStruct
}

fn test_empty_struct_embedding() {
	s1 := EmbeddingEmptyStruct{}
	getter1 := Getter(s1)
	s2 := &EmbeddingEmptyStruct{}
	getter2 := Getter(s2)
	getter3 := Getter(EmbeddingEmptyStruct{})
	getter4 := Getter(&EmbeddingEmptyStruct{})

	assert getter1.get() == 'empty'
	assert getter2.get() == 'empty'
	assert getter3.get() == 'empty'
	assert getter4.get() == 'empty'
}
