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

struct OverwritingStruct {
	Struct
}

fn (s OverwritingStruct) get() string {
	return 'overwritten $s.msg'
}

struct EmptyStruct {
	Struct
}

fn test_struct_embedding() {
	getter1 := Getter(&OverwritingStruct{
		msg: '1'
	})
	getter2 := Getter(&EmptyStruct{
		msg: '2'
	})
	getter3 := Getter(OverwritingStruct{
		msg: '3'
	})
	getter4 := Getter(EmptyStruct{
		msg: '4'
	})
	s5 := &OverwritingStruct{
		msg: '5'
	}
	getter5 := Getter(s5)
	s6 := &EmptyStruct{
		msg: '6'
	}
	getter6 := Getter(s6)
	s7 := OverwritingStruct{
		msg: '7'
	}
	getter7 := Getter(s7)
	s8 := EmptyStruct{
		msg: '8'
	}
	getter8 := Getter(s8)

	assert getter1.get() == 'overwritten 1'
	assert getter2.get() == '2'
	assert getter3.get() == 'overwritten 3'
	assert getter4.get() == '4'
	assert getter5.get() == 'overwritten 5'
	assert getter6.get() == '6'
	assert getter7.get() == 'overwritten 7'
	assert getter8.get() == '8'
}
