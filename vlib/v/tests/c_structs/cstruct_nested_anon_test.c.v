module main

#insert "@VMODROOT/deep.h"

struct C.DeepStruct {
	A1 int
	S1 struct {
		A2 int
		S2 struct {
			A3 int
			S3 struct {
				A4 int
			}
		}
	}
}

fn test_cstruct_nested_anon() {
	x := C.DeepStruct{}
	y := C.DeepStruct{}
	dump(x)
	assert x == y
}
