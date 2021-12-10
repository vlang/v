struct Empty {}

type Text = string

type SumTypeB = Empty | Text

struct DataStruct {
	y SumTypeB
}

fn isok(a DataStruct, b DataStruct) bool {
	if a.y is Text {
		if b.y is Text {
			return a.y == b.y
		}
	}
	return false
}

fn test_nested_if_smartcast_selector_exprs() {
	a := DataStruct{
		y: Text('da')
	}
	b := DataStruct{
		y: Text('da')
	}
	assert isok(a, b)
}
