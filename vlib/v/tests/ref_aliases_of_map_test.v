type FooMap = map[string]int
type FooArr = []string
type FooStr = string

fn test_reference_aliases_of_map() {
    addr1 := &FooMap(map[string]int{})
	 println('${addr1:p}')

	 assert true
}

fn test_reference_aliases_of_array() {
	 addr2 := &FooArr([]string{})
	 println('${addr2:p}')

	 assert true
}

fn test_reference_aliases_of_string() {
	 addr3 := &FooStr('hello')
	 println('${addr3:p}')

	 assert true
}
