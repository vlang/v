import encoding.csv

fn test_no_ending() {
	data := 'x,y,d
a,b
w'
	mut parser := csv.new_reader(data)
	mut arr := []string{}
	for {
		items := parser.read() or { break }
		arr << items.join('-')
	}
	dump(arr)
	assert arr[0].str() == 'x-y-d'
	assert arr[1].str() == 'a-b'
	assert arr[2].str() == 'w'
}

fn test_with_ending() {
	data := 'x,y,d
a,b
w
'
	mut parser := csv.new_reader(data)
	mut arr := []string{}
	for {
		items := parser.read() or { break }
		arr << items.join('-')
	}
	dump(arr)
	assert arr[0].str() == 'x-y-d'
	assert arr[1].str() == 'a-b'
	assert arr[2].str() == 'w'
}
