import encoding.csv

fn test_encoding_csv() {
	// test reading
	data := 'name,email,phone,other\njoe,joe@blow.com,0400000000,test\nsam,sam@likesham.com,0433000000,"test quoted field"\n#chris,chris@nomail.com,94444444,"commented row"\nmike,mike@mikesbikes.com,98888888,"bike store"\n'
	mut csv_reader := csv.new_reader(data)

	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count== 1 {
			assert row[0] == 'name'
		}
		if row_count == 2 {
			assert row[0] == 'joe'
		}
		if row_count == 3 {
			assert row[0] == 'sam'
		}
		if row_count == 4 {
			assert row[0] == 'mike'
		}
	}

	assert row_count == 4

	// test writing to come
}
