import encoding.csv

fn test_encoding_csv_reader() {
	data := 'name,email,phone,other\njoe,joe@blow.com,0400000000,test\nsam,sam@likesham.com,0433000000,"test quoted field"\n#chris,chris@nomail.com,94444444,"commented row"\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'email'
			assert row[2] == 'phone'
			assert row[3] == 'other'
		} else if row_count == 2 {
			assert row[0] == 'joe'
			assert row[1] == 'joe@blow.com'
			assert row[2] == '0400000000'
			assert row[3] == 'test'
		} else if row_count == 3 {
			assert row[0] == 'sam'
			assert row[1] == 'sam@likesham.com'
			assert row[2] == '0433000000'
			// quoted field
			assert row[3] == 'test quoted field'
		}
	}
	assert row_count == 3
}

fn test_line_break_lf() {
	lf_data := 'name,email\njoe,joe@blow.com\n'
	mut csv_reader := csv.new_reader(lf_data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'email'
		} else if row_count == 2 {
			assert row[0] == 'joe'
			assert row[1] == 'joe@blow.com'
		}
	}
	assert row_count == 2
}

fn test_line_break_cr() {
	cr_data := 'name,email\rjoe,joe@blow.com\r'
	mut csv_reader := csv.new_reader(cr_data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'email'
		} else if row_count == 2 {
			assert row[0] == 'joe'
			assert row[1] == 'joe@blow.com'
		}
	}
	assert row_count == 2
}

fn test_line_break_crlf() {
	crlf_data := 'name,email\r\njoe,joe@blow.com\r\n'
	mut csv_reader := csv.new_reader(crlf_data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'email'
		} else if row_count == 2 {
			assert row[0] == 'joe'
			assert row[1] == 'joe@blow.com'
		}
	}
	assert row_count == 2
}

fn test_no_line_ending() {
	data := 'name,email,phone,other\njoe,joe@blow.com,0400000000,test'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		csv_reader.read() or {
			break
		}
		row_count++
	}
	assert row_count == 2
}

fn test_last_field_empty() {
	data := '"name","description","value"\n"one","first","1"\n"two","second",\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'description'
			assert row[2] == 'value'
		} else if row_count == 2 {
			assert row[0] == 'one'
			assert row[1] == 'first'
			assert row[2] == '1'
		} else if row_count == 3 {
			assert row[0] == 'two'
			assert row[1] == 'second'
		}
	}
}

fn test_empty_line() {
	data := '"name","description","value"\n\n\n"one","first","1"\n\n"two","second",\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'description'
			assert row[2] == 'value'
		} else if row_count == 2 {
			assert row[0] == 'one'
			assert row[1] == 'first'
			assert row[2] == '1'
		} else if row_count == 3 {
			assert row[0] == 'two'
			assert row[1] == 'second'
		}
	}
}

fn test_field_multiple_line() {
	data := '"name","multiple

 line","value"\n"one","first","1"'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or {
			break
		}
		row_count++
		if row_count == 1 {
			assert row[0] == 'name'
			assert row[1] == 'multiple\n\n line'
			assert row[2] == 'value'
		} else if row_count == 2 {
			assert row[0] == 'one'
			assert row[1] == 'first'
			assert row[2] == '1'
		}
	}
}
