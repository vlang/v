import encoding.csv

fn test_encoding_csv_reader() {
	data := 'name,email,phone,other\njoe,joe@blow.com,0400000000,test\nsam,sam@likesham.com,0433000000,"test quoted field"\n#chris,chris@nomail.com,94444444,"commented row"\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
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

fn test_encoding_csv_reader_with_custom_delimiter() {
	data := 'name|email|phone|other\njoe|joe@blow.com|0400000000|test\nsam|sam@likesham.com|0433000000|"test quoted field"\n#chris|chris@nomail.com|94444444|"commented row"\n'
	mut csv_reader := csv.new_reader(data, delimiter: `|`)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
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
		row := csv_reader.read() or { break }
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
		row := csv_reader.read() or { break }
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
		row := csv_reader.read() or { break }
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
		csv_reader.read() or { break }
		row_count++
	}
	assert row_count == 2
}

fn test_last_field_empty() {
	data := '"name","description","value"\n"one","first","1"\n"two","second",\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
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
			assert row[2] == ''
		}
	}
	assert row_count == 3
}

fn test_empty_fields_no_quotes() {
	data := '1,2,3,4\n,6,7,8\n9,,11,12\n13,14,,16\n17,18,19,\n'

	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
		row_count++
		if row_count == 1 {
			assert row[0] == '1'
			assert row[1] == '2'
			assert row[2] == '3'
			assert row[3] == '4'
		} else if row_count == 2 {
			assert row[0] == ''
			assert row[1] == '6'
			assert row[2] == '7'
			assert row[3] == '8'
		} else if row_count == 3 {
			assert row[0] == '9'
			assert row[1] == ''
			assert row[2] == '11'
			assert row[3] == '12'
		} else if row_count == 4 {
			assert row[0] == '13'
			assert row[1] == '14'
			assert row[2] == ''
			assert row[3] == '16'
		} else if row_count == 5 {
			assert row[0] == '17'
			assert row[1] == '18'
			assert row[2] == '19'
			assert row[3] == ''
		}
	}
	assert row_count == 5
}

fn test_empty_line() {
	data := '"name","description","value"\n\n\n"one","first","1"\n\n"two","second",\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
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
	assert row_count == 3
}

fn test_field_multiple_line() {
	data := '"name","multiple

 line","value"\n"one","first","1"\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
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
	assert row_count == 2
}

fn test_field_quotes_for_parts() {
	data := 'a1,"b1",c1\n"a2",b2,c2\na3,b3,"c3"\na4,b4,c4\n'
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
		row_count++
		if row_count == 1 {
			assert row[0] == 'a1'
			assert row[1] == 'b1'
			assert row[2] == 'c1'
		} else if row_count == 2 {
			assert row[0] == 'a2'
			assert row[1] == 'b2'
			assert row[2] == 'c2'
		} else if row_count == 3 {
			assert row[0] == 'a3'
			assert row[1] == 'b3'
			assert row[2] == 'c3'
		} else if row_count == 4 {
			assert row[0] == 'a4'
			assert row[1] == 'b4'
			assert row[2] == 'c4'
		}
	}
	assert row_count == 4
}

fn test_field_double_quotes() {
	row1 := '11,"12\n13"\n'
	row2 := '21,"2""2""\n23"\n'
	row3 := '"3""1""",32\n'
	data := row1 + row2 + row3
	mut csv_reader := csv.new_reader(data)
	mut row_count := 0
	for {
		row := csv_reader.read() or { break }
		row_count++
		if row_count == 1 {
			assert row[0] == '11'
			assert row[1] == '12\n13'
		} else if row_count == 2 {
			assert row[0] == '21'
			assert row[1] == '2"2"\n23'
		} else if row_count == 3 {
			assert row[0] == '3"1"'
			assert row[1] == '32'
		}
	}
	assert row_count == 3
}
