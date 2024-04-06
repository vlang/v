// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module csv

import os

fn test_new_reader_from_file() {
	test_file_path_for_reader := os.join_path(os.temp_dir(), 'test_new_reader_from_file.csv')

	text := 'id,bonus,amount,yes\n1,bomb,1,true\n2,rocket,1,false,\n3,lightning,2,2\n'
	os.write_file(test_file_path_for_reader, text)!

	mut reader := new_reader_from_file(test_file_path_for_reader)!
	mut writer := new_writer()

	for {
		row := reader.read() or { break }
		writer.write(row) or { panic(err) }
	}

	assert text == writer.str()

	os.rm(test_file_path_for_reader)!
}
