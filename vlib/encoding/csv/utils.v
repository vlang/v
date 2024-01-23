// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module csv

import os

// new_reader_from_file create a csv reader from a file
pub fn new_reader_from_file(csv_file_path string, config ReaderConfig) !&Reader {
	csv_file_content := os.read_file(csv_file_path)!
	return new_reader(csv_file_content, config)
}
