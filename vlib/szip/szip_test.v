import szip
import os

fn test_szip() {

// create temp files for zip/unzip test
	test_path := 'zip files'
	test_out_zip := 'v_test_zip.zip'
	
	os.mkdir(test_path) or { panic(err) }
	os.write_file(test_path + os.path_separator + 'file_1.txt', 'file one') or { panic(err) }
	os.write_file(test_path + os.path_separator + 'file_2.txt', 'file file two') or { panic(err) }

	// get list files from directory
	mut files := os.ls(test_path) or { panic(err) }
	for mut file in files {
		file = os.getwd() + os.path_separator + test_path + os.path_separator + *file
	}

// zip files
	szip.zip_files(files, test_out_zip) or { panic(err) }
	assert os.exists(test_out_zip)

// remove files before next test
	os.rm(test_path + os.path_separator + 'file_1.txt') or { panic(err) }
	os.rm(test_path + os.path_separator + 'file_2.txt') or { panic(err) }

// extract test
	szip.extract_zip_to_dir(test_out_zip, test_path) or { panic(err) }
	assert os.exists(test_path + os.path_separator + 'file_1.txt')
	assert os.exists(test_path + os.path_separator + 'file_2.txt')

// clear temp files
	// remove temp files and dir
	os.rmdir_all (test_path) or { panic(err) }
	os.rm(test_out_zip) or { }
}