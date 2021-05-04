import szip
import os

const (
	test_out_zip = 'v_test_zip.zip'
	test_path    = 'zip files'
	fpath1       = os.join_path(test_path, 'file_1.txt')
	fpath2       = os.join_path(test_path, 'file_2.txt')
)

fn test_szip_create_temp_files() ? {
	os.chdir(os.temp_dir())
	os.rmdir_all(test_path) or {}
	os.mkdir(test_path) ?
	os.write_file(fpath1, 'file one') ?
	os.write_file(fpath2, 'file two') ?
	assert os.exists(fpath1)
	assert os.exists(fpath2)
}

fn test_zipping_files() ? {
	files := (os.ls(test_path) ?).map(os.join_path(test_path, it))
	szip.zip_files(files, test_out_zip) ?
	assert os.exists(test_out_zip)
}

fn test_extract_zipped_files() ? {
	os.rm(fpath1) ?
	os.rm(fpath2) ?
	szip.extract_zip_to_dir(test_out_zip, test_path) ?
	assert os.exists(fpath1)
	assert os.exists(fpath2)
	assert (os.read_file(fpath1) ?) == 'file one'
	assert (os.read_file(fpath2) ?) == 'file two'
	os.rmdir_all(test_path) ?
	os.rm(test_out_zip) or {}
}
