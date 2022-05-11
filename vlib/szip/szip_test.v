import szip
import os

const (
	test_out_zip = 'v_test_zip.zip'
	test_dir_zip = 'v_test_dir_zip.zip'
	test_path    = 'zip files'
	test_path2   = '.zip folder'
	test_path3   = 'test zip folder'
	test_path3_1 = os.join_path(test_path3, '1', '1')
	test_path3_2 = os.join_path(test_path3, '2', '1')
	test_path3_3 = os.join_path(test_path3, '3', '1')
	test_path3_4 = os.join_path(test_path3, '4', '1')
	fname1       = 'file_1.txt'
	fpath1       = os.join_path(test_path, fname1)
	fname2       = 'file_2.txt'
	fpath2       = os.join_path(test_path, fname2)
	fname3       = '.New Text Document.txt'
	fpath3       = os.join_path(test_path2, fname3)
	fname4       = 'file.txt'
	fpath4       = os.join_path(test_path3_1, fname4)
	fpath5       = os.join_path(test_path3_2, fname4)
	fpath6       = os.join_path(test_path3_4, fname4)
)

fn cleanup() {
	os.chdir(os.temp_dir()) or {}
	os.rmdir_all(test_path) or {}
	os.rmdir_all(test_path2) or {}
	os.rmdir_all(test_path3) or {}
	os.rm(test_out_zip) or {}
	os.rm(test_dir_zip) or {}
}

fn testsuite_begin() ? {
	cleanup()
}

fn testsuite_end() ? {
	cleanup()
}

fn test_szip_create_temp_files() ? {
	os.mkdir(test_path)?
	os.mkdir(test_path2)?
	os.write_file(fpath1, 'file one')?
	os.write_file(fpath2, 'file two')?
	os.write_file(fpath3, 'file three')?
	assert os.exists(fpath1)
	assert os.exists(fpath2)
	assert os.exists(fpath3)
}

fn test_zipping_files() ? {
	mut files := (os.ls(test_path)?).map(os.join_path(test_path, it))
	files << (os.ls(test_path2)?).map(os.join_path(test_path2, it))
	szip.zip_files(files, test_out_zip)?
	assert os.exists(test_out_zip)
	os.rm(fpath1)?
	os.rm(fpath2)?
	os.rm(fpath3)?
}

fn test_extract_zipped_files() ? {
	szip.extract_zip_to_dir(test_out_zip, test_path)?
	szip.extract_zip_to_dir(test_out_zip, test_path2)?
	assert os.exists(fpath1)
	assert os.exists(fpath2)
	assert os.exists(fpath3)
	assert (os.read_file(fpath1)?) == 'file one'
	assert (os.read_file(fpath2)?) == 'file two'
	assert (os.read_file(fpath3)?) == 'file three'
	cleanup()
}

fn test_reading_zipping_files() ? {
	n_files := 2
	mut file_name_list := []string{}
	for i in 0 .. n_files {
		file_name_list << 'file_${i:02}.txt'
	}

	cleanup()
	os.mkdir(test_path)?
	os.mkdir(test_path2)?
	os.write_file(fpath3, 'file three')?
	for c, f_name in file_name_list {
		tmp_path := os.join_path(test_path, f_name)
		os.write_file(tmp_path, 'file ${c:02}')?
		assert os.exists(tmp_path)
	}
	files := (os.ls(test_path)?).map(os.join_path(test_path, it))

	szip.zip_files(files, test_out_zip)?
	assert os.exists(test_out_zip)

	mut zp := szip.open(test_out_zip, szip.CompressionLevel.no_compression, szip.OpenMode.read_only)?
	n_entries := zp.total()?
	assert n_entries == n_files

	unsafe {
		data_len := 'file XX'.len
		buf_size := 32
		buf := malloc(data_len * 2)

		for _ in 0 .. n_files {
			zp.open_entry_by_index(0)?
			name := zp.name()
			assert name in file_name_list

			zp.read_entry_buf(buf, buf_size)?
			buf[data_len] = 0
			tmp_str := tos(buf, data_len)

			assert tmp_str[0..4] == 'file'
			assert tmp_str[5..7] == name[5..7]

			zp.close_entry()
		}

		free(buf)
	}
	zp.close()
}

fn test_zip_folder() ? {
	cleanup()
	os.mkdir_all(test_path3_1)?
	os.mkdir_all(test_path3_2)?
	os.mkdir_all(test_path3_3)?
	os.mkdir_all(test_path3_4)?
	os.write_file(fpath4, '4')?
	os.write_file(fpath5, '5')?
	os.write_file(fpath6, '6')?

	szip.zip_folder(test_path3, test_dir_zip)?
	assert os.exists(test_dir_zip)

	os.rmdir_all(test_path3)?
	os.mkdir_all(test_path3)?
	szip.extract_zip_to_dir(test_dir_zip, test_path3)?
	assert os.exists(test_path3_1)
	assert os.exists(test_path3_2)
	assert os.exists(test_path3_3) // This is the empty dir
	assert os.exists(test_path3_4)
	assert (os.read_file(fpath4)?) == '4'
	assert (os.read_file(fpath5)?) == '5'
	assert (os.read_file(fpath6)?) == '6'
}

fn test_zip_folder_omit_empty_directories() ? {
	cleanup()
	os.mkdir_all(test_path3_1)?
	os.mkdir_all(test_path3_2)?
	os.mkdir_all(test_path3_3)?
	os.mkdir_all(test_path3_4)?
	os.write_file(fpath4, '4')?
	os.write_file(fpath5, '5')?
	os.write_file(fpath6, '6')?

	szip.zip_folder(test_path3, test_dir_zip, omit_empty_folders: true)?
	assert os.exists(test_dir_zip)

	os.rmdir_all(test_path3)?
	os.mkdir_all(test_path3)?
	szip.extract_zip_to_dir(test_dir_zip, test_path3)?
	assert os.exists(test_path3_1)
	assert os.exists(test_path3_2)
	assert !os.exists(test_path3_3) // This is the empty dir, should be omitted with `omit_empty_folders`
	assert os.exists(test_path3_4)
	assert (os.read_file(fpath4)?) == '4'
	assert (os.read_file(fpath5)?) == '5'
	assert (os.read_file(fpath6)?) == '6'
}
