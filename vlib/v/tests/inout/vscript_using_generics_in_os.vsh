chdir(temp_dir())?
test_file_name := 'test_file'
write_file_array(test_file_name, [u8(0xff), 0xff, 0xff, 0xff])?
defer {
	rm(test_file_name) or {}
}
println(read_file_array<u8>(test_file_name))
