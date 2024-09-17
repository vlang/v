struct File {
	name   string
	typ    string
	is_dir bool
}

fn filter(all_files []File, interested_file File) []File {
	println(interested_file)
	return all_files.filter(fn [interested_file] (f File) bool {
		println(interested_file)
		return true
	})
}

fn test_array_filter_using_direct_closure() {
	filenames := ['one', 'two', 'three']
	files := filenames.map(fn (f string) File {
		return File{
			name:   f
			typ:    if f == 'one' {
				'file'
			} else {
				'dir'
			}
			is_dir: if f == 'one' {
				false
			} else {
				true
			}
		}
	})

	ret := filter(files, files[0])
	println(ret)
	assert ret.len == 3
	assert ret[0].name == 'one'
	assert ret[0].typ == 'file'
	assert ret[1].name == 'two'
	assert ret[1].typ == 'dir'
	assert ret[2].name == 'three'
	assert ret[2].typ == 'dir'
}
