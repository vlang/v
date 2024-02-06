import os
import rand
import time

fn test_stat() {
	start_time := time.utc()

	temp_dir := os.join_path(os.temp_dir(), rand.ulid())
	os.mkdir(temp_dir)!
	defer {
		os.rmdir(temp_dir) or {}
	}

	test_file := os.join_path(temp_dir, rand.ulid())
	test_content := rand.ulid()
	os.write_file(test_file, test_content)!
	defer {
		os.rm(test_file) or {}
	}

	end_time := time.utc()

	mut fstat := os.stat(test_file)!
	assert fstat.get_filetype() == .regular
	assert fstat.size == u64(test_content.len)
	assert fstat.get_ctime().unix >= start_time.unix
	assert fstat.get_ctime().unix <= end_time.unix
	assert fstat.get_mtime().unix >= start_time.unix
	assert fstat.get_mtime().unix <= end_time.unix

	$if !windows {
		os.chmod(test_file, 0o600)!
		fstat = os.stat(test_file)!

		mut fmode := fstat.get_mode()
		assert fmode.typ == .regular
		assert fmode.owner.read && fmode.owner.write && !fmode.owner.execute
		assert !fmode.group.read && !fmode.group.write && !fmode.group.execute
		assert !fmode.others.read && !fmode.others.write && !fmode.others.execute

		os.chmod(test_file, 0o421)!
		fstat = os.stat(test_file)!
		fmode = fstat.get_mode()
		assert fmode.owner.read && !fmode.owner.write && !fmode.owner.execute
		assert !fmode.group.read && fmode.group.write && !fmode.group.execute
		assert !fmode.others.read && !fmode.others.write && fmode.others.execute

		os.chmod(test_file, 0o600)!
	}

	// When using the Time struct, allow for up to 1 second difference due to nanoseconds
	// which are not captured in the timestamp
	dstat := os.stat(temp_dir)!
	assert dstat.get_filetype() == .directory
	assert (dstat.get_ctime() - start_time) > -time.second
	assert (dstat.get_ctime() - end_time) <= time.second
	assert (dstat.get_mtime() - start_time) > -time.second
	assert (dstat.get_mtime() - end_time) <= time.second

	assert fstat.dev == dstat.dev, 'File and directory should be created on same device'
	assert fstat.rdev == dstat.rdev, 'File and directory should have same device ID'
}
