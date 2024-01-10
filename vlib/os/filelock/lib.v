module filelock

pub struct FileLock {
	name string
mut:
	fd int
}

pub fn new(fileName string) FileLock {
	return FileLock{
		name: fileName
		fd: -1
	}
}
