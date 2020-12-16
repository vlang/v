// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

const (
	prot_read     = 1
	prot_write    = 2
	map_private   = 0x02
	map_anonymous = 0x20
)

pub const (
	sys_write = 1
	sys_open  = 2
	sys_close = 3
	sys_mkdir = 83
	sys_creat = 85
)

/*
// TODO no pub => error
pub fn write(fd int, data voidptr, nbytes int) int {
	return syscall5(
          1, // SYS_write
          fd,
          data,
          nbytes,
          0, // ignored
          0  // ignored
	)
}

pub fn println(s string) {
	write(1, (s + '\n').str, s.len)
}

fn mmap(start voidptr, len, prot, flags, fd, off int) byteptr {
	return syscall6(9, start, len, prot, flags, fd, off) // sys_mmap
}

pub fn malloc(n int) byteptr {
	println('malloc($n)')
	return mmap(0, n, 3, 		4098, //prot_read|prot_write,
	 -1,0)			//map_private|map_anonymous,
*/
