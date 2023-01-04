// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
