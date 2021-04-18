// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

#include "@VROOT/vlib/os/os_darwin.m"

pub const (
	sys_write         = 4
	sys_open          = 5
	sys_close         = 6
	sys_mkdir         = 136
	sys_creat         = 8
	sys_open_nocancel = 398
	sys_stat64        = 338
)

fn C.darwin_log(s string)
