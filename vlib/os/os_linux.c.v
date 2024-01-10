// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

const prot_read = 1
const prot_write = 2
const map_private = 0x02
const map_anonymous = 0x20

pub const sys_write = 1
pub const sys_open = 2
pub const sys_close = 3
pub const sys_mkdir = 83
pub const sys_creat = 85
