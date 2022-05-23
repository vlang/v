// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module buffer

pub struct PRNGBuffer {
mut:
	bytes_left int
	buffer     u64
}
