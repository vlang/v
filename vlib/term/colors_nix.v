// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module term

pub fn format(msg, open, close string) string {
	return _format(msg, open, close)
}
