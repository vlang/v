// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module input

// Config is used to configure input to the toml module.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	text           string // TOML text
	file_path      string // '/path/to/file.toml'
}

pub fn (c Config) validate() {
	if c.file_path != '' && c.text != '' {
		panic(@MOD + '.' + @FN +
			' ${typeof(c).name} should contain only one of the fields `file_path` OR `text` filled out')
	} else if c.file_path == '' && c.text == '' {
		panic(@MOD + '.' + @FN +
			' ${typeof(c).name} must either contain a valid `file_path` OR a non-empty `text` field')
	}
}
