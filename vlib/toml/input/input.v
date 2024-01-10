// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module input

import os

// Config is used to configure input to the toml module.
// Only one of the fields `text` and `file_path` is allowed to be set at time of configuration.
pub struct Config {
pub:
	text      string // TOML text
	file_path string // '/path/to/file.toml'
}

// validate returns an optional error if more than one of the fields
// in `Config` has a non-default value (empty string).
fn (c Config) validate() ! {
	if c.file_path != '' && c.text != '' {
		error(@MOD + '.' + @FN +
			' ${typeof(c).name} should contain only one of the fields `file_path` OR `text` filled out')
	} else if c.file_path == '' && c.text == '' {
		error(@MOD + '.' + @FN +
			' ${typeof(c).name} must either contain a valid `file_path` OR a non-empty `text` field')
	}
}

// read_input returns either Config.text or the read file contents of Config.file_path
// depending on which one is not empty.
pub fn (c Config) read_input() !string {
	c.validate()!
	mut text := c.text
	if text == '' && os.is_file(c.file_path) {
		text = os.read_file(c.file_path) or {
			return error(@MOD + '.' + @STRUCT + '.' + @FN +
				' Could not read "${c.file_path}": "${err.msg()}"')
		}
	}
	return text
}
