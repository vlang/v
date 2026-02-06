// Copyright (c) 2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os

// resolve_env_value replaces all occurrences of `$env('ENV_VAR_NAME')`
// in `str` with the value of the env variable `$ENV_VAR_NAME`.
pub fn resolve_env_value(str string, check_for_presence bool) !string {
	env_ident := "\$env('"
	at := str.index(env_ident) or {
		return error('no "${env_ident}' + '...\')" could be found in "${str}".')
	}
	mut ch := u8(`.`)
	mut benv_lit := []u8{cap: 20}
	for i := at + env_ident.len; i < str.len && ch != `)`; i++ {
		ch = u8(str[i])
		if ch.is_letter() || ch.is_digit() || ch == `_` {
			benv_lit << ch
		} else {
			if !(ch == `'` || ch == `)`) {
				if ch == `$` {
					return error('cannot use string interpolation in compile time \$env() expression')
				}
				return error('invalid environment variable name in "${str}", invalid character "${rune(ch)}"')
			}
		}
	}
	env_lit := benv_lit.bytestr()
	if env_lit == '' {
		return error('supply an env variable name like HOME, PATH or USER')
	}
	mut env_value := ''
	if check_for_presence {
		env_value = os.environ()[env_lit] or {
			return error('the environment variable "${env_lit}" does not exist.')
		}
		if env_value == '' {
			return error('the environment variable "${env_lit}" is empty.')
		}
	} else {
		env_value = os.getenv(env_lit)
	}
	rep := str.replace_once(env_ident + env_lit + "'" + ')', env_value)
	if rep.contains(env_ident) {
		return resolve_env_value(rep, check_for_presence)
	}
	return rep
}
