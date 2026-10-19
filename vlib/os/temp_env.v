module os

// windows_temp_env_max_len is the longest TMP, TEMP or USERPROFILE value that GetTempPathW
// reads safely: its first buffer holds MAX_PATH+1 characters with the terminator. In a
// longPathAware process, a longer value makes it retry forever with a buffer that is still
// too small, and CreateProcessW calls it for every program that it starts.
const windows_temp_env_max_len = 260

// windows_temp_env_names are the variables that GetTempPathW reads, in its order.
const windows_temp_env_names = ['TMP', 'TEMP', 'USERPROFILE']

struct TempEnvChange {
	name  string
	value string
}

// windows_utf16_len returns the length of `s` in UTF-16 code units, the unit of Windows paths.
fn windows_utf16_len(s string) int {
	mut n := 0
	for r in s.runes() {
		n += if u32(r) > 0xFFFF { 2 } else { 1 }
	}
	return n
}

// windows_temp_env_change returns the variable to set so that GetTempPathW never reads a
// value longer than windows_temp_env_max_len, or none when it already does not. GetTempPathW
// stops at the first of TMP, TEMP and USERPROFILE that is set, so only that one matters.
// `env` holds those of the three that are set, by their upper case names, and `candidates`
// lists replacements for a too long value of the named variable, best first. A too long TMP
// or TEMP is replaced in place; USERPROFILE is kept, and TMP, which GetTempPathW reads first,
// takes the replacement instead.
fn windows_temp_env_change(env map[string]string, candidates fn (string, string) []string) ?TempEnvChange {
	for name in windows_temp_env_names {
		value := env[name] or { continue }
		if windows_utf16_len(value) <= windows_temp_env_max_len {
			return none
		}
		for candidate in candidates(name, value) {
			if candidate != '' && windows_utf16_len(candidate) <= windows_temp_env_max_len {
				return TempEnvChange{
					name:  if name == 'USERPROFILE' { 'TMP' } else { name }
					value: candidate
				}
			}
		}
		return none
	}
	return none
}

// windows_temp_env_entries_fix applies windows_temp_env_change to an environment given as
// `NAME=value` entries, as passed to a child process. Windows names are case-insensitive.
fn windows_temp_env_entries_fix(entries []string, candidates fn (string, string) []string) []string {
	mut env := map[string]string{}
	for entry in entries {
		name := entry.all_before('=').to_upper()
		if name in windows_temp_env_names && name !in env {
			env[name] = entry.all_after_first('=')
		}
	}
	change := windows_temp_env_change(env, candidates) or { return entries }
	mut fixed := entries.filter(it.all_before('=').to_upper() != change.name)
	fixed << '${change.name}=${change.value}'
	return fixed
}
