module fastc

import v.flat

// FastcGlobalSuffixIndex preserves the first suffix match in the fixed program
// tables. Declaration initializers still extend those tables and use the scan.
struct FastcGlobalSuffixIndex {
	ready     bool
	keys      map[string]string
	type_keys map[string]string
}

fn fastc_global_suffix_index(globals map[string]string, types flat.TypeTextBindings) FastcGlobalSuffixIndex {
	return FastcGlobalSuffixIndex{
		ready:     true
		keys:      fastc_first_suffix_keys(globals.keys())
		type_keys: fastc_first_suffix_keys(types.keys())
	}
}

// fastc_first_suffix_keys indexes exact keys and every dot-delimited suffix in
// insertion order, matching the existing first-match scan even for dotted names.
fn fastc_first_suffix_keys(keys []string) map[string]string {
	mut result := map[string]string{}
	for key in keys {
		if key !in result { result[key] = key }
		for i, c in key {
			if c == `.` {
				suffix := key[i + 1..]
				if suffix !in result { result[suffix] = key }
			}
		}
	}
	return result
}
