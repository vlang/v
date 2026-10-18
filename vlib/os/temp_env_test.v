module os

const short_tmp = 'C:\\short\\tmp'
const long_tmp = 'C:\\' + 'd'.repeat(windows_temp_env_max_len)

fn fixed_candidates(list []string) fn (string, string) []string {
	return fn [list] (name string, value string) []string {
		return list
	}
}

fn test_short_first_variable_needs_no_change() {
	// GetTempPathW stops at TMP, so a long TEMP behind a short TMP is never read.
	env := {
		'TMP':  short_tmp
		'TEMP': long_tmp
	}
	assert windows_temp_env_change(env, fixed_candidates(['C:\\x'])) == none
}

fn test_boundary_is_max_path() {
	at_limit := 'C:\\' + 'd'.repeat(windows_temp_env_max_len - 3)
	assert at_limit.len == 260
	assert windows_temp_env_change({
		'TMP': at_limit
	}, fixed_candidates(['C:\\x'])) == none
	past_limit := at_limit + 'd'
	change := windows_temp_env_change({
		'TMP': past_limit
	}, fixed_candidates(['C:\\x'])) or {
		assert false, 'a 261 character TMP must be replaced'
		return
	}
	assert change == TempEnvChange{'TMP', 'C:\\x'}
}

fn test_long_tmp_takes_the_first_candidate_that_fits() {
	change := windows_temp_env_change({
		'TMP': long_tmp
	}, fixed_candidates(['', long_tmp, 'C:\\fits'])) or {
		assert false
		return
	}
	assert change == TempEnvChange{'TMP', 'C:\\fits'}
}

fn test_long_temp_is_replaced_when_tmp_is_unset() {
	change := windows_temp_env_change({
		'TEMP':        long_tmp
		'USERPROFILE': long_tmp
	}, fixed_candidates(['C:\\fits'])) or {
		assert false
		return
	}
	assert change == TempEnvChange{'TEMP', 'C:\\fits'}
}

fn test_long_userprofile_is_kept_and_tmp_takes_the_replacement() {
	change := windows_temp_env_change({
		'USERPROFILE': long_tmp
	}, fixed_candidates(['C:\\fits'])) or {
		assert false
		return
	}
	assert change == TempEnvChange{'TMP', 'C:\\fits'}
}

fn test_no_variable_set_needs_no_change() {
	assert windows_temp_env_change(map[string]string{}, fixed_candidates(['C:\\x'])) == none
}

fn test_length_counts_utf16_units() {
	// U+1F600 is one rune, four UTF-8 bytes and two UTF-16 units.
	assert windows_utf16_len('\U0001F600') == 2
	assert windows_utf16_len('é') == 1
	// 129 such runes are 258 units, plus `C:\` is 261: past the limit, although it is
	// only 132 runes.
	value := 'C:\\' + '\U0001F600'.repeat(129)
	assert windows_temp_env_change({
		'TMP': value
	}, fixed_candidates(['C:\\x'])) or { TempEnvChange{} } == TempEnvChange{'TMP', 'C:\\x'}
	// 200 two-byte runes are 403 bytes but only 203 units: within the limit.
	accented := 'C:\\' + 'é'.repeat(200)
	assert accented.len > windows_temp_env_max_len
	assert windows_temp_env_change({
		'TMP': accented
	}, fixed_candidates(['C:\\x'])) == none
}

fn test_entries_fix_replaces_a_case_insensitive_entry() {
	env := ['Path=C:\\bin', 'tmp=${long_tmp}', 'TEMP=${short_tmp}']
	fixed := windows_temp_env_entries_fix(env, fixed_candidates(['C:\\fits']))
	assert fixed == ['Path=C:\\bin', 'TEMP=${short_tmp}', 'TMP=C:\\fits']
	assert windows_temp_env_entries_fix(fixed, fixed_candidates(['C:\\other'])) == fixed
}
