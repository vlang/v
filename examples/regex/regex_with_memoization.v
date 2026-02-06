import os

fn regex_match(src string, pat string) bool {
	src_size := src.len + 1
	pat_size := pat.len + 1
	mut memo := [][]int{len: src_size, init: []int{len: pat_size, init: -1}}
	return regex_match_core(src, pat, 0, 0, mut memo)
}

fn regex_match_core(src string, pat string, src_pos int, pat_pos int, mut memo [][]int) bool {
	if memo[src_pos][pat_pos] != -1 {
		return memo[src_pos][pat_pos] == 1
	}
	mut spos := src_pos
	mut ppos := pat_pos
	if spos >= src.len && ppos >= pat.len {
		memo[src_pos][pat_pos] = 1
		return true
	} else if spos < src.len && ppos >= pat.len {
		memo[src_pos][pat_pos] = 0
		return false
	} else if spos >= src.len && ppos < pat.len {
		if pat[ppos] == `\\` {
			ppos++
		}
		res := ppos + 1 < pat.len && pat[ppos + 1] in [`*`, `?`]
			&& regex_match_core(src, pat, spos, ppos + 2, mut memo)
		memo[src_pos][pat_pos] = if res { 1 } else { 0 }
		return res
	} else {
		first_is_bslash := pat[ppos] == `\\`
		if first_is_bslash {
			ppos++
		}
		first_bslash_and_match := first_is_bslash && ppos < pat.len
			&& (((pat[ppos] == `d` && src[spos].is_digit())
			|| (pat[ppos] == `D` && !src[spos].is_digit())
			|| (pat[ppos] == `s` && src[spos].is_space())
			|| (pat[ppos] == `S` && !src[spos].is_space())
			|| (pat[ppos] == `w` && (src[spos].is_digit() || src[spos].is_letter()
			|| src[spos] == `_`)) || (pat[ppos] == `W` && !(src[spos].is_digit()
			|| src[spos].is_letter() || src[spos] == `_`)))
			|| (pat[ppos] in [`d`, `D`, `s`, `S`, `w`, `W`] && ppos + 1 < pat.len
			&& pat[ppos + 1] in [`*`, `?`, `+`])
			|| (pat[ppos] !in [`d`, `D`, `s`, `S`, `w`, `W`] && src[spos] == pat[ppos]))
		if ppos + 1 < pat.len {
			match pat[ppos + 1] {
				`*` {
					if first_bslash_and_match {
						res := regex_match_core(src, pat, spos + 1, ppos - 1, mut memo)
							|| regex_match_core(src, pat, spos, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					} else if src[spos] == pat[ppos] || pat[ppos] == `.` {
						res := regex_match_core(src, pat, spos + 1, ppos, mut memo)
							|| regex_match_core(src, pat, spos, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					} else {
						res := regex_match_core(src, pat, spos, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					}
				}
				`+` {
					if first_bslash_and_match {
						res := regex_match_core(src, pat, spos + 1, ppos - 1, mut memo)
							|| regex_match_core(src, pat, spos + 1, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					} else if src[spos] == pat[ppos] || pat[ppos] == `.` {
						res := regex_match_core(src, pat, spos + 1, ppos, mut memo)
							|| regex_match_core(src, pat, spos + 1, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					} else {
						memo[src_pos][pat_pos] = 0
						return false
					}
				}
				`?` {
					if first_bslash_and_match || src[spos] == pat[ppos] || pat[ppos] == `.` {
						res := regex_match_core(src, pat, spos + 1, ppos + 2, mut memo)
							|| regex_match_core(src, pat, spos, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					} else {
						res := regex_match_core(src, pat, spos, ppos + 2, mut memo)
						memo[src_pos][pat_pos] = if res { 1 } else { 0 }
						return res
					}
				}
				else {}
			}
		}
		if first_is_bslash {
			res := first_bslash_and_match
				&& regex_match_core(src, pat, spos + 1, ppos + 1, mut memo)
			memo[src_pos][pat_pos] = if res { 1 } else { 0 }
			return res
		} else {
			res := (src[spos] == pat[ppos] || pat[ppos] == `.`) && pat[ppos] != `\\`
				&& regex_match_core(src, pat, spos + 1, ppos + 1, mut memo)
			memo[src_pos][pat_pos] = if res { 1 } else { 0 }
			return res
		}
	}
}

fn main() {
	mut cnt := 0
	println('currently supported patterns: . ? + * \\ \\d \\D \\s \\S \\w \\W')
	println('example: source `address@domain.net` matches pattern `\\w+@domain\\.net`')
	println('enter `exit` to quit\n')
	for {
		cnt++
		src := os.input('[${cnt}] enter source string: ')
		if src == 'exit' {
			break
		}
		pat := os.input('[${cnt}] enter pattern string: ')
		if pat == 'exit' {
			break
		}
		println('[${cnt}] whether `${src}` matches `${pat}`: ${regex_match(src, pat)}')
	}
}
