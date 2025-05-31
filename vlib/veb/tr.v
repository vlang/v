// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import os

const tr_map = load_tr_map()

pub fn raw(s string) RawHtml {
	return RawHtml(s)
}

/*
struct TrData {
	data
}
	m map[string]TrData
	*/

// This function is run once, on app startup. Setting the `tr_map` const.
// m['en']['house'] == 'House'
fn load_tr_map() map[string]map[string]string {
	// Find all translation files to figure out how many languages we have and to load the translation map
	files := os.walk_ext('translations', '.tr')
	mut res := map[string]map[string]string{}
	for tr_path in files {
		lang := fetch_lang_from_tr_path(tr_path)
		text := os.read_file(tr_path) or {
			eprintln('translation file "${tr_path}" failed to laod')
			return {}
		}
		x := text.split('-----\n')
		for s in x {
			// println('val="${val}"')
			nl_pos := s.index('\n') or { continue }
			key := s[..nl_pos]
			val := s[nl_pos + 1..]
			// v := vals[i + 1]
			// println('key="${key}" => val="${v}"')
			res[lang][key] = val
			// println(val)
		}
	}
	return res
}

fn fetch_lang_from_tr_path(path string) string {
	return path.find_between(os.path_separator, '.')
}

// Used by %key in templates
pub fn tr(lang string, key string) string {
	res := tr_map[lang][key]
	if res == '' {
		eprintln('NO TRANSLATION FOR KEY "${key}"')
		return key
	}
	return RawHtml(res)
}

pub fn tr_plural(lang string, key string, amount int) string {
	s := tr_map[lang][key]
	if s == '' {
		eprintln('NO TRANSLATION FOR KEY "${key}"')
		return key
	}
	if s.contains('|') {
		//-----
		// goods
		// товар|а|ов
		vals := s.split('|')
		if vals.len != 3 {
			return s
		}
		amount_str := amount.str()
		// 1, 21, 121 товар
		ending := if amount % 10 == 1 && !amount_str.ends_with('11') { // vals[0]
			''
			// 2, 3, 4, 22 товара
		} else if amount % 10 == 2 && !amount_str.ends_with('12') {
			vals[1]
		} else if amount % 10 == 3 && !amount_str.ends_with('13') {
			vals[1]
		} else if amount % 10 == 4 && !amount_str.ends_with('14') {
			vals[1]
		} else {
			// 5 товаров, 11 товаров etc
			vals[2]
		}
		return vals[0] + ending
	} else {
		return s
	}
}
