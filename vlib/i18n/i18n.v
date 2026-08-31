// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module i18n

import os

pub const default_translations_dir = 'translations'

const default_tr_map = load_tr_map()

// load_tr_map loads all .tr files from the default translations directory.
pub fn load_tr_map() map[string]map[string]string {
	return load_tr_map_from_dir(default_translations_dir)
}

// load_tr_map_from_dir loads all .tr files from dir into a lang -> key -> text map.
pub fn load_tr_map_from_dir(dir string) map[string]map[string]string {
	files := os.walk_ext(dir, '.tr')
	mut res := map[string]map[string]string{}
	for tr_path in files {
		lang := fetch_lang_from_tr_path(tr_path)
		if lang.len == 0 {
			continue
		}
		text := os.read_file(tr_path) or {
			eprintln('translation file "${tr_path}" failed to load')
			return {}
		}
		for key, val in parse_tr_text(text) {
			res[lang][key] = val
		}
	}
	return res
}

fn parse_tr_text(text string) map[string]string {
	mut res := map[string]string{}
	normalized := text.replace('\r\n', '\n')
	for section in normalized.split('-----\n') {
		nl_pos := section.index('\n') or { continue }
		key := section[..nl_pos].trim_space()
		if key.len == 0 {
			continue
		}
		res[key] = section[nl_pos + 1..].trim_right('\n')
	}
	return res
}

fn fetch_lang_from_tr_path(path string) string {
	return os.file_name(path).all_before_last('.tr')
}

// tr returns the translation for key from the default translations directory.
pub fn tr(lang string, key string) string {
	return tr_from_map(default_tr_map, lang, key)
}

// tr_from_map returns the translation for key from translations.
pub fn tr_from_map(translations map[string]map[string]string, lang string, key string) string {
	res := translations[lang][key]
	if res == '' {
		eprintln('NO TRANSLATION FOR KEY "${key}"')
		return key
	}
	return res
}

// tr_plural returns the pluralized translation for key from the default translations directory.
pub fn tr_plural(lang string, key string, amount int) string {
	return tr_plural_from_map(default_tr_map, lang, key, amount)
}

// tr_plural_from_map returns the pluralized translation for key from translations.
pub fn tr_plural_from_map(translations map[string]map[string]string, lang string, key string, amount int) string {
	s := translations[lang][key]
	if s == '' {
		eprintln('NO TRANSLATION FOR KEY "${key}"')
		return key
	}
	if !s.contains('|') {
		return s
	}
	// goods
	// товар|а|ов
	vals := s.split('|')
	if vals.len != 3 {
		return s
	}
	amount_str := amount.str()
	ending := if amount % 10 == 1 && !amount_str.ends_with('11') {
		''
	} else if amount % 10 == 2 && !amount_str.ends_with('12') {
		vals[1]
	} else if amount % 10 == 3 && !amount_str.ends_with('13') {
		vals[1]
	} else if amount % 10 == 4 && !amount_str.ends_with('14') {
		vals[1]
	} else {
		vals[2]
	}
	return vals[0] + ending
}
