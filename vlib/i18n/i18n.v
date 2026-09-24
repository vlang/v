// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module i18n

import json2
import os
import v.embed_file

pub const default_translations_dir = 'translations'

const default_tr_map = load_tr_map()

// load_tr_map loads all .tr files from the default translations directory.
pub fn load_tr_map() map[string]map[string]string {
	return load_tr_map_from_dir(default_translations_dir)
}

// load_tr_map_from_dir loads all .tr and .json files from dir into a
// lang -> key -> text map. A `.tr` entry wins when the same key is defined by both
// formats, since `.tr` is the primary format.
pub fn load_tr_map_from_dir(dir string) map[string]map[string]string {
	mut res := map[string]map[string]string{}
	for json_path in os.walk_ext(dir, '.json') {
		lang, prefix := fetch_lang_and_prefix_from_json_path(dir, json_path)
		if lang.len == 0 {
			continue
		}
		text := os.read_file(json_path) or {
			eprintln('translation file "${json_path}" failed to load')
			continue
		}
		for key, val in parse_tr_json(text, prefix, json_path) {
			res[lang][key] = val
		}
	}
	files := os.walk_ext(dir, '.tr')
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

// load_tr_map_from_files builds translations from file contents instead of reading
// them from disk. files maps each file's path inside the translations directory
// (`en.tr`, `en.json`, `zh/dashboard.json`) to its text, and is read exactly as
// load_tr_map_from_dir reads the same files from a directory. A path may use either
// `/` or `\` between its parts, so one built with `os.join_path` works everywhere.
pub fn load_tr_map_from_files(files map[string]string) map[string]map[string]string {
	mut res := map[string]map[string]string{}
	mut paths := files.keys()
	paths.sort()
	// JSON first, so that a `.tr` entry for the same key replaces it.
	for path in paths {
		if !path.ends_with('.json') {
			continue
		}
		lang, prefix := fetch_lang_and_prefix_from_relative_json_path(slash_path(path))
		if lang.len == 0 {
			continue
		}
		for key, val in parse_tr_json(files[path], prefix, path) {
			res[lang][key] = val
		}
	}
	for path in paths {
		if !path.ends_with('.tr') {
			continue
		}
		lang := slash_path(path).all_after_last('/').all_before_last('.tr')
		if lang.len == 0 {
			continue
		}
		for key, val in parse_tr_text(files[path]) {
			res[lang][key] = val
		}
	}
	return res
}

// load_tr_map_from_embedded builds translations from files embedded in the program
// with `$embed_file`, so it does not depend on a translations directory being
// installed next to the executable:
//
// ```v ignore
// const translations = i18n.load_tr_map_from_embedded('translations', [
// 	$embed_file('translations/en.tr'),
// 	$embed_file('translations/zh/dashboard.json'),
// ])
// ```
//
// dir is the translations directory, spelled the way the embedded paths spell it.
// A file's path inside it decides its language and key prefix, as it does for
// load_tr_map_from_dir. A file outside dir is read as if it were directly in it.
//
// Only a `-prod` build (or `-os cross` C output) puts the files' contents into the
// executable. A development build keeps just their paths, to keep rebuilds cheap,
// and reads the files from those paths when the translations are loaded. It works
// where it was built, but panics where the files are missing, so build anything you
// ship to other machines with `-prod`.
pub fn load_tr_map_from_embedded(dir string, files []embed_file.EmbedFileData) map[string]map[string]string {
	mut texts := map[string]string{}
	for file in files {
		texts[embedded_relative_path(dir, file.path)] = file.to_string()
	}
	return load_tr_map_from_files(texts)
}

// embedded_relative_path is where an embedded file sits inside the translations
// directory, with `/` separators whatever the platform.
fn embedded_relative_path(dir string, path string) string {
	clean_path := slash_path(os.norm_path(path))
	clean_dir := slash_path(os.norm_path(dir)).trim_right('/')
	if clean_dir in ['', '.'] {
		return clean_path
	}
	if clean_path.starts_with(clean_dir + '/') {
		return clean_path[clean_dir.len + 1..]
	}
	return os.file_name(clean_path)
}

// slash_path spells a path with `/` between its parts. Paths inside a translations
// directory come from callers on every platform, and `os.join_path` separates them
// with `\` on Windows.
fn slash_path(path string) string {
	return path.replace('\\', '/')
}

// fetch_lang_and_prefix_from_relative_json_path is fetch_lang_and_prefix_from_json_path
// for a path already relative to the translations directory, separated with `/`.
fn fetch_lang_and_prefix_from_relative_json_path(path string) (string, string) {
	parts := path.split('/')
	name := parts.last().all_before_last('.json')
	if parts.len == 1 {
		return name, ''
	}
	return parts[parts.len - 2], name
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

// fetch_lang_and_prefix_from_json_path maps a translation file path to its language
// and to the prefix its keys get. A file directly in `dir` is named after its
// language, like a `.tr` file (`translations/en.json` -> `en`). A file in a
// subdirectory uses that directory as the language and its own name as a key prefix,
// so translations can be split per feature (`translations/zh/dashboard.json` -> `zh`,
// with keys under `dashboard.`).
fn fetch_lang_and_prefix_from_json_path(dir string, path string) (string, string) {
	name := os.file_name(path).all_before_last('.json')
	parent := os.file_name(os.dir(path))
	if os.norm_path(os.dir(path)) == os.norm_path(dir) || parent.len == 0 {
		return name, ''
	}
	return parent, name
}

// parse_tr_json flattens a JSON object into `key -> text` pairs. Nested objects are
// joined with `.`, so `{"menu": {"file": "File"}}` defines `menu.file`.
fn parse_tr_json(text string, prefix string, path string) map[string]string {
	mut res := map[string]string{}
	decoded := json2.decode[json2.Any](text) or {
		eprintln('translation file "${path}" is not valid JSON: ${err}')
		return res
	}
	flatten_tr_json(decoded, prefix, mut res)
	return res
}

fn flatten_tr_json(value json2.Any, key string, mut res map[string]string) {
	if value is map[string]json2.Any {
		for name, child in value {
			child_key := if key.len == 0 { name } else { '${key}.${name}' }
			flatten_tr_json(child, child_key, mut res)
		}
		return
	}
	if value is []json2.Any {
		// an array cannot name a translation; ignore it rather than inventing indices
		return
	}
	if key.len != 0 {
		res[key] = if value is json2.Null { 'null' } else { value.str() }
	}
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
