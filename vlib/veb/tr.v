// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import i18n

const tr_map = i18n.load_tr_map()

pub fn raw(s string) RawHtml {
	return RawHtml(s)
}

// This function is run once, on app startup. Setting the `tr_map` const.
// m['en']['house'] == 'House'
fn load_tr_map() map[string]map[string]string {
	return i18n.load_tr_map()
}

// Used by %key in templates
pub fn tr(lang string, key string) string {
	return RawHtml(i18n.tr_from_map(tr_map, lang, key))
}

pub fn tr_plural(lang string, key string, amount int) string {
	return i18n.tr_plural_from_map(tr_map, lang, key, amount)
}
