module switch_lang

import veb

pub fn construct(abbrev_lang string) veb.RawHtml {
	return $tmpl('./switch_lang.html')
}
