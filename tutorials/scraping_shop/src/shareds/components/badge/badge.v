module badge

import veb

pub enum StateBadge {
	none
	success
	failure
}

const map_state_class_css = {
	StateBadge.none: 'badge-none'
	StateBadge.failure: 'badge-error'
	StateBadge.success: 'badge-success'
}

// list_badge_to_html list of badge data for rendered badge components
// map['status'], map['icon'], map['label']
pub fn list_badge_to_html(badges []map[string]string) []veb.RawHtml {
	mut result := []veb.RawHtml{}

	for current_badge in badges {
		state := StateBadge.from(current_badge['status']) or { StateBadge.none }
		result << construct(state, current_badge['icon'], current_badge['label'])
	}

	return result
}

pub fn construct(state StateBadge, icon string, content string) veb.RawHtml {
	state_badge := map_state_class_css[state]

	return $tmpl('./badge.html')
}
