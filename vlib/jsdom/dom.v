// DOM API wrapper for JS backend
module jsdom

pub fn get_html_canvas_element(elem IElement) ?HTMLCanvasElement {
	if elem is HTMLCanvasElement {
		return *elem
	} else {
		return none
	}
}
