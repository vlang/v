module dtm

import encoding.html

fn filter(s string) string {
	return html.escape(s)
}
