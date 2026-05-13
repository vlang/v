module dtm

import strings

fn filter(s string) string {
	mut escaped := strings.new_builder(s.len)
	for i := 0; i < s.len; i++ {
		match s[i] {
			`&` {
				escaped.write_string('&amp;')
			}
			`<` {
				escaped.write_string('&lt;')
			}
			`>` {
				escaped.write_string('&gt;')
			}
			`"` {
				escaped.write_string('&#34;')
			}
			`'` {
				escaped.write_string('&#39;')
			}
			else {
				escaped.write_u8(s[i])
			}
		}
	}
	return escaped.str()
}
