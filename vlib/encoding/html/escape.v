module html

[params]
pub struct EscapeConfig {
	quote bool = true
}

// escape converts special characters in the input, specifically "<", ">", and "&"
// to HTML-safe sequences. If `quote` is set to true (which is default), quotes in
// HTML will also be translated. Both double and single quotes will be affected.
// **Note:** escape() supports funky accents by doing nothing about them. V's UTF-8
// support through `string` is robust enough to deal with these cases.
pub fn escape(input string, config EscapeConfig) string {
	tag_free_input := input.replace_each(['&', '&amp;', '<', '&lt;', '>', '&gt;'])
	return if config.quote {
		tag_free_input.replace_each(['"', '&#34;', "'", '&#39;'])
	} else {
		tag_free_input
	}
}
