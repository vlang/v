module html

[params]
pub struct EscapeConfig {
	input string
	quote bool = true
}

// escape converts special characters in the input, specifically "<", ">", and "&"
// to HTML-safe sequences. If `quote` is set to true (which is default), quotes in
// HTML will also be translated. Both double and single quotes will be affected.
pub fn escape(config EscapeConfig) string {
	tag_free_input := config.input.replace_each(['&', '&amp;', '<', '&lt;', '>', '&gt;'])
	return if config.quote {
		tag_free_input.replace_each(['"', '&quot;', "'", '&#x27;'])
	} else {
		tag_free_input
	}
}
