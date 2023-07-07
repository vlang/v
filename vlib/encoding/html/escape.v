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
	tag_free_input := input.replace_each(['&', '&amp;', '<', '&lt;', '>', '&gt;', ' ', '&nbsp;'])
	return if config.quote {
		tag_free_input.replace_each(['"', '&quot;', "'", '&apos;'])
	} else {
		tag_free_input
	}
}

// Second Name of `escape()`
fn escape_symbol() {
	escape()
}

pub fn escape_decimal(input string, config EscapeConfig) string {
	tag_free_input := input.replace_each(['&', '&#38;', '<', '&#60;', '>', '&#62;', ' ', '&#160;'])

	return if config.quote {
		tag_free_input.replace_each(["'", '&#39;', '"', '&#34;'])
	} else {
		tag_free_input
	}
}

pub fn escape_hex(input string, config EscapeConfig) string {
	tag_free_input := input.replace_each(['&', '&#x26;', '<', '&#x3C;', '>', '&#x3E;', ' ', '&#xA0;'])

	return if config.quote {
		tag_free_input.replace_each(["'", '&#x27;', '"', '&#x22;'])
	} else {
		tag_free_input
	}
}
