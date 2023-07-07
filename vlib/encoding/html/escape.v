module html

[params]
pub struct Config {
	decimal bool
	hex bool
}

pub fn escape(input string, config Config) string {
	// For adding more HTML Encode characters just add them to the lists below

	// Symbolic HTML Encode
	mut symbol := ['&', '&amp;', '<', '&lt;', '>', '&gt;', "'", '&apos;', '"', "&quot;" ,' ', '&nbsp;']
	// Decimal HTML Encode
	mut decimal := ['&', '&#38;', '<', '&#60;', '>', '&#62;', "'", '&#39;', '"', "&#34;" ,' ', '&#160;']
	// Hexadecimal HTML Encode
	mut hex := ['&', '&#x26;', '<', '&#x3C;', '>', '&#x3E;', "'", '&#x27;', '"', "&#x22;" ,' ', '&#xA0;']

	return if config.decimal {
		input.replace_each(decimal)

	} else if config.hex {
		input.replace_each(hex)

	} else {
		input.replace_each(symbol)
	}
}
