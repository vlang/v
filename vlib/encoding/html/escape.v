module html

import encoding.hex
import strconv

[params]
pub struct EscapeConfig {
	quote bool = true
}

const (
	html_replacement_seq       = ['&', '&amp;', '<', '&lt;', '>', '&gt;']
	html_quote_replacement_seq = ['"', '&#34;', "'", '&#39;'] // `'&#34;'` is shorter than `'&quot;'`
	html_replacement_table     = {
		'AElig':    `Æ`
		'Aacute':   `Á`
		'aacute':   `á`
		'Acirc':    `Â`
		'Agrave':   `À`
		'Alpha':    `Α`
		'amp':      `&`
		'and':      `∧`
		'ang':      `∠`
		'apos':     `'`
		'aring':    `å`
		'asymp':    `≈`
		'Atilde':   `Ã`
		'Auml':     `Ä`
		'Beta':     `Β`
		'bdquo':    `„`
		'brvbar':   `¦`
		'bull':     `•`
		'Ccedil':   `Ç`
		'cedil':    `¸`
		'cent':     `¢`
		'Chi':      `Χ`
		'circ':     `ˆ`
		'clubs':    `♣`
		'cong':     `≅`
		'copy':     `©`
		'crarr':    `↵`
		'cup':      `∪`
		'curren':   `¤`
		'dagger':   `†`
		'Dagger':   `‡`
		'deg':      `°`
		'Delta':    `Δ`
		'diams':    `♦`
		'divide':   `÷`
		'dollar':   `$`
		'Eacute':   `É`
		'Ecirc':    `Ê`
		'Egrave':   `È`
		'ensp':     ` `
		'emsp':     ` `
		'Epsilon':  `Ε`
		'Euml':     `Ë`
		'euro':     `€`
		'exist':    `∃`
		'fnof':     `ƒ`
		'forall':   `∀`
		'frac12':   `½`
		'frac14':   `¼`
		'frac34':   `¾`
		'frasl':    `⁄`
		'Gamma':    `Γ`
		'ge':       `≥`
		'gt':       `>`
		'hArr':     `⇔`
		'harr':     `↔`
		'hearts':   `♥`
		'hellip':   `…`
		'iacute':   `í`
		'Iacute':   `Í`
		'icirc':    `î`
		'Icirc':    `Î`
		'iexcl':    `¡`
		'Igrave':   `Ì`
		'image':    `ℑ`
		'infin':    `∞`
		'int':      `∫`
		'iota':     `ι`
		'Iota':     `Ι`
		'iquest':   `¿`
		'isin':     `∈`
		'Iuml':     `Ï`
		'Kappa':    `Κ`
		'lArr':     `⇐`
		'Lambda':   `Λ`
		'lang':     `〈`
		'laquo':    `«`
		'lceil':    `⌈`
		'ldquo':    `“`
		'lsaquo':   `‹`
		'le':       `≤`
		'lfloor':   `⌊`
		'lowast':   `∗`
		'loz':      `◊`
		'lrm':      `‎`
		'lsquo':    `‘`
		'lt':       `<`
		'macr':     `¯`
		'mdash':    `—`
		'micro':    `µ`
		'middot':   `·`
		'Mu':       `Μ`
		'nabla':    `∇`
		'nbsp':     ` `
		'ndash':    `–`
		'ne':       `≠`
		'Ntilde':   `Ñ`
		'ntilde':   `ñ`
		'Nu':       `Ν`
		'Oacute':   `Ó`
		'ocirc':    `ô`
		'Ocirc':    `Ô`
		'OElig':    `Œ`
		'oelig':    `œ`
		'Omega':    `Ω`
		'omega':    `ω`
		'omicron':  `ο`
		'Omicron':  `Ο`
		'oplus':    `⊕`
		'or':       `∨`
		'ordf':     `ª`
		'ordm':     `º`
		'Oslash':   `Ø`
		'oslash':   `ø`
		'Otilde':   `Õ`
		'otilde':   `õ`
		'otimes':   `⊗`
		'Ouml':     `Ö`
		'para':     `¶`
		'part':     `∂`
		'permil':   `‰`
		'perp':     `⊥`
		'Phi':      `Φ`
		'phi':      `φ`
		'Pi':       `Π`
		'pi':       `π`
		'plusmn':   `±`
		'pound':    `£`
		'Prime':    `″`
		'prime':    `′`
		'prop':     `∝`
		'Psi':      `Ψ`
		'psi':      `ψ`
		'quot':     `"`
		'radic':    `√`
		'rang':     `〉`
		'raquo':    `»`
		'rdquo':    `”`
		'real':     `ℜ`
		'reg':      `®`
		'rfloor':   `⌋`
		'Rho':      `Ρ`
		'rho':      `ρ`
		'rlm':      `‏`
		'rsaquo':   `›`
		'rsquo':    `’`
		'sbquo':    `‚`
		'Scaron':   `Š`
		'scaron':   `š`
		'sect':     `§`
		'shy':      `­`
		'Sigma':    `Σ`
		'sigma':    `σ`
		'sigmaf':   `ς`
		'sim':      `∼`
		'spades':   `♠`
		'sub':      `⊂`
		'sube':     `⊆`
		'sum':      `∑`
		'sup':      `⊃`
		'sup1':     `¹`
		'sup2':     `²`
		'sup3':     `³`
		'szlig':    `ß`
		'Tau':      `Τ`
		'tau':      `τ`
		'there4':   `∴`
		'Theta':    `Θ`
		'theta':    `θ`
		'thetasym': `ϑ`
		'thinsp':   ` `
		'tilde':    `˜`
		'times':    `×`
		'trade':    `™`
		'Uacute':   `Ú`
		'uacute':   `ú`
		'uArr':     `⇑`
		'uarr':     `↑`
		'uml':      `¨`
		'upsih':    `ϒ`
		'Upsilon':  `Υ`
		'upsilon':  `υ`
		'Uuml':     `Ü`
		'uuml':     `ü`
		'Yacute':   `Ý`
		'yacute':   `ý`
		'yen':      `¥`
		'Yuml':     `Ÿ`
		'yuml':     `ÿ`
		'Zeta':     `Ζ`
		'zwj':      `‍`
		'zwnj':     `‌`
	}
)

// escape converts special characters in the input, specifically "<", ">", and "&"
// to HTML-safe sequences. If `quote` is set to true (which is default), quotes in
// HTML will also be translated. Both double and single quotes will be affected.
// **Note:** escape() supports funky accents by doing nothing about them. V's UTF-8
// support through `string` is robust enough to deal with these cases.
pub fn escape(input string, config EscapeConfig) string {
	tag_free_input := input.replace_each(html.html_replacement_seq)
	return if config.quote {
		tag_free_input.replace_each(html.html_quote_replacement_seq)
	} else {
		tag_free_input
	}
}

// unescape converts entities like "&lt;" to "<". It handles named, numeric, and hex values, and supports
// a wider range of entities than `escape`. For instance, '&apos;', '&#39;', and '&#x27;' unescape to "'".
// While `unescape(escape(s)) == s` always holds true, `escape(unescape(s)) == s` is not always true.
pub fn unescape(input string) string {
	mut result := []rune{}
	runes := input.runes()
	mut i := 0

	outer: for i < runes.len {
		if runes[i] == `&` {
			mut j := i + 1
			for j < runes.len && runes[j] != `;` {
				j++
			}
			if j < runes.len && runes[i + 1] == `#` {
				// Numeric escape sequences (e.g., &#34;)
				code := runes[i + 2..j].string()
				if code[0] == `x` {
					// Hexadecimal escape sequence
					for c in code[1..] {
						if !c.is_hex_digit() {
							// Leave invalid sequences unchanged
							result << runes[i..j + 1]
							i = j + 1
							continue outer
						}
					}
					result << hex.decode(code[1..]) or { []u8{} }.bytestr().runes()
				} else {
					// Decimal escape sequence
					if v := strconv.atoi(code) {
						result << v
					} else {
						// Leave invalid sequences unchanged
						result << runes[i..j + 1]
					}
				}
			} else {
				// Named entity (e.g., &lt;)
				entity := runes[i + 1..j].string()
				if v := html.html_replacement_table[entity] {
					result << v
				} else {
					// Leave unknown entities unchanged
					result << runes[i..j + 1]
				}
			}
			i = j + 1
		} else {
			result << runes[i]
			i++
		}
	}

	return result.string()
}
