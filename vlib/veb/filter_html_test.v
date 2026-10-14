// Coverage for `filter_html`, the function the template compiler emits for a
// `$veb.html()` interpolation.
//
// Nothing in the tree calls `filter_html` from a test: the veb template tests
// exercise `$tmpl`-generated code, which reaches it only through the generated
// `veb__filter` wrapper. It is the one place in the module where a
// user-controlled string is turned into markup, so it deserves direct
// assertions.
//
// The compile-time branch is `$if T is string`, and inside it the trusted type
// is recognised by its exact name. That makes the type-dispatch observable, so
// the interesting cases are the ones a caller can actually hit: a plain string,
// a `veb.RawHtml`, a *different* alias of string, and the non-string types that
// fall through to `str()`.
module veb

// UntrustedHtml is a second alias of `string`, declared in this module. The
// escaping decision is made by matching the name `veb.RawHtml` exactly, so this
// must be escaped rather than emitted verbatim.
type UntrustedHtml = string

struct Named {
pub:
	name string = 'x'
	age  int    = 7
}

// ---------------------------------------------------------------------
// string: the case that matters
// ---------------------------------------------------------------------

fn test_filter_html_escapes_the_five_special_characters() {
	assert veb.filter_html('<') == '&lt;'
	assert veb.filter_html('>') == '&gt;'
	assert veb.filter_html('&') == '&amp;'
	assert veb.filter_html('"') == '&#34;'
	assert veb.filter_html("'") == '&#39;'
	// All five in one string, in both orders, so the escaping is not
	// order-dependent.
	assert veb.filter_html('<>&"\'') == '&lt;&gt;&amp;&#34;&#39;'
	assert veb.filter_html('\'"&><') == '&#39;&#34;&amp;&gt;&lt;'
}

fn test_filter_html_leaves_text_without_specials_alone() {
	assert veb.filter_html('') == ''
	assert veb.filter_html('plain text') == 'plain text'
	assert veb.filter_html('a b\tc\nd') == 'a b\tc\nd'
	assert veb.filter_html('ünïcödé') == 'ünïcödé'
}

fn test_filter_html_escapes_a_typical_injection_attempt() {
	attack := '<script>alert("xss")</script>'
	out := veb.filter_html(attack)
	assert out == '&lt;script&gt;alert(&#34;xss&#34;)&lt;/script&gt;'
	assert out.contains('<script>') == false
}

// ---------------------------------------------------------------------
// veb.RawHtml is emitted verbatim
// ---------------------------------------------------------------------

fn test_filter_html_emits_raw_html_verbatim() {
	assert veb.filter_html(veb.RawHtml('<b>bold</b>')) == '<b>bold</b>'
	assert veb.filter_html(veb.RawHtml('')) == ''
	// The same text through a plain string is escaped, so the branch really
	// is dispatching on the type and not on the value.
	assert veb.filter_html('<b>bold</b>') == '&lt;b&gt;bold&lt;/b&gt;'
	assert veb.filter_html(veb.RawHtml('<b>bold</b>')) != veb.filter_html('<b>bold</b>')
}

fn test_filter_html_emits_raw_html_verbatim_through_a_variable() {
	raw := veb.RawHtml('<i>x</i>')
	assert veb.filter_html(raw) == '<i>x</i>'
}

fn test_filter_html_escapes_another_alias_of_string() {
	// A caller's own alias of `string` is not the trusted type, so it is
	// escaped even though it holds markup.
	alias := UntrustedHtml('<i>not trusted</i>')
	assert veb.filter_html(alias) == '&lt;i&gt;not trusted&lt;/i&gt;'
	assert veb.filter_html(alias) == veb.filter_html('<i>not trusted</i>')
}

// ---------------------------------------------------------------------
// non-string types fall through to str()
// ---------------------------------------------------------------------

fn test_filter_html_uses_str_for_non_string_types() {
	assert veb.filter_html(42) == '42'
	assert veb.filter_html(-7) == '-7'
	assert veb.filter_html(1.5) == '1.5'
	assert veb.filter_html(true) == 'true'
	assert veb.filter_html(false) == 'false'
	assert veb.filter_html(`A`) == 'A'
}

fn test_filter_html_does_not_escape_the_str_of_a_non_string() {
	// `str()` of a non-string is emitted as-is: the escaping branch is only
	// taken for `string`. This is the documented behaviour, and it is the
	// reason a value that is not a `string` must already be trusted.
	assert veb.filter_html(Named{}) == Named{}.str()
	assert veb.filter_html([1, 2, 3]) == [1, 2, 3].str()
	assert veb.filter_html({
		'a': 1
	}) == {
		'a': 1
	}.str()
}

fn test_filter_html_of_a_string_that_str_renders_differently() {
	// `str()` of a string is the string itself, so the escaping branch and
	// the fallback branch differ only for the special characters.
	s := 'a&b'
	assert veb.filter_html(s) != s.str()
	assert veb.filter_html(veb.RawHtml(s)) == s.str()
}
