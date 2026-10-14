import encoding.html

fn test_unescape_decodes_each_entity_once() {
	assert html.unescape('&amp;#34;') == '&#34;'
	assert html.unescape('&amp;#39;') == '&#39;'
	assert html.unescape('&amp;amp;') == '&amp;'
	assert html.unescape('&amp;lt;') == '&lt;'
	assert html.unescape('&amp;#34;&#34;&amp;#39;&#39;') == '&#34;"&#39;\''
	assert html.unescape('&amp;#34;&#34;', quote: false) == '&#34;&#34;'
	assert html.unescape('&amp;#34;&#34;', all: true) == '&#34;"'
	assert html.unescape(html.escape('&#34; " &#39; \'')) == '&#34; " &#39; \''
}
