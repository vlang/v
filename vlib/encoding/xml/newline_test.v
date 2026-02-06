import encoding.xml

fn test_newline() {
	doc := xml.XMLDocument.from_string('<?xml version="1.0" encoding="UTF-8"?>
<feed\r\nattr="value">
	<title>Test</title>
</feed>
\r\n<\r\ntest\r\n>\r
<\r\n/\ntest\r>
')!
	_ = doc
}
