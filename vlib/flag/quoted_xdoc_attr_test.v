import flag

struct QuotedXdocConfig {
	p_fpga_ver string @[long: fp_ver; name: 'FPGA Version'; xdoc: 'String to use as simulated FPGA version in Version responses. Must be in the form "a.bb.cccc"']
	p_cm_ver   string @[long: cm_ver; name: 'CM Version'; xdoc: 'String to use as simulated CM version in Version responses. Must be in the form "a.bb.cccc"']
}

fn test_flag_to_doc_with_quoted_xdoc_attrs() {
	doc := flag.to_doc[QuotedXdocConfig]()!
	assert doc.contains('--fp-ver <string>')
	assert doc.contains('--cm-ver <string>')
	assert doc.contains('Must be in the form "a.bb.cccc"')
}
