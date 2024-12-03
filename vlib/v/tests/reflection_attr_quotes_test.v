import v.reflection as r

struct MyParams {
	p_fpga_ver string @[long: fp_ver; name: 'FPGA Version'; xdoc: 'String to use as simulated FPGA version in Version responses. Must be in the form "a.bb.cccc"']
	p_cm_ver   string @[long: cm_ver; name: 'CM Version'; xdoc: 'String to use as simulated CM version in Version responses. Must be in the form "a.bb.cccc"']
}

fn test_main() {
	a := MyParams{}
	t := r.type_of(a)
	if t.sym.info is r.Struct {
		assert t.sym.info.fields[0].attrs[2] == 'xdoc=String to use as simulated FPGA version in Version responses. Must be in the form "a.bb.cccc"'
	} else {
		assert false
	}
}
