struct Bauds {
	standard bool
	bauds    u32
}

fn max(v u32) !u32 {
	if v > 38400 {
		return error('too fast')
	}
	return v
}

fn new_bauds(bauds u32) !&Bauds {
	return &Bauds{
		standard: match bauds {
			300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 38400 { true }
			else { false }
		}
		bauds:    max(bauds)!
	}
}

fn test_main() {
	t := new_bauds(310)!
	assert '${t}' == '&Bauds{
    standard: false
    bauds: 310
}'
}
