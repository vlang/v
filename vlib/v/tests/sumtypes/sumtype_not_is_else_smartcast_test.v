type NotIsElseSumtype = NotIsElseBb | NotIsElseCc

struct NotIsElseBb {
	b int
}

struct NotIsElseCc {
	c int
}

fn test_sumtype_not_is_else_smartcast() {
	aa := NotIsElseSumtype(NotIsElseBb{0})
	if aa !is NotIsElseBb {
		assert false
	} else {
		assert aa.b == 0
	}
}
