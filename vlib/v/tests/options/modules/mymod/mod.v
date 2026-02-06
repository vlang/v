// mod.v

module mymod

import math.big

pub struct BigintRange {
pub mut:
	start big.Integer
	end   big.Integer
}

pub const range = BigintRange{
	start: big.integer_from_string('338288524927261089654018896841347694592')!
	end:   big.integer_from_string('338620831926207318622244848606417780735')!
}
