module objects

import gx
import rand

pub fn random_color() gx.Color {
	return gx.Color{
		r: byte(rand.int_in_range(0, 256))
		g: byte(rand.int_in_range(0, 256))
		b: byte(rand.int_in_range(0, 256))
	}
}
