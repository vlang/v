module objects

import gx
import rand

pub fn random_color() gx.Color {
	return gx.Color{
		r: rand.byte()
		g: rand.byte()
		b: rand.byte()
	}
}
