module other

import tsc.execute.tsc
import gostd.gort

// make returns a value stored in a generic heap object using an imported type.
pub fn make() int {
	c := &gort.Cell[tsc.CompileTimes]{
		v: tsc.CompileTimes{ n: 3 }
	}
	return c.v.n
}
