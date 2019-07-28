import os
import time as t
import crypto.sha256 as s2

import (
	math
	log as l
	crypto.sha515 as s5
)

pub fn test_modules() {
	assert os.SUCCESS == os.SUCCESS &&
		t.MonthDays[0] == t.MonthDays[0] &&
		s2.Size == s2.Size &&
		math.Pi == math.Pi &&
		l.INFO == l.INFO &&
		s5.Size == s5.Size
}
