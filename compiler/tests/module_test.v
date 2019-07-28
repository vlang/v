import os
import time as t

import (
	math
	log as l
)

pub fn test_modules() {
	assert os.SUCCESS == os.SUCCESS &&
		t.MonthDays[0] == t.MonthDays[0] &&
		math.Pi == math.Pi &&
		l.INFO == l.INFO
}
