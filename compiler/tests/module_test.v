import os
import time as t
import crypto.sha256 as s2

import (
	math
	log as l
	crypto.sha512 as s5
)

fn test_import() {
	assert os.SUCCESS == os.SUCCESS &&
		t.MonthDays[0] == t.MonthDays[0] &&
		s2.Size == s2.Size &&
		math.Pi == math.Pi &&
		l.INFO == l.INFO &&
		s5.Size == s5.Size
}
