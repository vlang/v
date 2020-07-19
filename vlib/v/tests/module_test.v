import os
import crypto.sha256
import term { white }
import crypto.md5 { sum }
import log as l
import time as t { now, utc, Time }
import math
import crypto.sha512

struct TestAliasInStruct {
	time t.Time
}

fn test_import() {
	info := l.Level.info
	assert info == .info
	assert term.white('INFO') == white('INFO')
	assert os.o_rdonly == os.o_rdonly
	assert t.month_days[0] == t.month_days[0]
	assert sha256.size == sha256.size
	assert math.pi == math.pi
	assert sha512.size == sha512.size
	assert md5.sum('module'.bytes()).hex() == sum('module'.bytes()).hex()
	assert t.utc().unix_time() == utc().unix_time()
}

fn test_alias_in_struct_field() {
	a := TestAliasInStruct{
		time: t.Time{
			year: 2020
		}
	}
	assert a.time.year == 2020
}
