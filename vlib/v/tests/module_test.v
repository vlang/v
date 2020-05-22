import os
import time as t
import crypto.sha256
import math
import log as l
import crypto.sha512

struct TestAliasInStruct {
	time t.Time
}

fn test_import() {
	info := l.Level.info
	assert os.o_rdonly == os.o_rdonly && t.month_days[0] == t.month_days[0] && sha256.size ==
		sha256.size && math.pi == math.pi && info == .info && sha512.size == sha512.size
}

fn test_alias_in_struct_field() {
	a := TestAliasInStruct{
		time: t.Time{
			year: 2020
		}
	}
	assert a.time.year == 2020
}
