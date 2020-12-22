import os
import crypto.sha256
import term { white }
import crypto.md5 { sum }
import log as l
import time as t { now, utc, Time }
import math
import crypto.sha512
import cli { Command }

struct TestAliasInStruct {
	time Time
}

fn test_import() {
	info := l.Level.info
	assert info == .info
	assert white('INFO') == white('INFO')
	assert os.o_rdonly == os.o_rdonly
	assert t.month_days[0] == t.month_days[0]
	assert sha256.size == sha256.size
	assert math.pi == math.pi
	assert sha512.size == sha512.size
	assert sum('module'.bytes()).hex() == sum('module'.bytes()).hex()
	assert utc().unix_time() == utc().unix_time()
}

fn test_imports_array_as_fn_arg() {
	mut cmd := Command{
		name: 'module test'
	}
	c1 := Command{}
	c2 := Command{
		name: 'cmd2'
	}
	cmd.add_commands([c1, c2])
}

fn test_alias_in_struct_field() {
	a := TestAliasInStruct{
		time: Time{
			year: 2020
		}
	}
	assert a.time.year == 2020
}
