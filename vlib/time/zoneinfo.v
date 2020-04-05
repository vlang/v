module time

import (
	math
)

const (
	GMT = "0:00Z" 
)

struct Zone {
	name string
	offset int
	is_dst bool
}

struct ZoneTrans {
	when i64
	idx u8
	is_std bool
	is_utc bool
}

pub struct Localtion {
	name string
	zone []Zone
	tx []ZoneTrans
mut:
	cache_start i64
	cache_end i64
	cache_zone &Zone
}

fn (l &Localtion) get() &Location{
	if isnil(l){
		return utc_loc
	}
	if l == &local_loc {

	}
}

pub fn load_location(name string) ?Localtion {
	if name == '' || name == 'UTC'{
		return UTC
	}
	if name == 'Local'{
		return Local
	}
	if contains_dot_dot(name) || name[0] == '/' || name[0] == '\\'{
		return error('syntax error: invalid location.')
	}
}

pub fn fixed_zone(name string,offset int) &Location {
	l := Location{
		name: name,
		zone: []zone << zone{name,offset,false}
		cache_start: math.min_i64
		cache_end: math.max_i64
	}
}

fn contains_dot_dot(s string) bool{
	if s.len < 2 {
		return false
	}
	for i := 0; i < s.len - 1; i++{
		if s[i] == '.' && s[i+1] == '.'{
			return true
		}
	}
	return false
}
