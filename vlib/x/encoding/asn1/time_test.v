// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import os
import time

fn test_serialize_utctime_basic() ! {
	inp := '191215190210Z'

	exp := [u8(0x17), 0x0D, 49, 57, 49, 50, 49, 53, 49, 57, 48, 50, 49, 48, 90]

	ut := UtcTime.new(inp)!
	out := encode(ut)!

	assert out == exp

	// back
	back, pos := UtcTime.decode(out)!
	assert back.tag().tag_number() == int(TagType.utctime)
	assert back == ut
	assert back.value == inp
}

fn test_create_utctime_from_std_time() ! {
	now := time.new(year: 2024, month: 11, day: 13, hour: 17, minute: 45, second: 50)

	utb := UtcTime.from_time(now)!
	utc := UtcTime.from_time(now)!

	assert utb.value == utc.value

	tt := utc.into_utctime()!
	assert now == tt

	inp := '191215190210Z'
	t_inp := UtcTime.new(inp)!
	t_inp_utc := t_inp.into_utctime()!

	// time to UtcTime back
	tinp_back := UtcTime.from_time(t_inp_utc)!
	assert tinp_back.value == inp
}

fn test_create_utctime_from_std_time_with_negative_offset() ! {
	tz := os.getenv('TZ')
	os.setenv('TZ', 'utc 1', true)

	defer {
		os.setenv('TZ', tz, true)
	}

	now := time.new(year: 2024, month: 11, day: 13, hour: 17, minute: 45, second: 50)
	UtcTime.from_time(now)!
}

fn test_serialize_utctime_error_without_z() ! {
	// this input does not contains zulu 'Z' part
	inp := '191215190210'

	exp := [u8(0x17), 0x0D, 49, 57, 49, 50, 49, 53, 49, 57, 48, 50, 49, 48]

	ut := UtcTime.new(inp) or {
		assert err == error('UtcTime: fail on validate utctime')
		return
	}
}

fn test_serialize_utctime_error_month() ! {
	// the month part is > 12
	inp := '191815190210Z'

	exp := [u8(0x17), 0x0D, 49, 57, 49, 56, 49, 53, 49, 57, 48, 50, 49, 48]

	ut := UtcTime.new(inp) or {
		assert err == error('UtcTime: fail on validate utctime')
		return
	}
}

fn test_serialize_utctime_error_day() ! {
	// the day part is > 30
	inp := '191235190210Z'

	exp := [u8(0x17), 0x0D, 0x31, 0x39, 0x31, 0x32, 0x31, 0x32, 0x31, 0x39, 0x30, 0x32, 0x31, 0x30,
		0x5A]

	ut := UtcTime.new(inp) or {
		assert err == error('UtcTime: fail on validate utctime')
		return
	}
}

fn test_serialize_decode_generalizedtime() ! {
	s := '20100102030405Z'

	exp := [u8(0x18), 0x0f, 50, 48, 49, 48, 48, 49, 48, 50, 48, 51, 48, 52, 48, 53, 90]

	gt := GeneralizedTime.new(s)!
	out := encode(gt)!
	assert out == exp

	// back
	back, pos := GeneralizedTime.decode(out)!

	assert back == gt
	assert back.value == s
	assert back.tag().tag_number() == int(TagType.generalizedtime)
}

fn test_create_generalizedtime_from_std_time() ! {
	now := time.new(year: 2024, month: 11, day: 13, hour: 17, minute: 45, second: 50)

	gtb := GeneralizedTime.from_time(now)!
	gtc := GeneralizedTime.from_time(now)!

	assert gtb.value == gtc.value
	assert gtb.value == '20241113174550Z'
	assert gtc.value == '20241113174550Z'

	tt := gtc.into_utctime()!
	assert now == tt

	s := '20100102030405Z'
	gt := GeneralizedTime.new(s)!

	g_utc := gt.into_utctime()!
	g_utc_back := GeneralizedTime.from_time(g_utc)!

	assert g_utc_back.value == s
}

fn test_create_generalizedtime_from_std_time_with_negative_offset() ! {
	tz := os.getenv('TZ')
	os.setenv('TZ', 'utc 1', true)

	defer {
		os.setenv('TZ', tz, true)
	}

	now := time.new(year: 2024, month: 11, day: 13, hour: 17, minute: 45, second: 50)
	GeneralizedTime.from_time(now)!
}
