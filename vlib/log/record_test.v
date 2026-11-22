module log

import context
import io
import json2
import sync
import time

struct Capture {
mut:
	buf []u8
}

fn (mut c Capture) write(data []u8) !int {
	c.buf << data
	return data.len
}

fn (c &Capture) text() string {
	return c.buf.bytestr()
}

fn sattr(k string, v string) Attr {
	return Attr{ key: k, value: v }
}

fn iattr(k string, v i64) Attr {
	return Attr{ key: k, value: v }
}

fn strip_timestamp(s string) string {
	if s.len > 26 && s[4] == u8(45) {
		return s[26..]
	}
	return s
}

fn test_text_handler_plain_matches_legacy_format() {
	mut new_cap := &Capture{}
	nh := new_text_handler(io.Writer(new_cap), HandlerOptions{})
	nl := new_logger(nh)
	nl.info('hello')

	mut old_cap := &Capture{}
	mut old := Log{}
	old.set_output_stream(io.Writer(old_cap))
	old.info('hello')

	assert strip_timestamp(new_cap.text()) == strip_timestamp(old_cap.text())
}

fn test_text_handler_with_fields() {
	mut cap := &Capture{}
	h := new_text_handler(io.Writer(cap), HandlerOptions{ color: false })
	l := new_logger(h)
	l.info('request', sattr('user', 'alice'), iattr('code', 42))
	out := cap.text()
	assert out.contains('request')
	assert out.contains('user=alice')
	assert out.contains('code=42')
}

fn test_json_handler_record() {
	mut cap := &Capture{}
	h := new_json_handler(io.Writer(cap), HandlerOptions{})
	l := new_logger(h)
	l.info('hello', sattr('k', 'v'))
	out := cap.text().trim_space()
	decoded := json2.decode[map[string]string](out) or { panic('bad json: ${out} / ${err}') }
	assert decoded['level'] == 'info'
	assert decoded['msg'] == 'hello'
	assert decoded['k'] == 'v'
}

fn test_with_and_group() {
	mut cap := &Capture{}
	h := new_json_handler(io.Writer(cap), HandlerOptions{})
	base := new_logger(h)
	child := base.with(sattr('service', 'api')).with_group('http')
	child.info('done', iattr('status', 200))
	out := cap.text()
	assert out.contains('"service":"api"')
	assert out.contains('"status":200')
}

fn test_level_filtering() {
	mut cap := &Capture{}
	mut var_level := new_level_var(.warn)
	h := new_json_handler_var(io.Writer(cap), HandlerOptions{}, var_level)
	l := new_logger(h)
	l.info('hidden')
	assert cap.text() == ''
	var_level.set(.info)
	l.info('visible')
	assert cap.text().contains('visible')
}

fn test_attrs_builder() {
	mut a := Attrs{}
	a.add(sattr('a', '1')).add(iattr('b', 2))
	assert a.values().len == 2
}

fn test_context_logging() {
	mut cap := &Capture{}
	h := new_json_handler(io.Writer(cap), HandlerOptions{})
	l := new_logger(h)
	ctx := context.with_value(context.background(), 'trace', 'abc')
	l.info_context(ctx, 'with ctx')
	assert cap.text().contains('with ctx')
}

fn test_concurrent_logging() {
	mut cap := &Capture{}
	h := new_text_handler(io.Writer(cap), HandlerOptions{ color: false })
	l := new_logger(h)
	mut wg := sync.new_waitgroup()
	for i in 0 .. 50 {
		wg.add(1)
		spawn fn (l &RecordLogger, wg &sync.WaitGroup, i int) {
			l.info('msg', iattr('i', i))
			wg.done()
		}(l, wg, i)
	}
	wg.wait()
	assert cap.text().split('\n').len >= 50
}

fn test_direct_timestamp_matches_formatter() {
	t := time.utc()
	formats := [
		TimeFormat.tf_rfc3339_micro,
		TimeFormat.tf_ss_micro,
		TimeFormat.tf_ss_milli,
		TimeFormat.tf_ss,
		TimeFormat.tf_default,
		TimeFormat.tf_ymmdd,
		TimeFormat.tf_hhmmss,
	]
	for f in formats {
		mut buf := []u8{}
		assert append_timestamp(mut buf, t, f, false), 'format ${f} should be direct'
		assert buf.bytestr() == format_time(t, f, ''), 'format ${f} differs'
	}
}

fn test_global_structured_logger() {
	old := get_record_logger()
	defer { set_record_logger(old) }
	mut cap := &Capture{}
	h := new_json_handler(io.Writer(cap), HandlerOptions{})
	set_record_logger(new_logger(h))
	info('global', sattr('k', 'v'))
	assert cap.text().contains('"k":"v"')
}

fn test_time_format_custom() {
	mut cap := &Capture{}
	opts := HandlerOptions{
		time_format:        .tf_custom_format
		custom_time_format: 'YYYY'
		color:              false
	}
	h := new_text_handler(io.Writer(cap), opts)
	l := new_logger(h)
	l.info('x')
	assert cap.text().contains(time.now().year.str())
}
