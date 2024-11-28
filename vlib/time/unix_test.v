import time

fn test_unix_timestamp_conversion_to_time() {
	t := time.now()
	ss := dump(time.unix(t.unix()))
	sms := dump(time.unix_milli(t.unix_milli()))
	sus := dump(time.unix_micro(t.unix_micro()))
	sns := dump(time.unix_nano(t.unix_nano()))
	assert sns.format_ss() == ss.format_ss()
	assert sns.format_ss_milli() == sms.format_ss_milli()
	assert sns.format_ss_micro() == sus.format_ss_micro()
}
