import time

fn test_custom_format() {
	date := time.now()
	assert date.custom_format('YYYY-MM-DD HH:mm') == date.format()
	assert date.custom_format('MMM') == date.smonth()

	test_str := 'M MM Mo MMM MMMM\nD DD DDD DDDD\nd dd ddd dddd\nYY YYYY a A\nH HH h hh k kk e\nm mm s ss Z ZZ ZZZ\nDo DDDo Q Qo QQ\nN NN w wo ww\nM/D/YYYY N-HH:mm:ss Qo?a'

	println(date.custom_format(test_str))
}

fn test_hours() {
	assert time.parse('2023-08-04 00:00:45')!.custom_format('hh A h a') == '00 AM 0 am'
	assert time.parse('2023-08-04 01:00:45')!.custom_format('hh A h a') == '01 AM 1 am'
	assert time.parse('2023-08-04 02:00:45')!.custom_format('hh A h a') == '02 AM 2 am'
	assert time.parse('2023-08-04 03:00:45')!.custom_format('hh A h a') == '03 AM 3 am'
	assert time.parse('2023-08-04 04:00:45')!.custom_format('hh A h a') == '04 AM 4 am'
	assert time.parse('2023-08-04 05:00:45')!.custom_format('hh A h a') == '05 AM 5 am'
	assert time.parse('2023-08-04 06:00:45')!.custom_format('hh A h a') == '06 AM 6 am'
	assert time.parse('2023-08-04 07:00:45')!.custom_format('hh A h a') == '07 AM 7 am'
	assert time.parse('2023-08-04 08:00:45')!.custom_format('hh A h a') == '08 AM 8 am'
	assert time.parse('2023-08-04 09:00:45')!.custom_format('hh A h a') == '09 AM 9 am'
	assert time.parse('2023-08-04 10:00:45')!.custom_format('hh A h a') == '10 AM 10 am'
	assert time.parse('2023-08-04 11:00:45')!.custom_format('hh A h a') == '11 AM 11 am'
	assert time.parse('2023-08-04 12:00:45')!.custom_format('hh A h a') == '12 PM 12 pm'
	assert time.parse('2023-08-04 13:00:45')!.custom_format('hh A h a') == '01 PM 1 pm'
	assert time.parse('2023-08-04 14:00:45')!.custom_format('hh A h a') == '02 PM 2 pm'
	assert time.parse('2023-08-04 15:00:45')!.custom_format('hh A h a') == '03 PM 3 pm'
	assert time.parse('2023-08-04 16:00:45')!.custom_format('hh A h a') == '04 PM 4 pm'
	assert time.parse('2023-08-04 17:00:45')!.custom_format('hh A h a') == '05 PM 5 pm'
	assert time.parse('2023-08-04 18:00:45')!.custom_format('hh A h a') == '06 PM 6 pm'
	assert time.parse('2023-08-04 19:00:45')!.custom_format('hh A h a') == '07 PM 7 pm'
	assert time.parse('2023-08-04 20:00:45')!.custom_format('hh A h a') == '08 PM 8 pm'
	assert time.parse('2023-08-04 21:00:45')!.custom_format('hh A h a') == '09 PM 9 pm'
	assert time.parse('2023-08-04 22:00:45')!.custom_format('hh A h a') == '10 PM 10 pm'
	assert time.parse('2023-08-04 23:00:45')!.custom_format('hh A h a') == '11 PM 11 pm'
}
