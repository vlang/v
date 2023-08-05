import time

fn test_custom_format() {
	date := time.now()
	assert date.custom_format('YYYY-MM-DD HH:mm') == date.format()
	assert date.custom_format('MMM') == date.smonth()

	test_str := 'M MM Mo MMM MMMM\nD DD DDD DDDD\nd dd ddd dddd\nYY YYYY a A\nH HH h hh k kk e\nm mm s ss Z ZZ ZZZ\nDo DDDo Q Qo QQ\nN NN w wo ww\nM/D/YYYY N-HH:mm:ss Qo?a'

	println(date.custom_format(test_str))
}

fn test_hh() {
	assert time.parse('2023-08-04 00:00:45')!.custom_format('hh') == '00'
	assert time.parse('2023-08-04 01:00:45')!.custom_format('hh') == '01'
	assert time.parse('2023-08-04 02:00:45')!.custom_format('hh') == '02'
	assert time.parse('2023-08-04 03:00:45')!.custom_format('hh') == '03'
	assert time.parse('2023-08-04 04:00:45')!.custom_format('hh') == '04'
	assert time.parse('2023-08-04 05:00:45')!.custom_format('hh') == '05'
	assert time.parse('2023-08-04 06:00:45')!.custom_format('hh') == '06'
	assert time.parse('2023-08-04 07:00:45')!.custom_format('hh') == '07'
	assert time.parse('2023-08-04 08:00:45')!.custom_format('hh') == '08'
	assert time.parse('2023-08-04 09:00:45')!.custom_format('hh') == '09'
	assert time.parse('2023-08-04 10:00:45')!.custom_format('hh') == '10'
	assert time.parse('2023-08-04 11:00:45')!.custom_format('hh') == '11'
	assert time.parse('2023-08-04 12:00:45')!.custom_format('hh') == '12'
	assert time.parse('2023-08-04 13:00:45')!.custom_format('hh') == '01'
	assert time.parse('2023-08-04 14:00:45')!.custom_format('hh') == '02'
	assert time.parse('2023-08-04 15:00:45')!.custom_format('hh') == '03'
	assert time.parse('2023-08-04 16:00:45')!.custom_format('hh') == '04'
	assert time.parse('2023-08-04 17:00:45')!.custom_format('hh') == '05'
	assert time.parse('2023-08-04 18:00:45')!.custom_format('hh') == '06'
	assert time.parse('2023-08-04 19:00:45')!.custom_format('hh') == '07'
	assert time.parse('2023-08-04 20:00:45')!.custom_format('hh') == '08'
	assert time.parse('2023-08-04 21:00:45')!.custom_format('hh') == '09'
	assert time.parse('2023-08-04 22:00:45')!.custom_format('hh') == '10'
	assert time.parse('2023-08-04 23:00:45')!.custom_format('hh') == '11'
}

fn test_h() {
	assert time.parse('2023-08-04 00:00:45')!.custom_format('h') == '0'
	assert time.parse('2023-08-04 01:00:45')!.custom_format('h') == '1'
	assert time.parse('2023-08-04 02:00:45')!.custom_format('h') == '2'
	assert time.parse('2023-08-04 03:00:45')!.custom_format('h') == '3'
	assert time.parse('2023-08-04 04:00:45')!.custom_format('h') == '4'
	assert time.parse('2023-08-04 05:00:45')!.custom_format('h') == '5'
	assert time.parse('2023-08-04 06:00:45')!.custom_format('h') == '6'
	assert time.parse('2023-08-04 07:00:45')!.custom_format('h') == '7'
	assert time.parse('2023-08-04 08:00:45')!.custom_format('h') == '8'
	assert time.parse('2023-08-04 09:00:45')!.custom_format('h') == '9'
	assert time.parse('2023-08-04 10:00:45')!.custom_format('h') == '10'
	assert time.parse('2023-08-04 11:00:45')!.custom_format('h') == '11'
	assert time.parse('2023-08-04 12:00:45')!.custom_format('h') == '12'
	assert time.parse('2023-08-04 13:00:45')!.custom_format('h') == '1'
	assert time.parse('2023-08-04 14:00:45')!.custom_format('h') == '2'
	assert time.parse('2023-08-04 15:00:45')!.custom_format('h') == '3'
	assert time.parse('2023-08-04 16:00:45')!.custom_format('h') == '4'
	assert time.parse('2023-08-04 17:00:45')!.custom_format('h') == '5'
	assert time.parse('2023-08-04 18:00:45')!.custom_format('h') == '6'
	assert time.parse('2023-08-04 19:00:45')!.custom_format('h') == '7'
	assert time.parse('2023-08-04 20:00:45')!.custom_format('h') == '8'
	assert time.parse('2023-08-04 21:00:45')!.custom_format('h') == '9'
	assert time.parse('2023-08-04 22:00:45')!.custom_format('h') == '10'
	assert time.parse('2023-08-04 23:00:45')!.custom_format('h') == '11'
}
