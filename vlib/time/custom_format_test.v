import time

fn test_custom_format() {
	date := time.now()
	assert date.custom_format('YYYY-MM-DD HH:mm') == date.format()
	assert date.custom_format('MMM') == date.smonth()

	test_str := 'M MM MMM MMMM\nD DD DDD DDDD\nd dd ddd dddd\nYY YYYY a A\nH HH h hh k kk e\nm mm s ss Z ZZ ZZZ\nDo DDDo Q Qo QQ\nN NN w wo ww\nM/D/YYYY N-HH:mm:ss Qo?a'

	println(date.custom_format(test_str))
}
