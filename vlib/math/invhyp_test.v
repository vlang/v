import math

fn test_acosh() {
	assert math.close(math.acosh(1234567890.12345), 21.627134039822003)
	assert math.close(math.acosh(12.123456789), 3.1865840454481904)
}

fn test_asinh() {
	assert math.close(math.asinh(1234567890.12345), 21.627134039822003)
	assert math.close(math.asinh(12.123456789), 3.1899859431901603)
}
