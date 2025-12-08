import math

fn test_acosh() {
	assert math.close(math.acosh(1234567890.12345), 21.627134039822003)
	assert math.close(math.acosh(12.123456789), 3.1865840454481904)
}
