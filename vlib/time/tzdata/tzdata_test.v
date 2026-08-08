import time
import time.tzdata as _

fn test_import_tzdata_registers_embedded_loader() {
	loc := time.load_location('Asia/Shanghai')!
	t := loc.unix_to_local(1_704_067_200)!
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 1
	assert t.hour == 8
}
