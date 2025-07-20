import x.json2

struct Disk {
	dev  string
	size ?struct {
		value u64
	}
}

fn test_main() {
	disk := Disk{
		size: struct {
			value: 123
		}
	}
	disk_str := json2.encode[Disk](disk)
	assert disk_str == '{"dev":"","size":{"value":123}}'
}

fn test_none() {
	disk := Disk{}
	disk_str := json2.encode[Disk](disk)
	assert disk_str == '{"dev":""}'
}
