module attrprobe

import v.capi

@[capi.CApiSkip]
pub fn hidden() int {
	return 0
}

@[capi.CApiName('renamed_sum')]
pub fn sum(a int, b int) int {
	return a + b
}

@[capi.CApiExport]
fn internal() int {
	return 1
}
