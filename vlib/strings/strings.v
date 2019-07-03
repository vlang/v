module strings 

pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	mut arr := malloc(n + 1)
	for i := 0; i < n; i++ {
		arr[i] = c
	}
	arr[n] = `\0`
	return tos(arr, n)
}
