fn main() {
	unsafe {
		mut buffer := malloc_noscan(5)
		s := utf32_to_str_no_malloc(77, buffer)
		println(s)
	}
}
