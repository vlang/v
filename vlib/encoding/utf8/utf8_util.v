/*

utf-8 util

Copyright (c) 2019 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains utilities for utf8 strings

*/
module utf8

/*

Utility functions

*/

// len return the leght as number of unicode chars from a string
pub fn len(s string) int {
	mut count := 0
	mut index := 0

	for {
		ch_len := utf8util_char_len(s.str[index])
		index += ch_len
		if index > s.len {
			break
		}
		count++
	}
	return count
}

// u_len return the leght as number of unicode chars from a ustring
pub fn u_len(s ustring) int {
	return len(s.s)
}

// get_uchar convert a unicode glyph in string[index] into a int unicode char
pub fn get_uchar(s string, index int) int {
	mut res := 0
	mut ch_len := 0
	if s.len > 0  {
		ch_len = utf8util_char_len(s.str[index])

		if ch_len == 1 {
			return u16(s.str[index])
		}if ch_len > 1 && ch_len < 5{
			mut lword := 0
			for i:=0; i < ch_len ; i++ {
				lword = (lword << 8 ) | int( s.str[index + i] )
			}

			// 2 byte utf-8
			// byte format: 110xxxxx 10xxxxxx
			//
			if ch_len == 2 {
				res = (lword & 0x1f00) >> 2 | (lword & 0x3f)
			}
			// 3 byte utf-8
			// byte format: 1110xxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 3 {
				res = ( lword & 0x0f0000 ) >> 4 | ( lword & 0x3f00 ) >> 2 | ( lword & 0x3f )
			}
			// 4 byte utf-8
			// byte format: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 4 {
				res = (( lword & 0x07000000 ) >> 6)  | (( lword & 0x003f0000 ) >> 4) |
						(( lword & 0x00003F00 ) >> 2 ) | ( lword & 0x0000003f )
			}


		}

	}
	return res
}


/*

Conversion functions

*/

// to_upper return an uppercase string from a string
pub fn to_upper(s string) string {
	return up_low(s, true)
}

// u_to_upper return an uppercase string from a ustring
pub fn u_to_upper(s ustring) ustring {
	tmp := up_low(s.s, true)
	return tmp.ustring()
}

// to_lower return an lowercase string from a string
pub fn to_lower(s string) string {
	return up_low(s, false)
}

// u_to_lower return an lowercase string from a ustring
pub fn u_to_lower(s ustring) ustring {
	tmp := up_low(s.s, false)
	return tmp.ustring()
}


/*

Punctuation functions

The "western" function search on a small table, that is quicker than
the global unicode table search. **Use only for western chars**.

*/

//
// Western
//

// is_punct return true if the string[index] byte is the start of a unicode western punctuation
pub fn is_punct( s string , index int) bool {
	return is_uchar_punct(get_uchar(s, index))
}

// is_uchar_punct return true if the input unicode is a western unicode punctuation
pub fn is_uchar_punct( uchar int ) bool {
	return find_punct_in_table(uchar, unicode_punct_western ) != 0
}

//
// Global
//

// is_global_punct return true if the string[index] byte of is the start of a global unicode punctuation
pub fn is_global_punct( s string , index int) bool {
	return is_uchar_global_punct(get_uchar(s, index))
}

// is_uchar_global_punct return true if the input unicode is a global unicode punctuation
pub fn is_uchar_global_punct( uchar int ) bool {
	return find_punct_in_table( uchar , unicode_punct ) != 0
}


/*

Private functions

*/
// utf8util_char_len calculate the length in bytes of a utf8 char
fn utf8util_char_len(b byte) int {
	return (( 0xe5000000 >> (( b >> 3 ) & 0x1e )) & 3 ) + 1
}

//
// if upper_flag == true  then make low ==> upper conversion
// if upper_flag == false then make upper ==> low conversion
//
// up_low make the dirt job
fn up_low(s string, upper_flag bool) string {
	mut index := 0
	mut str_res := malloc(s.len + 1)

	for {
		ch_len := utf8util_char_len(s.str[index])

		if ch_len == 1 {
			if upper_flag==true {
				str_res[index] = byte(C.toupper(s.str[index]))
			}else{
				str_res[index] = byte(C.tolower(s.str[index]))
			}
		}
		else if ch_len > 1 && ch_len < 5{
			mut lword := 0

			for i:=0; i < ch_len ; i++ {
				lword = (lword << 8 ) | int( s.str[index + i] )
			}

			//C.printf(" #%d (%x) ", index, lword)

			mut res := 0

			// 2 byte utf-8
			// byte format: 110xxxxx 10xxxxxx
			//
			if ch_len == 2 {
				res = (lword & 0x1f00) >> 2 | (lword & 0x3f)
			}
			// 3 byte utf-8
			// byte format: 1110xxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 3 {
				res = ( lword & 0x0f0000 ) >> 4 | ( lword & 0x3f00 ) >> 2 | ( lword & 0x3f )
			}
			// 4 byte utf-8
			// byte format: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
			//
			else if ch_len == 4 {
				res = (( lword & 0x07000000 ) >> 6)  | (( lword & 0x003f0000 ) >> 4) |
						(( lword & 0x00003F00 ) >> 2 ) | ( lword & 0x0000003f )
			}

			//C.printf("len: %d code: %04x ",ch_len,res)
			ch_index := find_char_in_table(u16(res), upper_flag)
			//C.printf(" utf8 index: %d ",ch_index)

			// char not in table, no need of conversion
			if ch_index == 0 {
				for i in 0..ch_len {
					str_res[index + i] = s.str[index + i]
				}
				//C.printf("\n")
			}else{
				tab_char := unicode_con_table_up_to_low[ch_index]
				//C.printf("tab_char: %04x ",tab_char)

				if ch_len == 2 {
					ch0 := byte( (tab_char >> 6) & 0x1f ) | 0xc0  	/*110x xxxx*/
					ch1 := byte( (tab_char >> 0) & 0x3f ) | 0x80		/*10xx xxxx*/
					//C.printf("[%02x%02x] \n",ch0,ch1)

					str_res[ index + 0 ] = ch0
					str_res[ index + 1 ] = ch1

					//****************************************************************
					//  BUG: doesn't compile, workaround use shitf to right of 0 bit
					//****************************************************************
					//str_res[index + 1 ] = byte( tab_char & 0xbf )			/*1011 1111*/

				}
				else if ch_len == 3 {
					ch0 := byte( (tab_char >> 12) & 0x0f ) | 0xe0  	/*1110 xxxx*/
					ch1 := byte( (tab_char >> 6) & 0x3f ) | 0x80		/*10xx xxxx*/
					ch2 := byte( (tab_char >> 0) & 0x3f ) | 0x80		/*10xx xxxx*/
					//C.printf("[%02x%02x%02x] \n",ch0,ch1,ch2)

					str_res[index + 0 ] = ch0
					str_res[index + 1 ] = ch1
					str_res[index + 2 ] = ch2
				}
				// TODO: write if needed
				else if ch_len == 4 {
					// place holder!!
					// at the present time simply copy the utf8 char
					for i in 0..ch_len {
						str_res[index + i] = s.str[index + i]
					}
				}
			}

		}
		// other cases, just copy the string
		else{
			for i in 0..ch_len {
				str_res[index + i] = s.str[index + i]
			}
		}

		index += ch_len

		// we are done, exit the loop
		if index >= s.len {
			break
		}
	}

	// for c compatibility set the ending 0
	str_res[index]=0

	//C.printf("str_res: %s\n--------------\n",str_res)

	return tos(str_res, s.len)
}

// find_char_in_table utility function for up_low, search utf8 chars in the conversion table
fn find_char_in_table( in_code u16, upper_flag bool) int {
	//
	// We will use a simple binary search
	//

	mut first_index := 0 										// first index of our utf8 char range
	mut last_index := (unicode_con_table_up_to_low.len >> 1)		// last+1 index of our utf8 char range
	mut index := 0
	mut x := u16(0)

	mut offset:=0 		// up to low
	mut i_step:=1		// up to low
	if upper_flag==true {
		offset=1		// low to up
		i_step=0		// low to up
	}

	//C.printf("looking for [%04x] in (%d..%d).\n",in_code,first_index,last_index)
	for {
		index = (first_index+last_index) >> 1
		x = unicode_con_table_up_to_low[ (index<<1)+offset ]

		//C.printf("(%d..%d) index:%d base[%04x]==>[%04x]\n",first_index,last_index,index,in_code,x)

		if x == in_code {
			//C.printf(" Found!\n")
			return ( (index<<1) + i_step)
		}
		else if x>in_code {
			last_index=index
		}else {
			first_index=index
		}

		if (last_index-first_index)<=1 {
			break
		}
	}
	//C.printf("not found.\n")
	return 0
}

// find punct in lockup table
fn find_punct_in_table( in_code int , in_table []int ) int {
	//
	// We will use a simple binary search
	//

	mut first_index := 0
	mut last_index := (in_table.len)
	mut index := 0
	mut x := 0

	for {
		index = (first_index+last_index) >> 1
		x = in_table[ index ]
		//C.printf("(%d..%d) index:%d base[%08x]==>[%08x]\n",first_index,last_index,index,in_code,x)

		if x == in_code {
			return index
		}
		else if x>in_code {
			last_index=index
		}else {
			first_index=index
		}

		if (last_index-first_index)<=1 {
			break
		}
	}
	//C.printf("not found.\n")
	return 0
}