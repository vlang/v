module io

import os

const (
	default_buf_len = 128*1024 // large for fast reading of big(ish) files
	default_buf_max = 0        // 0 -> no limit
)

pub struct ReaderOptions {
	buf_len int = 128*1024
	buf_max int = 0
	//buf     &[]byte = 0 // TODO - ability to pass external buffer ([]byte)
	filename string = ""
	file    os.File
}

struct Reader {
mut:
	file      os.File
	buf       []byte
	buf_max   int
	offset    int  // start of line offset
	carry     int  // number of bytes to carry between file reads
	n_read    int  // number of bytes read in the last file read op
	is_end    bool // end of file reached
	do_read   bool // file read operation should be perfomed on next call
}

pub fn new_reader(o ReaderOptions) ? &Reader {
	assert o.buf_len >= 2
	assert o.buf_len <= o.buf_max || o.buf_max==0
	// file
	mut file := os.File{}
	if o.filename=="" {
		file = o.file
	} else {
		file = os.open(o.filename) ?
	}
	// TODO buffer
	// create
	r := &Reader{
		file:      file
		buf:       []byte{len:o.buf_len, cap:o.buf_len}
		buf_max:   o.buf_max
		offset:    0
		carry:     0
		n_read:    0
		is_end:    false
		do_read:   true
	}
	return r
}

pub fn (mut r Reader) close() {
	r.file.close()
	// TODO free the buffer ?
}

// NOTICE abstracting read_line into read_string(delim) and calling that in
//        read_line + stripping \r reduces the performance by 2 times

// read_line
[direct_array_access]
pub fn (mut r Reader) read_line() ? (string,bool) {
	unsafe {
		
		// read file into buffer (when necessary)
		if r.do_read {
			if r.is_end { return none } // it's faster when it's here vs before if r.do_read			
			r.n_read = r.file.read_bytes_into(r.buf[r.carry..], r.buf.len-r.carry) + r.carry // TODO rename r.n_read
			//eprintln('BUF ${r.carry} ${r.n_read} ${r.buf}')
			r.do_read = false
			if r.n_read == 0 { return none }
		}
		
		// scan buffer
		start := r.offset
		idx := r.buf[start..r.n_read].index(`\n`)
				
		// found
		if idx>=0 {
			mut len := idx
			r.offset += len+1
			if idx>0 && r.buf[start+len-1]==`\r` { len-- }
			return tos(&r.buf[start], len),false
		}
		
		mut len := r.n_read - start
		
		// not found, last line
		if r.n_read < r.buf.len {
			r.is_end = true
			r.do_read = true // required for r.is_end to be checked
			if len>1 && r.buf[start+len-1]==`\r` { len-- }
			return tos(&r.buf[start], len),false // TODO check golang 
		}
		
		// not found (and line is longer than the buffer)
		if start == 0 {
			
			// buffer size limit reached
			if r.buf_max != 0 && r.buf.len >= r.buf_max {
				r.carry = 0
				r.do_read = true
				if len>1 && r.buf[start+len-1]==`\r` { len-- }
				return tos(&r.buf[0], len),true
			}
			
			// grow the buffer
			r.carry = r.buf.len
			mut new_size := r.buf.len*2
			if r.buf_max != 0 {
				if new_size > r.buf_max {
					new_size = r.buf_max
				}
			} 
			//eprintln('growing to $new_size')
			r.buf << [byte(0)].repeat(new_size-r.buf.len) // TODO better way?
		
		// not found, carry start of the line to the buffer's head
		} else {
			r.carry = r.n_read - start
			for i in 0..r.carry {
				r.buf[i] = r.buf[start+i]
			}
		}
		
		// continue scanning
		r.offset = 0
		r.do_read = true
		return r.read_line() ?
	}
}

/*

// ---[ EXAMPLE ]---------------------------------------------------------------

import os
import io

f := os.open('example.txt') ?
mut r := io.new_reader({file:f}) ? 
for {
	line,_ := r.read_line() or { break }
	println('> $line')
}

*/

