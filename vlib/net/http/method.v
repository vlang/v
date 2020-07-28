// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

pub enum Method {
	get
	post
	put
	head
	delete
	options
	trace
	connect
	patch
}

fn (m Method) str() string {
	return match m {
		.get { 'GET' }
		.post { 'POST' }
		.put { 'PUT' }
		.head { 'HEAD' }
		.delete { 'DELETE' }
		.options { 'OPTIONS' }
		.trace { 'TRACE' }
		.connect { 'CONNECT' }
		.patch { 'PATCH' }
	}
}

pub fn method_from_str(m string) Method {
	return match m {
		'GET' { Method.get }
		'POST' { Method.post }
		'PUT' { Method.put }
		'HEAD' { Method.head }
		'DELETE' { Method.delete }
		'CONNECT' { Method.connect }
		else { Method.get } // should we default to GET?
	}
}
