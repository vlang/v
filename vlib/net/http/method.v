// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// The methods listed here are all of those on the list available at:
// https://www.iana.org/assignments/http-methods/http-methods.xhtml
pub enum Method { // as of 2023-06-20
	get // Note: get ***should*** remain the first value here, to ensure that http.fetch() by default will use it
	head
	post
	put
	// uncommon ones:
	acl
	baseline_control
	bind
	checkin
	checkout
	connect
	copy
	delete
	label
	link
	lock
	merge
	mkactivity
	mkcalendar
	mkcol
	mkredirectref
	mkworkspace
	move
	options
	orderpatch
	patch
	pri
	propfind
	proppatch
	rebind
	report
	search
	trace
	unbind
	uncheckout
	unlink
	unlock
	update
	updateredirectref
	version_control
}

// str returns the string representation of the HTTP Method `m`.
pub fn (m Method) str() string {
	return match m {
		.get { 'GET' }
		.head { 'HEAD' }
		.post { 'POST' }
		.put { 'PUT' }
		// uncommon ones:
		.acl { 'ACL' }
		.baseline_control { 'BASELINE-CONTROL' }
		.bind { 'BIND' }
		.checkin { 'CHECKIN' }
		.checkout { 'CHECKOUT' }
		.connect { 'CONNECT' }
		.copy { 'COPY' }
		.delete { 'DELETE' }
		.label { 'LABEL' }
		.link { 'LINK' }
		.lock { 'LOCK' }
		.merge { 'MERGE' }
		.mkactivity { 'MKACTIVITY' }
		.mkcalendar { 'MKCALENDAR' }
		.mkcol { 'MKCOL' }
		.mkredirectref { 'MKREDIRECTREF' }
		.mkworkspace { 'MKWORKSPACE' }
		.move { 'MOVE' }
		.options { 'OPTIONS' }
		.orderpatch { 'ORDERPATCH' }
		.patch { 'PATCH' }
		.pri { 'PRI' }
		.propfind { 'PROPFIND' }
		.proppatch { 'PROPPATCH' }
		.rebind { 'REBIND' }
		.report { 'REPORT' }
		.search { 'SEARCH' }
		.trace { 'TRACE' }
		.unbind { 'UNBIND' }
		.uncheckout { 'UNCHECKOUT' }
		.unlink { 'UNLINK' }
		.unlock { 'UNLOCK' }
		.update { 'UPDATE' }
		.updateredirectref { 'UPDATEREDIRECTREF' }
		.version_control { 'VERSION-CONTROL' }
	}
}

// method_from_str returns the corresponding Method enum field
// given a string `m`, e.g. `'GET'` would return Method.get.
//
// Currently, the default value is Method.get for unsupported string value.
pub fn method_from_str(m string) Method {
	return match m {
		'GET' { Method.get }
		'HEAD' { Method.head }
		'POST' { Method.post }
		'PUT' { Method.put }
		// uncommon ones:
		'ACL' { Method.acl }
		'BASELINE-CONTROL' { Method.baseline_control }
		'BIND' { Method.bind }
		'CHECKIN' { Method.checkin }
		'CHECKOUT' { Method.checkout }
		'CONNECT' { Method.connect }
		'COPY' { Method.copy }
		'DELETE' { Method.delete }
		'LABEL' { Method.label }
		'LINK' { Method.link }
		'LOCK' { Method.lock }
		'MERGE' { Method.merge }
		'MKACTIVITY' { Method.mkactivity }
		'MKCALENDAR' { Method.mkcalendar }
		'MKCOL' { Method.mkcol }
		'MKREDIRECTREF' { Method.mkredirectref }
		'MKWORKSPACE' { Method.mkworkspace }
		'MOVE' { Method.move }
		'OPTIONS' { Method.options }
		'ORDERPATCH' { Method.orderpatch }
		'PATCH' { Method.patch }
		'PRI' { Method.pri }
		'PROPFIND' { Method.propfind }
		'PROPPATCH' { Method.proppatch }
		'REBIND' { Method.rebind }
		'REPORT' { Method.report }
		'SEARCH' { Method.search }
		'TRACE' { Method.trace }
		'UNBIND' { Method.unbind }
		'UNCHECKOUT' { Method.uncheckout }
		'UNLINK' { Method.unlink }
		'UNLOCK' { Method.unlock }
		'UPDATE' { Method.update }
		'UPDATEREDIRECTREF' { Method.updateredirectref }
		'VERSION-CONTROL' { Method.version_control }
		else { Method.get } // always default to .get, it is the safest
	}
}
