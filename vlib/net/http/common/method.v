module common

// The methods listed here are all of those on the list available at:
// https://www.iana.org/assignments/http-methods/http-methods.xhtml
pub enum Method { // as of 2023-06-20
	get
	head
	post
	put
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

pub fn (m Method) str() string {
	return match m {
		.get { 'GET' }
		.head { 'HEAD' }
		.post { 'POST' }
		.put { 'PUT' }
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

pub fn method_from_str(m string) Method {
	return method_from_str_known(m) or { Method.get }
}

// method_from_str_known converts a method string to Method and returns none for unknown values.
pub fn method_from_str_known(m string) ?Method {
	return match m {
		'GET' { Method.get }
		'HEAD' { Method.head }
		'POST' { Method.post }
		'PUT' { Method.put }
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
		else { none }
	}
}
