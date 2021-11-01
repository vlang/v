module jsdom

pub struct JS.Document {
}

pub struct Document {
	Node
}

pub struct Location {
mut:
	loc JS.Location [noinit]
}

pub fn (l Location) str() string {
	mut res := 'Location{\n'
	res += '  origin: $l.origin()\n'
	res += '  href: $l.href()\n'
	res += '  protocol: $l.protocol()\n'
	res += '  host: $l.host()\n'
	res += '  hostname: $l.hostname()\n'
	res += '  port: $l.port()\n'
	res += '  pathname: $l.pathname()\n'
	res += '  search: $l.search()\n'
	res += '  hash: $l.hash()\n'
	return res
}

pub fn (mut l Location) assign(url string) {
	#l.val.loc.assign(url.str)
}

pub fn (l Location) reload() {
	#l.loc.reload()
}

pub fn (mut l Location) replace(url string) {
	#l.val.loc.replace(url.str)
}

pub fn (l Location) origin() string {
	return tos(l.loc.origin)
}

pub fn (l Location) href() string {
	return tos(l.loc.href)
}

pub fn (mut l Location) set_href(href string) {
	l.loc.href = href.str
}

pub fn (l Location) protocol() string {
	return tos(l.loc.protocol)
}

pub fn (mut l Location) set_protocol(protocol string) {
	l.loc.protocol = protocol.str
}

pub fn (l Location) host() string {
	return tos(l.loc.host)
}

pub fn (mut l Location) set_host(host string) {
	l.loc.host = host.str
}

pub fn (l Location) hostname() string {
	return tos(l.loc.hostname)
}

pub fn (mut l Location) set_hostname(hostname string) {
	l.loc.hostname = hostname.str
}

pub fn (l Location) port() string {
	return tos(l.loc.port)
}

pub fn (mut l Location) set_port(port string) {
	l.loc.port = port.str
}

pub fn (l Location) pathname() string {
	return tos(l.loc.pathname)
}

pub fn (mut l Location) set_pathname(pathname string) {
	l.loc.pathname = pathname.str
}

pub fn (l Location) hash() string {
	return tos(l.loc.hash)
}

pub fn (mut l Location) set_hash(hash string) {
	l.loc.hash = hash.str
}

pub fn (l Location) search() string {
	return tos(l.loc.search)
}

pub fn (mut l Location) set_search(search string) {
	l.loc.search = search.str
}

pub struct JS.Location {
pub:
	origin JS.String
mut:
	href     JS.String
	protocol JS.String
	host     JS.String
	hostname JS.String
	port     JS.String
	pathname JS.String
	search   JS.String
	hash     JS.String
}

pub fn (doc Document) active_element() Element {
	mut elem := Element{}
	#elem.node = doc.node.activeElement;

	return elem
}

pub fn (doc Document) get(name string) ?Element {
	mut elem := Element{}
	#elem.node = doc.node[name.str];
	#console.log(elem.node)
	#if (elem.node === null || elem.node === undefined) return new $ref(new Option({state: new byte(2),err: none__}));

	return elem
}

// location returns URI of the document
pub fn (doc Document) location() Location {
	mut loc := Location{}
	#loc.loc = doc.node.location;

	return loc
}

// get_title returns current title of document
pub fn (doc Document) get_title() string {
	res := ''
	#res.str = doc.node.title;

	return res
}

// set_title updates document title
pub fn (doc Document) set_title(title string) {
	#doc.node.title = title.str;
}

// url returns document location as a string
pub fn (doc Document) url() string {
	res := ''
	#res.str = doc.node.URL;

	return res
}

// node casts `Document` back to `Node`.
pub fn (doc Document) node() Node {
	node := Node{}
	#node.node = doc.node

	return node
}

pub fn (doc Document) get_element_by_id(id string) ?IElement {
	mut elem := IElement(Element{})
	found := false
	#let tmp = doc.node.getElementById(id.str);
	#elem = jsdom__dispatch_event_target(tmp);
	#found.val = !(elem.node == null)
	if !found {
		return none
	}
	return elem
}

pub type DocumentPrepend = Node | string

pub fn (doc Document) prepend(nodes_or_strings ...DocumentPrepend) ? {
	caught := false
	err := ''
	#try {

	for elem in nodes_or_strings {
		match elem {
			string {
				#doc.node.prepend(elem.str)
			}
			Node {
				#doc.node.prepend(elem.node)
			}
		}
	}
	#} catch (e) { caught.val = true; err.str = e.toString();  }
	if caught {
		return error(err)
	}
}

pub fn (doc Document) create_element(tag_name string) Element {
	elem := Element{}
	#elem.node = doc.node.createElement(tag_name.str)

	return elem
}

pub fn get_document() Document {
	doc := Document{}
	#doc.node = document;

	return doc
}

pub fn (elem Document) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}
