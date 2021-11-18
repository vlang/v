module jsdom

pub struct CanvasRenderingContext2DSettings {
pub mut:
	alpha                bool
	color_space          string
	desynchronized       bool
	will_read_frequently bool
}

pub interface JS.DOMMatrix2DInit {
mut:
	a JS.Number
	b JS.Number
	c JS.Number
	d JS.Number
	e JS.Number
	f JS.Number
	m11 JS.Number
	m12 JS.Number
	m21 JS.Number
	m22 JS.Number
	m41 JS.Number
	m42 JS.Number
}

pub interface JS.DOMMatrixInit {
	JS.DOMMatrix2DInit
	is2D JS.Boolean
mut:
	m13 JS.Number
	m14 JS.Number
	m23 JS.Number
	m24 JS.Number
	m31 JS.Number
	m32 JS.Number
	m33 JS.Number
	m34 JS.Number
	m43 JS.Number
	m44 JS.Number
}

pub interface JS.DOMMatrix {
	is2D JS.Boolean
	isIdentity JS.Boolean
	flipX() JS.DOMMatrix
	flipY() JS.DOMMatrix
	inverse() JS.DOMMatrix
	multiply(other JS.DOMMatrix) JS.DOMMatrix
	rotate(rotX JS.Number, rotY JS.Number, rotZ JS.Number) JS.DOMMatrix
	rotateAxisAngle(x JS.Number, y JS.Number, z JS.Number, angle JS.Number) JS.DOMMatrix
	scale(scaleX JS.Number, scaleY JS.Number, scaleZ JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	scale3d(scale JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	skewX(sx JS.Number) JS.DOMMatrix
	skewY(sy JS.Number) JS.DOMMatrix
	translate(tx JS.Number, ty JS.Number, tz JS.Number) JS.DOMMatrix
	invertSelf() JS.DOMMatrix
	multiplySelf(other JS.DOMMatrix) JS.DOMMatrix
	preMultiplySelf(other JS.DOMMatrix) JS.DOMMatrix
	rotateAxisAngleSelf(x JS.Number, y JS.Number, z JS.Number, angle JS.Number) JS.DOMMatrix
	rotateFromVectorSelf(x JS.Number, y JS.Number) JS.DOMMatrix
	rotateSelf(rotX JS.Number, rotY JS.Number, rotZ JS.Number) JS.DOMMatrix
	scale3dSelf(scale JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	scaleSelf(scaleX JS.Number, scaleY JS.Number, scaleZ JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	toString() JS.String
mut:
	a JS.Number
	b JS.Number
	c JS.Number
	d JS.Number
	e JS.Number
	f JS.Number
	m11 JS.Number
	m12 JS.Number
	m13 JS.Number
	m14 JS.Number
	m21 JS.Number
	m22 JS.Number
	m23 JS.Number
	m24 JS.Number
	m31 JS.Number
	m32 JS.Number
	m33 JS.Number
	m34 JS.Number
	m41 JS.Number
	m42 JS.Number
	m43 JS.Number
	m44 JS.Number
}

pub type SVGMatrix = JS.DOMMatrix
pub type WebKitCSSMatrix = JS.DOMMatrix

[use_new]
pub fn JS.DOMMatrix.prototype.constructor(init JS.Array) JS.DOMMatrix

pub interface JS.DOMPoint {
	matrixTransform(matrix JS.DOMMatrix) JS.DOMPoint
mut:
	w JS.Number
	x JS.Number
	y JS.Number
	z JS.Number
}

[use_new]
pub fn JS.DOMPoint.prototype.constructor(x JS.Number, y JS.Number, z JS.Number, w JS.Number) JS.DOMPoint

pub interface JS.DOMQuad {
	p1 JS.DOMPoint
	p2 JS.DOMPoint
	p3 JS.DOMPoint
	p4 JS.DOMPoint
	getBounds() JS.DOMRect
}

[use_new]
pub fn JS.DOMQuad.prototype.constructor(p1 JS.DOMPoint, p2 JS.DOMPoint, p3 JS.DOMPoint, p4 JS.DOMPoint) JS.DOMQuad
pub fn JS.DOMQuad.fromQuad(other JS.DOMQuad) JS.DOMQuad
pub fn JS.DOMQuad.fromRect(other JS.DOMRect) JS.DOMRect

pub interface JS.DOMRect {
	bottom JS.Number
	left JS.Number
	right JS.Number
	top JS.Number
mut:
	height JS.Number
	width JS.Number
	x JS.Number
	y JS.Number
}

[use_new]
pub fn JS.DOMRect.prototype.constructor(x JS.Number, y JS.Number, width JS.Number, height JS.Number) JS.DOMRect

pub interface JS.DOMStringList {
	length JS.Number
	contains(JS.String) JS.Boolean
	item(index JS.Number) ?JS.String
}

pub interface JS.DOMRectList {
	length JS.Number
	contains(JS.String) JS.Boolean
	item(index JS.Number) ?JS.Rect
}

pub type DOMTokenListForEachCb = fn (JS.String, JS.Number, JS.DOMTokenList)

pub interface JS.DOMTokenList {
	length JS.Number
	toString() JS.String
	add(tokens ...JS.Any) ?JS.Any
	contains(token JS.String) JS.Boolean
	item(index JS.Number) ?JS.String
	remove(tokens ...JS.Any) ?JS.Any
	replace(token JS.String, newToken JS.String) JS.Boolean
	supports(token JS.String) JS.Boolean
	toggle(token JS.String, force JS.Boolean) JS.Boolean
	forEach(cb DOMTokenListForEachCb, thisArg JS.Any)
mut:
	value JS.String
}

pub struct JS.EventListenerOptions {
	capture bool
}

pub interface JS.EventTarget {
	addEventListener(cb EventCallback, options JS.EventListenerOptions)
	dispatchEvent(event JS.Event) JS.Boolean
	removeEventListener(cb EventCallback, options JS.EventListenerOptions)
}

// Event is an event which takes place in the DOM.
pub interface JS.Event {
	JS.EventTarget
	bubbles JS.Boolean
	cancelable JS.Boolean
	composed JS.Boolean
	currentTarget JS.EventTarget
	defaultPrevented JS.Boolean
	eventPhase JS.Number
	isTrusted JS.Boolean
	srcElement JS.EventTarget
	timeStamp JS.DOMHighResTimeStamp // composedPath returns the invocation target objects of event's path.
	composedPath() JS.Array
	initEvent(typ JS.String, bubbles JS.Boolean, cancelable JS.Boolean)
	preventDefault()
	stopImmediatePropagation()
	stopPropagation()
mut:
	returnValue JS.Boolean
}

pub fn event_type(ev JS.Event) string {
	res := ''
	#res.str = ev.type;

	return res
}

pub fn create_event(typ string, bubbles bool, cancelable bool, composed bool) JS.Event {
	mut ev := JS.Event(voidptr(0))
	#ev = new Event(typ.str,bubbles.val,cancelable.val,composed.val);

	return ev
}

pub interface JS.UIEvent {
	JS.Event
	detail JS.Number
	view JS.Any
}

[use_new]
pub fn JS.UIEvent.prototype.constructor(typ JS.String, dict JS.UIEventDict) JS.UIEvent

pub struct JS.EventInit {
	bubbles    JS.Boolean
	cancelable JS.Boolean
	composed   JS.Boolean
}

pub struct JS.UIEventInitDict {
	bubbles    JS.Boolean
	cancelable JS.Boolean
	composed   JS.Boolean
	detail     JS.Number
	view       JS.Any
	which      JS.Number
}

pub interface JS.MouseEvent {
	JS.UIEvent
	altKey JS.Boolean
	button JS.Number
	buttons JS.Number
	clientX JS.Number
	clientY JS.Number
	ctrlKey JS.Number
	metaKey JS.Number
	movementX JS.Number
	movementY JS.Number
	offsetX JS.Number
	offsetY JS.Number
	pageX JS.Number
	pageY JS.Number
	relatedTarget JS.Any
	screenX JS.Number
	screenY JS.Number
	shiftKey JS.Boolean
	x JS.Number
	y JS.Number
	getModifierState(keyArg JS.String) JS.Boolean
}

pub interface JS.Node {
	JS.EventTarget
	baseURI JS.String
	childNodes JS.Any
	firstChild JS.ChildNode
	isConnected JS.Boolean
	lastChild JS.ChildNode
	nextSibling JS.ChildNode
	nodeName JS.String
	nodeType JS.Number
	ownerDocument JS.Document
	parentElement JS.HTMLElement
	parentNode JS.ParentNode
	previousSibling JS.ChildNode
	appendChild(node JS.Node) JS.Node
	cloneNode(deep JS.Boolean) JS.Node
	compareDocumentPosition(other JS.Node) JS.Number
	contains(other JS.Node) JS.Boolean
	getRootNode(composed JS.Boolean) JS.Node
	hasChildNodes() JS.Boolean
	insertBefore(node JS.Node, child JS.Node) JS.Node
	isEqualNode(otherNode JS.Node) JS.Boolean
	isSameNode(otherNode JS.Node) JS.Boolean
	lookupPrefix(namespace JS.String) JS.String
	normalize()
	removeChild(child JS.Node) JS.Node
	replaceChild(node JS.Node, child JS.Node) JS.Npde
mut:
	nodeValue JS.String
	textContent JS.String
}

pub interface JS.ChildNode {
	JS.Node
	after(nodes ...JS.Any)
	before(nodes ...JS.Any)
	remove()
	replaceWith(nodes ...JS.Any)
}

pub interface JS.ParentNode {
	JS.Node
	childElementCount JS.Number
	children JS.HTMLCollection
}

pub interface JS.Document {
	JS.Node
	all JS.HTMLAllCollection
	anchros JS.HTMLCollection
	applets JS.HTMLCollection
	characterSet JS.String
	charset JS.String
	compatMode JS.String
	contentType JS.String
	documentURI JS.String
	documentElement JS.HTMLElement
	hidden JS.Boolean
	head JS.HTMLHeadElement
	fullscreenEnabled JS.Boolean
	fullscreen JS.Boolean
	lastModified JS.String
	inputEncoding JS.String
	implementation JS.DOMImplementation
mut:
	bgColor JS.String
	body JS.HTMLElement
	cookie JS.String
	domain JS.String
}

pub interface JS.PointerEvent {
	JS.MouseEvent
	height JS.Number
	isPrimary JS.Boolean
	pointerId JS.Number
	pointerType JS.String
	pressure JS.Number
	tangentialPressure JS.Number
	tiltX JS.Number
	tiltY JS.Number
	twist JS.Number
	width JS.Number
	getCoalescedEvents() JS.Array
	getPredictedEvents() JS.Array
}

pub interface JS.Element {
	JS.Node
	classList JS.DOMTokenList
	clientHeight JS.Number
	clientLeft JS.Number
	clientTop JS.Number
	clientWidth JS.Number
	localName JS.String
	namespaceURI JS.String
	ownerDocument JS.Document
	part JS.DOMTokenList
	prefix JS.String
	scrollHeight JS.Number
	scrollWidth JS.Number
	tagName JS.String
	closest(selector JS.String) ?JS.Element
	getAttribute(qualifiedName JS.String) ?JS.String
	getAttributeNS(namespace JS.String, localName JS.String) ?JS.String
	getAttributeNames() JS.Array
	getClientRects() JS.DOMRectList
	getBoundingClientRect() JS.DOMRect
	scrollTo(x JS.Number, y JS.Number)
	scroll(x JS.Number, y JS.Number)
	scrollBy(x JS.Number, y JS.Number)
	toggleAttribute(qualifiedName JS.String, force JS.Boolean) JS.Boolean
mut:
	className JS.String
	id JS.String
	onfullscreenchange fn (this JS.Element, ev JS.Event) JS.Any
	onfullscreenerror fn (this JS.Element, ev JS.Event) JS.Any
	outerHTML JS.String
	scrollLeft JS.Number
	scrollTop JS.Number
	slot JS.String
}

pub const (
	document = JS.Document{}
)

fn init() {
	#jsdom__document = document;
}

pub type EventCallback = fn (JS.Event)

// event_listener returns proper listener callback. This function is useful when you need access to `this` value
// that is EventTarget. When you need access only to Event itself you can just use `fn (JS.Event)` as listener.
pub fn event_listener(callback fn (JS.EventTarget, JS.Event)) EventCallback {
	return fn [callback] (event JS.Event) {
		mut target := JS.EventTarget(voidptr(0))
		#target = this;
		callback(target, event)
	}
}
