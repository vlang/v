module dom

pub struct CanvasRenderingContext2DSettings {
pub mut:
	alpha                bool
	color_space          string
	desynchronized       bool
	will_read_frequently bool
}

pub fn (settings CanvasRenderingContext2DSettings) to_js() JS.Any {
	mut object := JS.Any{}
	#object = { alpha: settings.alpha, colorSpace: settings.color_space.str, desynchronized: settings.desynchronized.val, willReadFrequently: settings.will_read_frequently.val };

	return object
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
	addEventListener(event JS.String, cb EventCallback, options JS.EventListenerOptions)
	dispatchEvent(event JS.Event) JS.Boolean
	removeEventListener(event JS.String, cb EventCallback, options JS.EventListenerOptions)
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

pub type WindowProxy = JS.Window

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
	doctype JS.DocumentType
	embeds JS.HTMLCollection
	forms JS.HTMLCollection
	getElementById(id JS.String) ?JS.HTMLElement
	adoptNode(node JS.Node) JS.Node
	close()
	createAttribute(name JS.String) JS.Attr
	createAttributeNS(namespace JS.String, qualifiedName JS.String) JS.Attr
	createCDATASection(data JS.String) JS.CDATASection
	createComment(data JS.String) JS.Comment
	createDocumentFragment() JS.DocumentFragment
	createElement(tagName JS.String) JS.HTMLElement
	createElementNS(namespaceURI JS.String, qualifiedName JS.String) JS.Element
	createEvent(event JS.String) JS.Event
	createTextNode(data JS.String) JS.Text
	elementFromPoint(x JS.Number, y JS.Number) ?JS.Element
	elementsFromPoint(x JS.Number, y JS.Number) JS.Array
	execCommand(commandId JS.String, showUI JS.Boolean, value JS.String) JS.Boolean
	hasFocus() JS.Boolean
	open(url JS.String, name JS.String, features JS.String) ?WindowProxy
	queryCommandEnabled(commandId JS.String) JS.Boolean
	queryCommandIndeterm(commandId JS.String) JS.Boolean
	queryCommandState(commandId JS.String) JS.String
	write(text ...JS.Any)
	writeln(text ...JS.Any)
	exitFullscreen() JS.Promise
	exitPictureInPicture() JS.Promise
	exitPointerLock()
	requestPointerLock()
	requestFullScreen() JS.Promise
mut:
	bgColor JS.String
	fgColor JS.String
	body JS.HTMLElement
	cookie JS.String
	domain JS.String
	designMode JS.String
	dir JS.String
	vlinkColor JS.String
}

pub fn document_url(doc JS.Document) JS.String {
	mut url := JS.String{}
	#url = doc.URL;

	return url
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
	getElementsByClassName(className JS.String) JS.HTMLCollection
	getElementsByTagName(qualifiedName JS.String) JS.HTMLCollection
	getEelementsByTagNameNS(namespaecURI JS.String, localName JS.String) JS.HTMLCollection
	hasAttribute(qualifiedName JS.String) JS.Boolean
	hasAttributeNS(namespace JS.String, localName JS.String) JS.Boolean
	hasAttributes() JS.Boolean
	hasPointerCapture(pointerId JS.Number) JS.Boolean
	matches(selectors JS.String) JS.Boolean
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

pub fn window() JS.Window {
	mut x := JS.Any(unsafe { voidptr(u64(0)) })
	#x = window;

	return x
}

fn init() {
	#js__dom__document = document;
}

pub type EventCallback = fn (JS.Event)

// event_listener returns proper listener callback. This function is useful when you need access to `this` value
// that is EventTarget. When you need access only to Event itself you can just use `fn (JS.Event)` as listener.
pub fn event_listener(callback fn (JS.EventTarget, JS.Event)) EventCallback {
	return fn [callback] (event JS.Event) {
		mut target := JS.EventTarget(unsafe { voidptr(u64(0)) })
		#target = this;
		callback(target, event)
	}
}

pub interface JS.HTMLCollection {
	length JS.Number
	item(idx JS.Number) ?JS.Any
	namedItem(name JS.String) ?JS.Any
}

pub interface JS.HTMLElement {
	JS.Element
	accessKeyLabel JS.String
	offsetHeight JS.Number
	offsetLeft JS.Number
	offsetParent JS.Any
	offsetTop JS.Number
	offsetWidth JS.Number
	click()
mut:
	accessKey JS.String
	autocapitalize JS.String
	dir JS.String
	draggable JS.Boolean
	hidden JS.Boolean
	innerText JS.String
	lang JS.String
	outerText JS.String
	spellcheck JS.Boolean
	title JS.String
	translate JS.Boolean
}

pub fn JS.HTMLElement.prototype.constructor() JS.HTMLElement

pub interface JS.HTMLEmbedElement {
	getSVGDocument() ?JS.Document
mut:
	align JS.String
	height JS.String
	src JS.String
	width JS.String
}

pub fn html_embed_type(embed JS.HTMLEmbedElement) JS.String {
	mut str := JS.String{}
	#str = embed.type

	return str
}

pub fn JS.HTMLEmbedElement.prototype.constructor() JS.HTMLEmbedElement

pub type CanvasContext = JS.CanvasRenderingContext2D
	| JS.WebGL2RenderingContext
	| JS.WebGLRenderingContext

pub interface JS.HTMLCanvasElement {
	JS.HTMLElement
	getContext(contextId JS.String, options JS.Any) ?CanvasContext
mut:
	height JS.Number
	width JS.Number
}

pub type FillStyle = JS.CanvasGradient | JS.CanvasPattern | JS.String

pub interface JS.CanvasRenderingContext2D {
	canvas JS.HTMLCanvasElement
	beginPath()
	clip(path JS.Path2D, fillRule JS.String)
	fill(path JS.Path2D, fillRule JS.String)
	isPointInPath(path JS.Path2D, x JS.Number, y JS.Number, fillRule JS.String) JS.Boolean
	isPointInStroke(path JS.Path2D, x JS.Number, y JS.Number) JS.Boolean
	stoke(path JS.Path2D)
	createLinearGradient(x0 JS.Number, y0 JS.Number, x1 JS.Number, y1 JS.Number) JS.CanvasGradient
	createRadialGradient(x0 JS.Number, y0 JS.Number, r0 JS.Number, x1 JS.Number, y1 JS.Number, r1 JS.Number) JS.CanvasGradient
	createPattern(image JS.CanvasImageSource, repetition JS.String) ?JS.CanvasPattern
	arc(x JS.Number, y JS.Number, radius JS.Number, startAngle JS.Number, endAngle JS.Number, counterclockwise JS.Boolean)
	arcTo(x1 JS.Number, y1 JS.Number, x2 JS.Number, y2 JS.Number, radius JS.Number)
	bezierCurveTo(cp1x JS.Number, cp1y JS.Number, cp2x JS.Number, cp2y JS.Number, x JS.Number, y JS.Number)
	closePath()
	ellipse(x JS.Number, y JS.Number, radiusX JS.Number, radiusY JS.Number, rotation JS.Number, startAngle JS.Number, endAngle JS.Number, counterclockwise JS.Boolean)
	lineTo(x JS.Number, y JS.Number)
	moveTo(x JS.Number, y JS.Number)
	quadraticCurveTo(cpx JS.Number, cpy JS.Number, x JS.Number, y JS.Number)
	rect(x JS.Number, y JS.Number, w JS.Number, h JS.Number)
	getLineDash() JS.Array
	setLineDash(segments JS.Array)
	clearRect(x JS.Number, y JS.Number, w JS.Number, h JS.Number)
	fillRect(x JS.Number, y JS.Number, w JS.Number, h JS.Number)
	strokeRect(x JS.Number, y JS.Number, w JS.Number, h JS.Number)
	getTransformt() JS.DOMMatrix
	resetTransform()
	rotate(angle JS.Number)
	scale(x JS.Number, y JS.Number)
	setTransform(matrix JS.DOMMatrix)
	transform(a JS.Number, b JS.Number, c JS.Number, d JS.Number, e JS.Number, f JS.Number)
	translate(x JS.Number, y JS.Number)
	drawFocusIfNeeded(path JS.Path2D, element JS.Element)
	stroke()
	fillText(text JS.String, x JS.Number, y JS.Number)
mut:
	lineCap JS.String
	lineDashOffset JS.Number
	lineJoin JS.String
	lineWidth JS.Number
	miterLimit JS.Number
	fillStyle FillStyle
	strokeStyle FillStyle
	globalAlpha JS.Number
	globalCompositeOperation JS.String
	font JS.String
}

pub interface JS.CanvasGradient {
	addColorStop(offset JS.Number, color JS.String)
}

pub interface JS.CanvasPattern {
	setTransform(transform JS.DOMMatrix)
}

pub type OnDeviceMotion = fn (ev JS.DeviceMotionEvent) JS.Any

pub type OnDeviceOrientation = fn (ev JS.DeviceOrientationEvent) JS.Any

pub fn on_device_motion(cb fn (win JS.Window, ev JS.DeviceMotionEvent) JS.Any) OnDeviceMotion {
	clos := fn [cb] (ev JS.DeviceMotionEvent) JS.Any {
		mut win := JS.Any(unsafe { voidptr(u64(0)) })
		#win = this;

		return cb(win, ev)
	}
	return clos
}

pub fn on_device_orientation(cb fn (win JS.Window, ev JS.DeviceOrientationEvent) JS.Any) OnDeviceOrientation {
	clos := fn [cb] (ev JS.DeviceOrientationEvent) JS.Any {
		mut win := JS.Any(unsafe { voidptr(u64(0)) })
		#win = this;

		return cb(win, ev)
	}
	return clos
}

pub type AnimationFrameCallback = fn (JS.Number)

pub interface JS.Window {
	JS.EventTarget
	closed JS.Boolean
	devicePixelRatio JS.Number
	document JS.Document
	frameElement JS.Element
	innerHeight JS.Number
	innerWidth JS.Number
	length JS.Number
	outerHeight JS.Number
	outerWidth JS.Number
	screenLeft JS.Number
	screenTop JS.Number
	screenX JS.Number
	screenY JS.Number
	scrollX JS.Number
	scrollY JS.Number
	alert(message JS.Any)
	blur()
	cancelIdleCallback(handle JS.Number)
	captureEvents()
	close()
	confirm(message JS.String) JS.Boolean
	focus()
	moveBy(x JS.Number, y JS.Number)
	moveTo(x JS.Number, y JS.Number)
	print()
	prompt(message JS.String, default_ JS.String) ?JS.String
	stop()
	resizeBy(x JS.Number, y JS.Number)
	resizeTo(width JS.Number, height JS.Number)
	scroll(x JS.Number, y JS.Number)
	scrollBy(x JS.Number, y JS.Number)
	scrollTo(x JS.Number, y JS.Number)
	requestAnimationFrame(callback AnimationFrameCallback)
mut:
	name string
	opener JS.Any
	ondevicemotion OnDeviceMotion
	ondeviceorientation OnDeviceOrientation
}

pub interface JS.Path2D {}

pub struct JS.DeviceMotionEventAcceleration {
	x JS.Number
	y JS.Number
	z JS.Number
}

pub struct JS.DeviceMotionEventRotationRate {
	alpha JS.Number
	beta  JS.Number
	gamma JS.Number
}

pub interface JS.DeviceMotionEvent {
	JS.Event
	interval JS.Number
	acceleration JS.DeviceMotionEventAcceleration
	accelerationIncludingGravity JS.DeviceMotionEventAcceleration
	rotationRate JS.DeviceMotionEventRotationRate
}

pub interface JS.DeviceOrientationEvent {
	JS.Event
	absolute JS.Boolean
	alpha JS.Number
	beta JS.Number
	gamma JS.Number
}

pub interface JS.DocumentType {
	JS.Node
	JS.ChildNode
	name JS.String
	ownerDocument JS.Document
	publicId JS.String
	systemId JS.String
}

[single_impl]
pub interface JS.WebGLProgram {}

[single_impl]
pub interface JS.WebGLShader {}

[single_impl]
pub interface JS.WebGLBuffer {}

[single_impl]
pub interface JS.WebGLFramebuffer {}

[single_impl]
pub interface JS.WebGLRenderbuffer {}

[single_impl]
pub interface JS.WebGLTexture {}

[single_impl]
pub interface JS.WebGLUniformLocation {}

[single_impl]
pub interface JS.WebGLVertexArrayObject {}

pub interface JS.WebGLRenderingContext {
	canvas JS.HTMLCanvasElement
	drawingBufferHeight JS.Number
	drawingBufferWidth JS.Number
	activeTexture(texture JS.Number)
	attachShader(program JS.WebGLProgram, shader JS.WebGLProgram)
	linkProgram(program JS.WebGLProgram)
	bindAttribLocation(program JS.WebGLProgram, index JS.Number, name JS.String)
	bindBuffer(target JS.Number, buffer JS.WebGLBuffer)
	bindFramebuffer(target JS.Number, buffer JS.WebGLFrameBuffer)
	bindRenderbuffer(target JS.Number, renderbuffer JS.WebGLRenderbuffer)
	bindTexture(target JS.Number, texture JS.WebGLTexture)
	clear(mask JS.Number)
	clearColor(red JS.Number, green JS.Number, blue JS.Number, alpha JS.Number)
	clearDepth(depth JS.Number)
	clearStencil(s JS.Number)
	colorMask(red JS.Boolean, green JS.Boolean, blue JS.Boolean, alpha JS.Boolean)
	compileShader(shader JS.WebGLShader)
	createBuffer() ?JS.WebGLBuffer
	createFramebuffer() ?JS.WebGLFrameBuffer
	createProgram() ?JS.WebGLProgram
	createRenderbuffer() ?JS.WebGLRenderbuffer
	createShader(typ JS.Number) ?JS.WebGLShader
	createTexture() ?JS.WebGLTexture
	cullFace(mode JS.Number)
	deleteBuffer(buffer JS.WebGLBuffer)
	deleteFramebuffer(buffer JS.WebGLFrameBuffer)
	deleteProgram(program JS.WebGLProgram)
	deleteRenderbuffer(buffer JS.WebGLRenderbuffer)
	deleteShader(shader JS.WebGLShader)
	deleteTexture(texture JS.WebGLTexture)
	depthFunc(func JS.Number)
	depthMask(flag JS.Boolean)
	depthRange(zNear JS.Number, zFar JS.Number)
	detachShader(program JS.WebGLProgram, shader JS.WebGLShader)
	disable(cap JS.Number)
	disableVertexAttribArray(index JS.Number)
	drawArrays(mode JS.Number, first JS.Number, count JS.Number)
	drawElements(mode JS.Number, count JS.Number, typ JS.Number, offset JS.Number)
	enable(cap JS.Number)
	enableVertexAttribArray(index JS.Number)
	finish()
	flush()
	framebufferRenderbuffer(target JS.Number, attachment JS.Number, renderbuffertarget JS.Number, renderbuffer JS.WebGLRenderbuffer)
	framebufferTexture2D(target JS.Number, attachment JS.Number, textarget JS.Number, texture JS.WebGLTexture, level JS.Number)
	frontFace(mode JS.Number)
	generateMipmap(target JS.Number)
	getError() JS.Number
	getExtension(name JS.String) JS.Any
	getParameter(name JS.Number) JS.Any
	getProgramParameter(program JS.WebGLProgram, pname JS.Number) JS.Any
	getShaderSource(shader JS.WebGLShader) ?JS.String
	bufferData(target JS.Number, data JS.TypedArray, usage JS.Number)
	shaderSource(shader JS.WebGLShader, source JS.String)
	getShaderParameter(shader JS.WebGLShader, pname JS.Number) JS.Any
	vertexAttribPointer(index JS.Number, size JS.Number, typ JS.Number, normalized JS.Boolean, stride JS.Number, offset JS.Number)
	getAttribLocation(program JS.WebGLProgram, name JS.String) JS.Number
	useProgram(program JS.WebGLProgram)
	getUniformLocation(program JS.WebGLProgram, name JS.String) ?JS.WebGLUniformLocation
	uniformMatrix2fv(location JS.WebGLUniformLocation, transpose JS.Boolean, value JS.Array)
	uniformMatrix3fv(location JS.WebGLUniformLocation, transpose JS.Boolean, value JS.Array)
	uniformMatrix4fv(location JS.WebGLUniformLocation, transpose JS.Boolean, value JS.Array)
	getProgramInfoLog(program JS.WebGLProgram) JS.String
	getShaderInfoLog(shader JS.WebGLShader) JS.String
	viewport(x JS.Number, y JS.Number, width JS.Number, height JS.Number)
	scissor(x JS.Number, y JS.Number, width JS.Number, height JS.Number)
	stencilFunc(func JS.Number, ref JS.Number, mask JS.Number)
	stencilFuncSeparate(face JS.Number, func JS.Number, ref JS.Number, mask JS.Number)
	stencilMask(mask JS.Number)
	stencilMaskSeparate(face JS.Number, mask JS.Number)
	stencilOp(fail JS.Number, zfail JS.Number, zpass JS.Number)
	stencilOpSeparate(face JS.Number, fail JS.Number, zfail JS.Number, zpass JS.Number)
	texParameterf(target JS.Number, pname JS.Number, param JS.Number)
	texParameteri(target JS.Number, pname JS.Number, param JS.Number)
	uniform1f(location JS.WebGLUniformLocation, x JS.Number)
	uniform1i(location JS.WebGLUniformLocation, x JS.Number)
	uniform2f(location JS.WebGLUniformLocation, x JS.Number, y JS.Number)
	uniform2i(location JS.WebGLUniformLocation, x JS.Number, y JS.Number)
	uniform3f(location JS.WebGLUniformLocation, x JS.Number, y JS.Number, z JS.Number)
	uniform3i(location JS.WebGLUniformLocation, x JS.Number, y JS.Number, z JS.Number)
	uniform4f(location JS.WebGLUniformLocation, x JS.Number, y JS.Number, z JS.Number, w JS.Number)
	uniform4i(location JS.WebGLUniformLocation, x JS.Number, y JS.Number, z JS.Number, w JS.Number)
	validateProgram(program JS.WebGLProgram)
	vertexAttrib1f(index JS.Number, x JS.Number)
	vertexAttrib1fv(index JS.Number, values JS.Array)
	vertexAttrib2f(index JS.Number, x JS.Number, y JS.Number)
	vertexAttrib2fv(index JS.Number, x JS.Number, y JS.Number, values JS.Array)
	vertexAttrib3f(index JS.Number, x JS.Number, y JS.Number, z JS.Number)
	vertexAttrib3fv(index JS.Number, x JS.Number, y JS.Number, z JS.Number, values JS.Array)
	vertexAttrib4f(index JS.Number, x JS.Number, y JS.Number, z JS.Number, w JS.Number)
	vertexAttrib4fv(index JS.Number, x JS.Number, y JS.Number, z JS.Number, w JS.Number, values JS.Array)
	bufferSubData(target JS.Number, offset JS.Number, data JS.TypedArray)
	compressedTexImage2D(target JS.Number, level JS.Number, internalformat JS.Number, width JS.Number, height JS.Number, border JS.Number, data JS.TypedArray)
	compressedTexSubImage2D(target JS.Number, level JS.Number, xoffset JS.Number, yoffset JS.Number, width JS.Number, height JS.Number, format JS.Number, data JS.TypedArray)
	readPixels(x JS.Number, y JS.Number, width JS.Number, height JS.Number, format JS.Number, typ JS.Number, border JS.Number, pixels JS.TypedArray)
	texImage2D(target JS.Number, level JS.Number, internalformat JS.Number, format JS.Number, source JS.Node)
}

pub interface JS.WebGL2RenderingContext {
	JS.WebGLRenderingContext
}

pub fn gl_vertex_shader() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.VERTEX_SHADER;

	return num
}

pub fn gl_fragment_shader() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.FRAGMENT_SHADER;

	return num
}

pub fn gl_element_array_buffer() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.ELEMENT_ARRAY_BUFFER;

	return num
}

pub fn gl_array_buffer() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.ARRAY_BUFFER;

	return num
}

pub fn gl_color_buffer_bit() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.COLOR_BUFFER_BIT;

	return num
}

pub fn gl_depth_buffer_bit() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.COLOR_BUFFER_BIT;

	return num
}

pub fn gl_triangles() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.TRIANGLES;

	return num
}

pub fn gl_unsigned_short() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.UNSIGNED_SHORT;

	return num
}

pub fn gl_static_draw() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.STATIC_DRAW;

	return num
}

pub fn gl_link_status() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.LINK_STATUS;

	return num
}

pub fn gl_compile_status() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.COMPILE_STATUS;

	return num
}

pub fn gl_float() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.FLOAT;

	return num
}

pub fn gl_depth_test() JS.Number {
	mut num := JS.Number{}
	#num = WebGLRenderingContext.DEPTH_TEST;

	return num
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
	mut ev := JS.Event(unsafe { voidptr(u64(0)) })
	#ev = new Event(typ.str,bubbles.val,cancelable.val,composed.val);

	return ev
}

pub interface JS.ErrorEvent {
	JS.Event
	colno JS.Number
	error JS.Number
	filename JS.Number
	lineno JS.Number
	message JS.String
}

[use_new]
pub fn JS.ErrorEvent.prototype.constructor(typ JS.String) JS.ErrorEvent

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

pub interface JS.WheelEvent {
	JS.MouseEvent
	deltaX JS.Number
	deltaY JS.Number
	deltaZ JS.Number
	deltaMode JS.Number
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

pub interface JS.Gamepad {
	axes JS.Array // Array<number>
	buttons JS.Array // Array<GamepadButton>
	connected JS.Boolean
	hapticActuators JS.Array // Array<GamepadHapticActuator>
	id JS.String
	index JS.Number
	mapping JS.String
	timestamp JS.Number
}

[single_impl]
pub interface JS.GamepadButton {
	pressed JS.Boolean
	touched JS.Boolean
	value JS.Number
}

[single_impl]
pub interface JS.GamepadHapticActuator {
}

pub interface JS.GamepadEvent {
	JS.Event
	gamepad JS.Gamepad
}

pub interface JS.HashChangeEvent {
	JS.Event
	newURL JS.String
	oldURL JS.String
}

pub interface JS.MessageEvent {
	JS.Event
	data JS.Any
	lastEventId JS.String
	origin JS.String
	ports JS.Array
	source JS.Any
}

pub interface JS.MessagePort {
	JS.EventTarget
	close()
	portMessage(message JS.Any, transfer JS.Array)
	start()
}

pub interface JS.PageTransitionEvent {
	JS.Event
	persisted JS.Boolean
}

pub interface JS.PopStateEvent {
	JS.Event
	state JS.Any
}

pub interface JS.ProgressEvent {
	lenghtComputable JS.Boolean
	loaded JS.Number
	target JS.Any
	total JS.Number
}

pub interface JS.KeyboardEvent {
	JS.UIEvent
	altKey JS.Boolean
	code JS.String
	ctrlKey JS.Boolean
	isComposing JS.Boolean
	key JS.String
	location JS.Number
	metaKey JS.Boolean
	repeat JS.Boolean
	shiftKey JS.Boolean
}
