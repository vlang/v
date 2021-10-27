module ctx

struct JS.CanvasRenderingContext2D {
mut:
	lineWidth                JS.Number
	lineCap                  JS.String
	lineJoin                 JS.String
	miterLimit               JS.Number
	lineDashOffset           JS.Number
	font                     JS.String
	textAlign                JS.String
	textBaseline             JS.String
	direction                JS.String
	fillStyle                voidptr
	strokeStyle              voidptr
	shadowBlur               JS.Number
	shadowColor              JS.String
	shadowOffsetX            JS.Number
	shadowOffsetY            JS.Number
	globalAlpha              JS.Number
	globalCompositeOperation JS.String
}

pub struct CanvasRenderingContext2D {
	ctx JS.CanvasRenderingContext2D [noinit]
}

pub type StrokeStyle = string

pub fn (ctx CanvasRenderingContext2D) begin_path() {
	#ctx.ctx.beginPath();
}

pub fn (ctx CanvasRenderingContext2D) set_stroke_style(style StrokeStyle) {
	#ctx.ctx.strokeStyle = style.str;
}

pub fn (ctx CanvasRenderingContext2D) line_width() int {
	res := 0
	#res.val = ctx.ctx.lineWidth

	return res
}

pub fn (ctx CanvasRenderingContext2D) set_line_width(width int) {
	#ctx.ctx.lineWidth = width.val
}

pub fn (ctx CanvasRenderingContext2D) move_to(x int, y int) {
	#ctx.ctx.moveTo(x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) line_to(x int, y int) {
	#ctx.ctx.lineTo(x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) stroke() {
	#ctx.ctx.stroke();
}

pub fn (ctx CanvasRenderingContext2D) close_path() {
	#ctx.ctx.closePath();
}

pub fn (ctx CanvasRenderingContext2D) clear_rect(x int, y int, width int, height int) {
	#ctx.ctx.clearRect(x.val,y.val,width.val,height.val);
}
