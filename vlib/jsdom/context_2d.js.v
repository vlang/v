module jsdom

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

pub enum LineJoin {
	bevel
	round
	miter
}

pub struct CanvasRenderingContext2D {
mut:
	ctx JS.CanvasRenderingContext2D [noinit]
}

pub type StrokeStyle = JS.CanvasGradient | string

pub fn (ctx CanvasRenderingContext2D) begin_path() {
	#ctx.ctx.beginPath();
}

pub fn (ctx CanvasRenderingContext2D) set_line_join(j LineJoin) {
	match j {
		.bevel {
			#ctx.ctx.lineJoin = 'bevel'.str
		}
		.round {
			#ctx.ctx.lineJoin = 'round'.str
		}
		.miter {
			#ctx.ctx.lineJoin = 'miter'.str
		}
	}
}

pub fn (ctx CanvasRenderingContext2D) set_stroke_style(style StrokeStyle) {
	#ctx.ctx.strokeStyle = style instanceof string ? style.str : style
}

pub fn (ctx CanvasRenderingContext2D) set_fill_style(style StrokeStyle) {
	#ctx.ctx.fillStyle = style instanceof string ? style.str : style
}

pub fn (ctx CanvasRenderingContext2D) line_width() f64 {
	res := 0
	#res.val = ctx.ctx.lineWidth

	return res
}

pub fn (ctx CanvasRenderingContext2D) font() string {
	res := ''
	#res.str = ctx.ctx.font;

	return res
}

pub fn (ctx CanvasRenderingContext2D) set_font(font string) {
	#ctx.ctx.font = font.str;
}

pub fn (ctx CanvasRenderingContext2D) fill_rect(x f64, y f64, width f64, height f64) {
	#ctx.ctx.fillRect(x.val,y.val,width.val,height.val)
}

pub fn (ctx CanvasRenderingContext2D) set_line_width(width f64) {
	#ctx.ctx.lineWidth = width.val
}

pub fn (ctx CanvasRenderingContext2D) move_to(x f64, y f64) {
	#ctx.ctx.moveTo(x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) line_to(x f64, y f64) {
	#ctx.ctx.lineTo(x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) stroke() {
	#ctx.ctx.stroke();
}

pub fn (ctx CanvasRenderingContext2D) close_path() {
	#ctx.ctx.closePath();
}

pub fn (ctx CanvasRenderingContext2D) stroke_rect(x f64, y f64, width f64, height f64) {
	#ctx.ctx.strokeRect(x.val,y.val,width.val,height.val);
}

pub fn (ctx CanvasRenderingContext2D) clear_rect(x f64, y f64, width f64, height f64) {
	#ctx.ctx.clearRect(x.val,y.val,width.val,height.val);
}

pub fn (ctx CanvasRenderingContext2D) create_linear_gradient(x0 f64, y0 f64, x1 f64, y1 f64) JS.CanvasGradient {
	return ctx.ctx.createLinearGradient(x0, y0, x1, y1)
}

pub fn (ctx CanvasRenderingContext2D) create_conic_gradient(start_angle f64, x f64, y f64) JS.CanvasGradient {
	return ctx.ctx.createConicGradient(start_angle, x, y)
}

pub fn (ctx CanvasRenderingContext2D) create_radial_gradient(x0 f64, y0 f64, r0 f64, x1 f64, y1 f64, r1 f64) JS.CanvasGradient {
	return ctx.ctx.createRadialGradient(x0, y0, r0, x1, y1, r1)
}

pub fn (ctx CanvasRenderingContext2D) bezier_curve_to(cp1x f64, cp1y f64, cp2x f64, cp2y f64, x f64, y f64) {
	#ctx.ctx.bezierCurveTo(cp1x.val,cp1y.val,cp2x.val,cp2y.val, x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) quadratic_curve_to(cpx f64, cpy f64, x f64, y f64) {
	#ctx.ctx.quadraticCurveTo(cpx.val,cpy.val,x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) arc(x f64, y f64, radius f64, start_angle f64, end_angle f64, counter_clockwise bool) {
	#ctx.ctx.arc(x.val,y.val,radius.val,start_angle.val,end_angle.val,counter_clockwise.val)
}

pub fn (ctx CanvasRenderingContext2D) arc_to(x1 f64, y1 f64, x2 f64, y2 f64, radius f64) {
	#ctx.ctx.arcTo(x1.val,y1.val,x2.val,y2.val,radius.val);
}

pub fn (ctx CanvasRenderingContext2D) ellipse(x f64, y f64, radius_x f64, radius_y f64, rotation f64, start_angle f64, end_angle f64, counter_clockwise bool) {
	#ctx.ctx.ellipse(x.val,y.val,radius_x.val,radius_y.val,rotation.val,start_angle.val,end_angle.val,counter_clockwise.val);
}

pub fn (ctx CanvasRenderingContext2D) rect(x f64, y f64, width f64, height f64) {
	#ctx.ctx.rect(x.val,y.val,widht.val,height.val);
}

pub fn (ctx CanvasRenderingContext2D) draw_focus_if_needed(el IElement) {
	#ctx.ctx.drawFocusIfNeeded(el.val.node);
}

fn (ctx JS.CanvasRenderingContext2D) createRadialGradient(x0 f64, y0 f64, r0 f64, x1 f64, y1 f64, r1 f64) JS.CanvasGradient

fn (ctx JS.CanvasRenderingContext2D) createConicGradient(startAngle f64, x f64, y f64) JS.CanvasGradient

fn (ctx JS.CanvasRenderingContext2D) createLinearGradient(x0 f64, y0 f64, x1 f64, y1 f64) JS.CanvasGradient

pub fn (gradient JS.CanvasGradient) addColorStop(x f64, color string)
