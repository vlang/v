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

pub enum TextAlign {
	left
	right
	center
	start
	end
}

pub struct CanvasRenderingContext2D {
mut:
	ctx JS.CanvasRenderingContext2D [noinit]
}

pub type StrokeStyle = JS.CanvasGradient | string

pub fn (ctx CanvasRenderingContext2D) begin_path() {
	#ctx.ctx.beginPath();
}

pub fn (ctx CanvasRenderingContext2D) set_line_dash(arr []f64) {
	#let tmp = []

	for x in arr {
		#tmp.push(x.val);

		_ := x
	}
	#ctx.ctx.setLineDash(tmp);
}

pub fn (ctx CanvasRenderingContext2D) get_line_dash() []f64 {
	arr := []f64{}
	#for (elem of ctx.ctx.getLineDash()) { array_push(arr,elem); }

	return arr
}

pub fn (ctx CanvasRenderingContext2D) set_text_align(align TextAlign) {
	match align {
		.left {
			#ctx.ctx.textAlign = 'start';
		}
		.right {
			#ctx.ctx.textAlign = 'right';
		}
		.center {
			#ctx.ctx.textAlign = 'center';
		}
		.start {
			#ctx.ctx.textAlign = 'start';
		}
		.end {
			#ctx.ctx.textAlign = 'end';
		}
	}
}

pub fn (ctx CanvasRenderingContext2D) set_line_join(j LineJoin) {
	match j {
		.bevel {
			#ctx.ctx.lineJoin = 'bevel'
		}
		.round {
			#ctx.ctx.lineJoin = 'round'
		}
		.miter {
			#ctx.ctx.lineJoin = 'miter'
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

pub fn (ctx CanvasRenderingContext2D) draw_focus_if_needed_wpath(path Path2D, el IElement) {
	#ctx.ctx.drawFocusIfNeeded(path.path, el.val.node);
}

fn (ctx JS.CanvasRenderingContext2D) createRadialGradient(x0 f64, y0 f64, r0 f64, x1 f64, y1 f64, r1 f64) JS.CanvasGradient

fn (ctx JS.CanvasRenderingContext2D) createConicGradient(startAngle f64, x f64, y f64) JS.CanvasGradient

fn (ctx JS.CanvasRenderingContext2D) createLinearGradient(x0 f64, y0 f64, x1 f64, y1 f64) JS.CanvasGradient

pub fn (gradient JS.CanvasGradient) addColorStop(x f64, color string)

/// fill_text draws a text string at the specified coordinates. `max_width` allows specifying a maximum
/// width for the rendered text, set max_width to -1.0 to automatically adjust text width.
pub fn (ctx CanvasRenderingContext2D) fill_text(text string, x f64, y f64, max_width f64) {
	if max_width == -1.0 {
		#ctx.ctx.fillText(text.str,x.val,y.val,max_width.val)
	} else {
		#ctx.ctx.fillText(text.str,x.val,y.val)
	}
}

/// stoke_text strokes — that is, draws the outlines of — the characters of a text string at the specified coordinates.
///  `max_width` allows specifying a maximum width for the rendered text, set max_width to -1.0 to automatically adjust text width.
pub fn (ctx CanvasRenderingContext2D) stroke_text(text string, x f64, y f64, max_width f64) {
	if max_width == -1.0 {
		#ctx.ctx.stokeText(text.str,x.val,y.val,max_width.val)
	} else {
		#ctx.ctx.stokeText(text.str,x.val,y.val)
	}
}

pub fn (ctx CanvasRenderingContext2D) measure_text(text string) TextMetrics {
	metrics := TextMetrics{}
	#let tmp = ctx.ctx.measureText(text.str);
	#metrics.width = new f64(tmp.width);
	#metrics.actual_bounding_box_left = new f64(tmp.actualBoundingBoxLeft);
	#metrics.actual_bounding_box_right = new f64(tmp.actualBoundingBoxRight);
	#metrics.actual_bounding_box_ascent = new f64(tmp.actualBoundingBoxAscent);
	#metrics.actual_bounding_box_descent = new f64(tmp.actualBoundingBoxDescent);
	#metrics.font_bounding_box_ascent = new f64(tmp.fontBoundingBoxAscent);
	#metrics.font_bounding_box_descent = new f64(tmp.fontBoundingBoxDescent);
	#metrics.em_height_ascent = new f64(tmp.emHeightAscent);
	#metrics.em_height_descent = new f64(tmp.emHeightDescent);
	#metrics.hanging_baseline = new f64(tmp.hangingBaseline);
	#metrics.alphabetic_baseline = new f64(tmp.alphabeticBaseline);
	#metrics.ideographic_baseline = new f64(tmp.ideographicBaseline);

	return metrics
}

pub fn (ctx CanvasRenderingContext2D) fill_path(p Path2D) {
	#ctx.ctx.fill(p.path);
}

pub enum FillRule {
	nonzero
	evenodd
}

pub fn (ctx CanvasRenderingContext2D) clip_rule(rule FillRule) {
	match rule {
		.nonzero {
			#ctx.ctx.clip('nonzero')
		}
		.evenodd {
			#ctx.ctx.clip('evenodd')
		}
	}
}

pub fn (ctx CanvasRenderingContext2D) clip() {
	#ctx.ctx.clip();
}

pub fn (ctx CanvasRenderingContext2D) clip_path(path Path2D) {
	#ctx.ctx.clip(path.path);
}

pub fn (ctx CanvasRenderingContext2D) clip_path_rule(path Path2D, rule FillRule) {
	match rule {
		.nonzero {
			#ctx.ctx.clip(path.path, 'nonzero')
		}
		.evenodd {
			#ctx.ctx.clip(path.path,'evenodd')
		}
	}
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_path(x f64, y f64) bool {
	res := false
	#res.val = ctx.ctx.isPointInPath(x.val,y.val);

	return res
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_path_2(path Path2D, x f64, y f64) bool {
	res := false
	#res.val = ctx.ctx.isPointInPath(path.path,x.val,y.val);

	return res
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_path_3(path Path2D, x f64, y f64, rule FillRule) bool {
	res := false
	match rule {
		.nonzero {
			#res.val = ctx.ctx.isPointInPath(path.path,x.val,y.val,'nonzero');
		}
		.evenodd {
			#res.val = ctx.ctx.isPointInPath(path.path,x.val,y.val,'evenadd');
		}
	}
	return res
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_path_4(x f64, y f64, rule FillRule) bool {
	res := false
	match rule {
		.nonzero {
			#res.val = ctx.ctx.isPointInPath(x.val,y.val,'nonzero');
		}
		.evenodd {
			#res.val = ctx.ctx.isPointInPath(x.val,y.val,'evenadd');
		}
	}
	return res
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_stroke(x f64, y f64) bool {
	res := false
	#res.val = ctx.ctx.isPointInStroke(x.val,y.val);

	return res
}

pub fn (ctx CanvasRenderingContext2D) is_point_in_stroke_path(path Path2D, x f64, y f64) bool {
	res := false
	#res.val = ctx.ctx.isPointInStroke(path.path, x.val,y.val);

	return res
}

pub fn (ctx CanvasRenderingContext2D) get_transform() DOMMatrix {
	m := DOMMatrix{}
	#m.matrix = ctx.ctx.getTransform();

	return m
}

pub fn (ctx CanvasRenderingContext2D) rotate(angle f64) {
	#ctx.ctx.rotate(angle.val);
}

pub fn (ctx CanvasRenderingContext2D) scale(x f64, y f64) {
	#ctx.ctx.scale(x.val,y.val);
}

pub fn (ctx CanvasRenderingContext2D) translate(x f64, y f64) {
	#ctx.ctx.translate(x.val,y.val)
}

pub fn (ctx CanvasRenderingContext2D) transform(a f64, b f64, c f64, d f64, e f64, f f64) {
	#ctx.ctx.transform(a.val,b.val,c.val,d.val,e.val,f.val);
}

pub fn (ctx CanvasRenderingContext2D) set_transform_matrix(m DOMMatrix) {
	#ctx.ctx.setTransform(m.matrix);
}

pub fn (ctx CanvasRenderingContext2D) set_transform(a f64, b f64, c f64, d f64, e f64, f f64) {
	#ctx.ctx.setTransform(a.val,b.val,c.val,d.val,e.val,f.val);
}

pub fn (ctx CanvasRenderingContext2D) global_alpha() f64 {
	res := 0.0
	#res.val = ctx.ctx.globalAlpha

	return res
}

pub fn (ctx CanvasRenderingContext2D) set_global_alpha(alpha f64) {
	#ctx.ctx.globalAlpha = alpha.val;
}

pub fn (ctx CanvasRenderingContext2D) global_composite_operation() string {
	res := ''
	#res.str = ctx.ctx.globalCompositeOperation

	return res
}

pub fn (ctx CanvasRenderingContext2D) set_global_composite_operation(typ string) {
	#ctx.ctx.globalCompositeOperation = typ.str;
}

pub fn (ctx CanvasRenderingContext2D) save() {
	#ctx.ctx.save();
}

pub fn (ctx CanvasRenderingContext2D) restore() {
	#ctx.ctx.restore()
}

pub fn (ctx CanvasRenderingContext2D) canvas() HTMLCanvasElement {
	elem := HTMLCanvasElement{}
	#elem.node = ctx.ctx.canvas()

	return elem
}
