module jsdom

pub struct JS.Path2D {}

pub fn (p JS.Path2D) addPath(JS.Path2D, JS.DOMMatrix)
pub fn (p JS.Path2D) closePath()
pub fn (p JS.Path2D) moveTo(x JS.Number, y JS.Number)
pub fn (p JS.Path2D) lineTo(x JS.Number, y JS.Number)
pub fn (p JS.Path2D) bezierCurveTo(cp1x JS.Number, cp1y JS.Number, cp2x JS.Number, cp2y JS.Number, x JS.Number, y JS.Number)
pub fn (p JS.Path2D) quadraticCurveTo(cpx JS.Number, cpy JS.Number, x JS.Number, y JS.Number)
pub fn (p JS.Path2D) arc(x JS.Number, y JS.Number, radius JS.Number, startAngle JS.Number, endAngle JS.Number, counter_clockwise JS.Boolean)
pub fn (p JS.Path2D) arcTo(x1 JS.Number, y1 JS.Number, x2 JS.Number, y2 JS.Number, radius JS.Number)
pub fn (p JS.Path2D) ellipse(x JS.Number, y JS.Number, radius_x JS.Number, radius_y JS.Number, rotation JS.Number, start_angle JS.Number, end_angle JS.Number, counter_clockwise JS.Boolean)
pub fn (p JS.Path2D) rect(x JS.Number, y JS.Number, width JS.Number, height JS.Number)

pub struct Path2D {
mut:
	path JS.Path2D [noinit]
}

pub fn (p Path2D) add_path(p2 Path2D) {
	#p.path.addPath(p2.path);
}

pub fn (p Path2D) add_path_with_transform(p2 Path2D, m DOMMatrix) {
	p.path.addPath(p2.path, m.matrix)
}

pub fn (path Path2D) bezier_curve_to(cp1x f64, cp1y f64, cp2x f64, cp2y f64, x f64, y f64) {
	#path.path.bezierCurveTo(cp1x.val,cp1y.val,cp2x.val,cp2y.val, x.val,y.val);
}

pub fn (path Path2D) quadratic_curve_to(cpx f64, cpy f64, x f64, y f64) {
	#path.path.quadraticCurveTo(cpx.val,cpy.val,x.val,y.val);
}

pub fn (path Path2D) arc(x f64, y f64, radius f64, start_angle f64, end_angle f64, counter_clockwise bool) {
	#path.path.arc(x.val,y.val,radius.val,start_angle.val,end_angle.val,counter_clockwise.val)
}

pub fn (path Path2D) arc_to(x1 f64, y1 f64, x2 f64, y2 f64, radius f64) {
	#path.path.arcTo(x1.val,y1.val,x2.val,y2.val,radius.val);
}

pub fn (path Path2D) ellipse(x f64, y f64, radius_x f64, radius_y f64, rotation f64, start_angle f64, end_angle f64, counter_clockwise bool) {
	#path.path.ellipse(x.val,y.val,radius_x.val,radius_y.val,rotation.val,start_angle.val,end_angle.val,counter_clockwise.val);
}

pub fn (path Path2D) rect(x f64, y f64, width f64, height f64) {
	#path.path.rect(x.val,y.val,widht.val,height.val);
}

pub fn (path Path2D) line_to(x f64, y f64) {
	path.path.lineTo(JS.Number(x), JS.Number(y))
}

pub fn (path Path2D) move_to(x f64, y f64) {
	path.path.lineTo(JS.Number(x), JS.Number(y))
}

pub fn (path Path2D) close_path() {
	path.path.closePath()
}
