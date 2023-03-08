module gx

[params]
pub struct RectCfg {
	pub:
	width		int = 1
	heigh		int = 1
	radius		int
	thickness	int = 1// 0 for filled
	color		Color = black
}
