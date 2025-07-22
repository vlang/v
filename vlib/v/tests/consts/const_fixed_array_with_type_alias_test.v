pub const k_palette_colors = [Pixel([u8(255), 255, 255]!), Pixel([u8(192), 192, 192]!),
	Pixel([u8(96), 96, 96]!), Pixel([u8(0), 0, 0]!)]

pub type Pixel = [3]u8
pub type Palette = []Pixel

fn test_main() {
	a := Palette(k_palette_colors)

	assert a[0] == [u8(255), 255, 255]!
	assert a[1] == [u8(192), 192, 192]!
	assert a[2] == [u8(96), 96, 96]!
	assert a[3] == [u8(0), 0, 0]!
}
