module east_asian

fn test_width() {
	assert east_asian_width_property_at('A', 0) == .narrow
	assert east_asian_width_property_at('Ａ', 0) == .full
	assert east_asian_width_property_at('ｱ', 0) == .half
	assert east_asian_width_property_at('ア', 0) == .wide
	assert east_asian_width_property_at('☆', 0) == .ambiguous
	assert east_asian_width_property_at('ج', 0) == .neutral
	assert display_width('abc', 1) == 3
	assert display_width('ひらがな', 1) == 8
	assert display_width('カタカナ', 1) == 8
	assert display_width('ｶﾀｶﾅ', 1) == 4
	assert display_width('한글', 1) == 4
	assert display_width('한자', 1) == 4
	assert display_width('漢字', 1) == 4
	assert display_width('简体字', 1) == 6
	assert display_width('繁體字', 1) == 6
	assert display_width('अरबी लिपि', 1) == 9
	assert display_width('☆', 1) == 1
	assert display_width('☆', 2) == 2
	assert display_width('🐈👽📛', 1) == 6
}
