typedef unsigned char u8;
typedef unsigned short u16;
typedef u8 Array_fixed_u8_4[4];

typedef struct main__Setup {
	u8 status;
	u8 min_keycode;
	u8 max_keycode;
	Array_fixed_u8_4 pad1;
} main__Setup;

typedef struct main__Screen {
	u16 width;
} main__Screen;

typedef struct main__ScreenIter {
	main__Screen* data;
	int rem;
	int index;
} main__ScreenIter;

main__ScreenIter setup_roots_iterator(main__Setup* setup) {
	static main__Screen screen;
	screen.width = (u16)(640 + setup->pad1[0]);
	main__ScreenIter iter = {
		.data = &screen,
		.rem = (int)(setup->max_keycode - setup->min_keycode),
		.index = (int)(setup->status + setup->pad1[3]),
	};
	return iter;
}
