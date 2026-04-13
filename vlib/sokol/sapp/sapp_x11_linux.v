module sapp

// X11 backend implementation for sokol_app.
// Ports the C _sapp_x11_* functions from sokol_app.h to V.

const x11_max_keycodes = 256

// Keysym-to-unicode lookup table (from GLFW's xkb_unicode.c)
struct X11Codepair {
	keysym u16
	ucs    u16
}

const x11_keysymtab = [
	X11Codepair{0x01a1, 0x0104},
	X11Codepair{0x01a2, 0x02d8},
	X11Codepair{0x01a3, 0x0141},
	X11Codepair{0x01a5, 0x013d},
	X11Codepair{0x01a6, 0x015a},
	X11Codepair{0x01a9, 0x0160},
	X11Codepair{0x01aa, 0x015e},
	X11Codepair{0x01ab, 0x0164},
	X11Codepair{0x01ac, 0x0179},
	X11Codepair{0x01ae, 0x017d},
	X11Codepair{0x01af, 0x017b},
	X11Codepair{0x01b1, 0x0105},
	X11Codepair{0x01b2, 0x02db},
	X11Codepair{0x01b3, 0x0142},
	X11Codepair{0x01b5, 0x013e},
	X11Codepair{0x01b6, 0x015b},
	X11Codepair{0x01b7, 0x02c7},
	X11Codepair{0x01b9, 0x0161},
	X11Codepair{0x01ba, 0x015f},
	X11Codepair{0x01bb, 0x0165},
	X11Codepair{0x01bc, 0x017a},
	X11Codepair{0x01bd, 0x02dd},
	X11Codepair{0x01be, 0x017e},
	X11Codepair{0x01bf, 0x017c},
	X11Codepair{0x01c0, 0x0154},
	X11Codepair{0x01c3, 0x0102},
	X11Codepair{0x01c5, 0x0139},
	X11Codepair{0x01c6, 0x0106},
	X11Codepair{0x01c8, 0x010c},
	X11Codepair{0x01ca, 0x0118},
	X11Codepair{0x01cc, 0x011a},
	X11Codepair{0x01cf, 0x010e},
	X11Codepair{0x01d0, 0x0110},
	X11Codepair{0x01d1, 0x0143},
	X11Codepair{0x01d2, 0x0147},
	X11Codepair{0x01d5, 0x0150},
	X11Codepair{0x01d8, 0x0158},
	X11Codepair{0x01d9, 0x016e},
	X11Codepair{0x01db, 0x0170},
	X11Codepair{0x01de, 0x0162},
	X11Codepair{0x01e0, 0x0155},
	X11Codepair{0x01e3, 0x0103},
	X11Codepair{0x01e5, 0x013a},
	X11Codepair{0x01e6, 0x0107},
	X11Codepair{0x01e8, 0x010d},
	X11Codepair{0x01ea, 0x0119},
	X11Codepair{0x01ec, 0x011b},
	X11Codepair{0x01ef, 0x010f},
	X11Codepair{0x01f0, 0x0111},
	X11Codepair{0x01f1, 0x0144},
	X11Codepair{0x01f2, 0x0148},
	X11Codepair{0x01f5, 0x0151},
	X11Codepair{0x01f8, 0x0159},
	X11Codepair{0x01f9, 0x016f},
	X11Codepair{0x01fb, 0x0171},
	X11Codepair{0x01fe, 0x0163},
	X11Codepair{0x01ff, 0x02d9},
	X11Codepair{0x02a1, 0x0126},
	X11Codepair{0x02a6, 0x0124},
	X11Codepair{0x02a9, 0x0130},
	X11Codepair{0x02ab, 0x011e},
	X11Codepair{0x02ac, 0x0134},
	X11Codepair{0x02b1, 0x0127},
	X11Codepair{0x02b6, 0x0125},
	X11Codepair{0x02b9, 0x0131},
	X11Codepair{0x02bb, 0x011f},
	X11Codepair{0x02bc, 0x0135},
	X11Codepair{0x02c5, 0x010a},
	X11Codepair{0x02c6, 0x0108},
	X11Codepair{0x02d5, 0x0120},
	X11Codepair{0x02d8, 0x011c},
	X11Codepair{0x02dd, 0x016c},
	X11Codepair{0x02de, 0x015c},
	X11Codepair{0x02e5, 0x010b},
	X11Codepair{0x02e6, 0x0109},
	X11Codepair{0x02f5, 0x0121},
	X11Codepair{0x02f8, 0x011d},
	X11Codepair{0x02fd, 0x016d},
	X11Codepair{0x02fe, 0x015d},
	X11Codepair{0x03a2, 0x0138},
	X11Codepair{0x03a3, 0x0156},
	X11Codepair{0x03a5, 0x0128},
	X11Codepair{0x03a6, 0x013b},
	X11Codepair{0x03aa, 0x0112},
	X11Codepair{0x03ab, 0x0122},
	X11Codepair{0x03ac, 0x0166},
	X11Codepair{0x03b3, 0x0157},
	X11Codepair{0x03b5, 0x0129},
	X11Codepair{0x03b6, 0x013c},
	X11Codepair{0x03ba, 0x0113},
	X11Codepair{0x03bb, 0x0123},
	X11Codepair{0x03bc, 0x0167},
	X11Codepair{0x03bd, 0x014a},
	X11Codepair{0x03bf, 0x014b},
	X11Codepair{0x03c0, 0x0100},
	X11Codepair{0x03c7, 0x012e},
	X11Codepair{0x03cc, 0x0116},
	X11Codepair{0x03cf, 0x012a},
	X11Codepair{0x03d1, 0x0145},
	X11Codepair{0x03d2, 0x014c},
	X11Codepair{0x03d3, 0x0136},
	X11Codepair{0x03d9, 0x0172},
	X11Codepair{0x03dd, 0x0168},
	X11Codepair{0x03de, 0x016a},
	X11Codepair{0x03e0, 0x0101},
	X11Codepair{0x03e7, 0x012f},
	X11Codepair{0x03ec, 0x0117},
	X11Codepair{0x03ef, 0x012b},
	X11Codepair{0x03f1, 0x0146},
	X11Codepair{0x03f2, 0x014d},
	X11Codepair{0x03f3, 0x0137},
	X11Codepair{0x03f9, 0x0173},
	X11Codepair{0x03fd, 0x0169},
	X11Codepair{0x03fe, 0x016b},
	X11Codepair{0x047e, 0x203e},
	X11Codepair{0x04a1, 0x3002},
	X11Codepair{0x04a2, 0x300c},
	X11Codepair{0x04a3, 0x300d},
	X11Codepair{0x04a4, 0x3001},
	X11Codepair{0x04a5, 0x30fb},
	X11Codepair{0x04a6, 0x30f2},
	X11Codepair{0x04a7, 0x30a1},
	X11Codepair{0x04a8, 0x30a3},
	X11Codepair{0x04a9, 0x30a5},
	X11Codepair{0x04aa, 0x30a7},
	X11Codepair{0x04ab, 0x30a9},
	X11Codepair{0x04ac, 0x30e3},
	X11Codepair{0x04ad, 0x30e5},
	X11Codepair{0x04ae, 0x30e7},
	X11Codepair{0x04af, 0x30c3},
	X11Codepair{0x04b0, 0x30fc},
	X11Codepair{0x04b1, 0x30a2},
	X11Codepair{0x04b2, 0x30a4},
	X11Codepair{0x04b3, 0x30a6},
	X11Codepair{0x04b4, 0x30a8},
	X11Codepair{0x04b5, 0x30aa},
	X11Codepair{0x04b6, 0x30ab},
	X11Codepair{0x04b7, 0x30ad},
	X11Codepair{0x04b8, 0x30af},
	X11Codepair{0x04b9, 0x30b1},
	X11Codepair{0x04ba, 0x30b3},
	X11Codepair{0x04bb, 0x30b5},
	X11Codepair{0x04bc, 0x30b7},
	X11Codepair{0x04bd, 0x30b9},
	X11Codepair{0x04be, 0x30bb},
	X11Codepair{0x04bf, 0x30bd},
	X11Codepair{0x04c0, 0x30bf},
	X11Codepair{0x04c1, 0x30c1},
	X11Codepair{0x04c2, 0x30c4},
	X11Codepair{0x04c3, 0x30c6},
	X11Codepair{0x04c4, 0x30c8},
	X11Codepair{0x04c5, 0x30ca},
	X11Codepair{0x04c6, 0x30cb},
	X11Codepair{0x04c7, 0x30cc},
	X11Codepair{0x04c8, 0x30cd},
	X11Codepair{0x04c9, 0x30ce},
	X11Codepair{0x04ca, 0x30cf},
	X11Codepair{0x04cb, 0x30d2},
	X11Codepair{0x04cc, 0x30d5},
	X11Codepair{0x04cd, 0x30d8},
	X11Codepair{0x04ce, 0x30db},
	X11Codepair{0x04cf, 0x30de},
	X11Codepair{0x04d0, 0x30df},
	X11Codepair{0x04d1, 0x30e0},
	X11Codepair{0x04d2, 0x30e1},
	X11Codepair{0x04d3, 0x30e2},
	X11Codepair{0x04d4, 0x30e4},
	X11Codepair{0x04d5, 0x30e6},
	X11Codepair{0x04d6, 0x30e8},
	X11Codepair{0x04d7, 0x30e9},
	X11Codepair{0x04d8, 0x30ea},
	X11Codepair{0x04d9, 0x30eb},
	X11Codepair{0x04da, 0x30ec},
	X11Codepair{0x04db, 0x30ed},
	X11Codepair{0x04dc, 0x30ef},
	X11Codepair{0x04dd, 0x30f3},
	X11Codepair{0x04de, 0x309b},
	X11Codepair{0x04df, 0x309c},
	X11Codepair{0x05ac, 0x060c},
	X11Codepair{0x05bb, 0x061b},
	X11Codepair{0x05bf, 0x061f},
	X11Codepair{0x05c1, 0x0621},
	X11Codepair{0x05c2, 0x0622},
	X11Codepair{0x05c3, 0x0623},
	X11Codepair{0x05c4, 0x0624},
	X11Codepair{0x05c5, 0x0625},
	X11Codepair{0x05c6, 0x0626},
	X11Codepair{0x05c7, 0x0627},
	X11Codepair{0x05c8, 0x0628},
	X11Codepair{0x05c9, 0x0629},
	X11Codepair{0x05ca, 0x062a},
	X11Codepair{0x05cb, 0x062b},
	X11Codepair{0x05cc, 0x062c},
	X11Codepair{0x05cd, 0x062d},
	X11Codepair{0x05ce, 0x062e},
	X11Codepair{0x05cf, 0x062f},
	X11Codepair{0x05d0, 0x0630},
	X11Codepair{0x05d1, 0x0631},
	X11Codepair{0x05d2, 0x0632},
	X11Codepair{0x05d3, 0x0633},
	X11Codepair{0x05d4, 0x0634},
	X11Codepair{0x05d5, 0x0635},
	X11Codepair{0x05d6, 0x0636},
	X11Codepair{0x05d7, 0x0637},
	X11Codepair{0x05d8, 0x0638},
	X11Codepair{0x05d9, 0x0639},
	X11Codepair{0x05da, 0x063a},
	X11Codepair{0x05e0, 0x0640},
	X11Codepair{0x05e1, 0x0641},
	X11Codepair{0x05e2, 0x0642},
	X11Codepair{0x05e3, 0x0643},
	X11Codepair{0x05e4, 0x0644},
	X11Codepair{0x05e5, 0x0645},
	X11Codepair{0x05e6, 0x0646},
	X11Codepair{0x05e7, 0x0647},
	X11Codepair{0x05e8, 0x0648},
	X11Codepair{0x05e9, 0x0649},
	X11Codepair{0x05ea, 0x064a},
	X11Codepair{0x05eb, 0x064b},
	X11Codepair{0x05ec, 0x064c},
	X11Codepair{0x05ed, 0x064d},
	X11Codepair{0x05ee, 0x064e},
	X11Codepair{0x05ef, 0x064f},
	X11Codepair{0x05f0, 0x0650},
	X11Codepair{0x05f1, 0x0651},
	X11Codepair{0x05f2, 0x0652},
	X11Codepair{0x06a1, 0x0452},
	X11Codepair{0x06a2, 0x0453},
	X11Codepair{0x06a3, 0x0451},
	X11Codepair{0x06a4, 0x0454},
	X11Codepair{0x06a5, 0x0455},
	X11Codepair{0x06a6, 0x0456},
	X11Codepair{0x06a7, 0x0457},
	X11Codepair{0x06a8, 0x0458},
	X11Codepair{0x06a9, 0x0459},
	X11Codepair{0x06aa, 0x045a},
	X11Codepair{0x06ab, 0x045b},
	X11Codepair{0x06ac, 0x045c},
	X11Codepair{0x06ae, 0x045e},
	X11Codepair{0x06af, 0x045f},
	X11Codepair{0x06b0, 0x2116},
	X11Codepair{0x06b1, 0x0402},
	X11Codepair{0x06b2, 0x0403},
	X11Codepair{0x06b3, 0x0401},
	X11Codepair{0x06b4, 0x0404},
	X11Codepair{0x06b5, 0x0405},
	X11Codepair{0x06b6, 0x0406},
	X11Codepair{0x06b7, 0x0407},
	X11Codepair{0x06b8, 0x0408},
	X11Codepair{0x06b9, 0x0409},
	X11Codepair{0x06ba, 0x040a},
	X11Codepair{0x06bb, 0x040b},
	X11Codepair{0x06bc, 0x040c},
	X11Codepair{0x06be, 0x040e},
	X11Codepair{0x06bf, 0x040f},
	X11Codepair{0x06c0, 0x044e},
	X11Codepair{0x06c1, 0x0430},
	X11Codepair{0x06c2, 0x0431},
	X11Codepair{0x06c3, 0x0446},
	X11Codepair{0x06c4, 0x0434},
	X11Codepair{0x06c5, 0x0435},
	X11Codepair{0x06c6, 0x0444},
	X11Codepair{0x06c7, 0x0433},
	X11Codepair{0x06c8, 0x0445},
	X11Codepair{0x06c9, 0x0438},
	X11Codepair{0x06ca, 0x0439},
	X11Codepair{0x06cb, 0x043a},
	X11Codepair{0x06cc, 0x043b},
	X11Codepair{0x06cd, 0x043c},
	X11Codepair{0x06ce, 0x043d},
	X11Codepair{0x06cf, 0x043e},
	X11Codepair{0x06d0, 0x043f},
	X11Codepair{0x06d1, 0x044f},
	X11Codepair{0x06d2, 0x0440},
	X11Codepair{0x06d3, 0x0441},
	X11Codepair{0x06d4, 0x0442},
	X11Codepair{0x06d5, 0x0443},
	X11Codepair{0x06d6, 0x0436},
	X11Codepair{0x06d7, 0x0432},
	X11Codepair{0x06d8, 0x044c},
	X11Codepair{0x06d9, 0x044b},
	X11Codepair{0x06da, 0x0437},
	X11Codepair{0x06db, 0x0448},
	X11Codepair{0x06dc, 0x044d},
	X11Codepair{0x06dd, 0x0449},
	X11Codepair{0x06de, 0x0447},
	X11Codepair{0x06df, 0x044a},
	X11Codepair{0x06e0, 0x042e},
	X11Codepair{0x06e1, 0x0410},
	X11Codepair{0x06e2, 0x0411},
	X11Codepair{0x06e3, 0x0426},
	X11Codepair{0x06e4, 0x0414},
	X11Codepair{0x06e5, 0x0415},
	X11Codepair{0x06e6, 0x0424},
	X11Codepair{0x06e7, 0x0413},
	X11Codepair{0x06e8, 0x0425},
	X11Codepair{0x06e9, 0x0418},
	X11Codepair{0x06ea, 0x0419},
	X11Codepair{0x06eb, 0x041a},
	X11Codepair{0x06ec, 0x041b},
	X11Codepair{0x06ed, 0x041c},
	X11Codepair{0x06ee, 0x041d},
	X11Codepair{0x06ef, 0x041e},
	X11Codepair{0x06f0, 0x041f},
	X11Codepair{0x06f1, 0x042f},
	X11Codepair{0x06f2, 0x0420},
	X11Codepair{0x06f3, 0x0421},
	X11Codepair{0x06f4, 0x0422},
	X11Codepair{0x06f5, 0x0423},
	X11Codepair{0x06f6, 0x0416},
	X11Codepair{0x06f7, 0x0412},
	X11Codepair{0x06f8, 0x042c},
	X11Codepair{0x06f9, 0x042b},
	X11Codepair{0x06fa, 0x0417},
	X11Codepair{0x06fb, 0x0428},
	X11Codepair{0x06fc, 0x042d},
	X11Codepair{0x06fd, 0x0429},
	X11Codepair{0x06fe, 0x0427},
	X11Codepair{0x06ff, 0x042a},
	X11Codepair{0x07a1, 0x0386},
	X11Codepair{0x07a2, 0x0388},
	X11Codepair{0x07a3, 0x0389},
	X11Codepair{0x07a4, 0x038a},
	X11Codepair{0x07a5, 0x03aa},
	X11Codepair{0x07a7, 0x038c},
	X11Codepair{0x07a8, 0x038e},
	X11Codepair{0x07a9, 0x03ab},
	X11Codepair{0x07ab, 0x038f},
	X11Codepair{0x07ae, 0x0385},
	X11Codepair{0x07af, 0x2015},
	X11Codepair{0x07b1, 0x03ac},
	X11Codepair{0x07b2, 0x03ad},
	X11Codepair{0x07b3, 0x03ae},
	X11Codepair{0x07b4, 0x03af},
	X11Codepair{0x07b5, 0x03ca},
	X11Codepair{0x07b6, 0x0390},
	X11Codepair{0x07b7, 0x03cc},
	X11Codepair{0x07b8, 0x03cd},
	X11Codepair{0x07b9, 0x03cb},
	X11Codepair{0x07ba, 0x03b0},
	X11Codepair{0x07bb, 0x03ce},
	X11Codepair{0x07c1, 0x0391},
	X11Codepair{0x07c2, 0x0392},
	X11Codepair{0x07c3, 0x0393},
	X11Codepair{0x07c4, 0x0394},
	X11Codepair{0x07c5, 0x0395},
	X11Codepair{0x07c6, 0x0396},
	X11Codepair{0x07c7, 0x0397},
	X11Codepair{0x07c8, 0x0398},
	X11Codepair{0x07c9, 0x0399},
	X11Codepair{0x07ca, 0x039a},
	X11Codepair{0x07cb, 0x039b},
	X11Codepair{0x07cc, 0x039c},
	X11Codepair{0x07cd, 0x039d},
	X11Codepair{0x07ce, 0x039e},
	X11Codepair{0x07cf, 0x039f},
	X11Codepair{0x07d0, 0x03a0},
	X11Codepair{0x07d1, 0x03a1},
	X11Codepair{0x07d2, 0x03a3},
	X11Codepair{0x07d4, 0x03a4},
	X11Codepair{0x07d5, 0x03a5},
	X11Codepair{0x07d6, 0x03a6},
	X11Codepair{0x07d7, 0x03a7},
	X11Codepair{0x07d8, 0x03a8},
	X11Codepair{0x07d9, 0x03a9},
	X11Codepair{0x07e1, 0x03b1},
	X11Codepair{0x07e2, 0x03b2},
	X11Codepair{0x07e3, 0x03b3},
	X11Codepair{0x07e4, 0x03b4},
	X11Codepair{0x07e5, 0x03b5},
	X11Codepair{0x07e6, 0x03b6},
	X11Codepair{0x07e7, 0x03b7},
	X11Codepair{0x07e8, 0x03b8},
	X11Codepair{0x07e9, 0x03b9},
	X11Codepair{0x07ea, 0x03ba},
	X11Codepair{0x07eb, 0x03bb},
	X11Codepair{0x07ec, 0x03bc},
	X11Codepair{0x07ed, 0x03bd},
	X11Codepair{0x07ee, 0x03be},
	X11Codepair{0x07ef, 0x03bf},
	X11Codepair{0x07f0, 0x03c0},
	X11Codepair{0x07f1, 0x03c1},
	X11Codepair{0x07f2, 0x03c3},
	X11Codepair{0x07f3, 0x03c2},
	X11Codepair{0x07f4, 0x03c4},
	X11Codepair{0x07f5, 0x03c5},
	X11Codepair{0x07f6, 0x03c6},
	X11Codepair{0x07f7, 0x03c7},
	X11Codepair{0x07f8, 0x03c8},
	X11Codepair{0x07f9, 0x03c9},
	X11Codepair{0x0cdf, 0x2017},
	X11Codepair{0x0ce0, 0x05d0},
	X11Codepair{0x0ce1, 0x05d1},
	X11Codepair{0x0ce2, 0x05d2},
	X11Codepair{0x0ce3, 0x05d3},
	X11Codepair{0x0ce4, 0x05d4},
	X11Codepair{0x0ce5, 0x05d5},
	X11Codepair{0x0ce6, 0x05d6},
	X11Codepair{0x0ce7, 0x05d7},
	X11Codepair{0x0ce8, 0x05d8},
	X11Codepair{0x0ce9, 0x05d9},
	X11Codepair{0x0cea, 0x05da},
	X11Codepair{0x0ceb, 0x05db},
	X11Codepair{0x0cec, 0x05dc},
	X11Codepair{0x0ced, 0x05dd},
	X11Codepair{0x0cee, 0x05de},
	X11Codepair{0x0cef, 0x05df},
	X11Codepair{0x0cf0, 0x05e0},
	X11Codepair{0x0cf1, 0x05e1},
	X11Codepair{0x0cf2, 0x05e2},
	X11Codepair{0x0cf3, 0x05e3},
	X11Codepair{0x0cf4, 0x05e4},
	X11Codepair{0x0cf5, 0x05e5},
	X11Codepair{0x0cf6, 0x05e6},
	X11Codepair{0x0cf7, 0x05e7},
	X11Codepair{0x0cf8, 0x05e8},
	X11Codepair{0x0cf9, 0x05e9},
	X11Codepair{0x0cfa, 0x05ea},
	X11Codepair{0x13a4, 0x20ac},
	X11Codepair{0x13bc, 0x0152},
	X11Codepair{0x13bd, 0x0153},
	X11Codepair{0x13be, 0x0178},
	X11Codepair{0x20ac, 0x20ac},
	X11Codepair{0xfe50, 0x0060},
	X11Codepair{0xfe51, 0x00b4},
	X11Codepair{0xfe52, 0x005e},
	X11Codepair{0xfe53, 0x007e},
	X11Codepair{0xfe54, 0x00af},
	X11Codepair{0xfe55, 0x02d8},
	X11Codepair{0xfe56, 0x02d9},
	X11Codepair{0xfe57, 0x00a8},
	X11Codepair{0xfe58, 0x02da},
	X11Codepair{0xfe59, 0x02dd},
	X11Codepair{0xfe5a, 0x02c7},
	X11Codepair{0xfe5b, 0x00b8},
	X11Codepair{0xfe5c, 0x02db},
	X11Codepair{0xfe5d, 0x037a},
	X11Codepair{0xfe5e, 0x309b},
	X11Codepair{0xfe5f, 0x309c},
	X11Codepair{0xfe63, 0x002f},
	X11Codepair{0xfe64, 0x02bc},
	X11Codepair{0xfe65, 0x02bd},
	X11Codepair{0xfe66, 0x02f5},
	X11Codepair{0xfe67, 0x02f3},
	X11Codepair{0xfe68, 0x02cd},
	X11Codepair{0xfe69, 0xa788},
	X11Codepair{0xfe6a, 0x02f7},
	X11Codepair{0xfe6e, 0x002c},
	X11Codepair{0xfe6f, 0x00a4},
	X11Codepair{0xfe80, 0x0061},
	X11Codepair{0xfe81, 0x0041},
	X11Codepair{0xfe82, 0x0065},
	X11Codepair{0xfe83, 0x0045},
	X11Codepair{0xfe84, 0x0069},
	X11Codepair{0xfe85, 0x0049},
	X11Codepair{0xfe86, 0x006f},
	X11Codepair{0xfe87, 0x004f},
	X11Codepair{0xfe88, 0x0075},
	X11Codepair{0xfe89, 0x0055},
	X11Codepair{0xfe8a, 0x0259},
	X11Codepair{0xfe8b, 0x018f},
	X11Codepair{0xfe8c, 0x00b5},
	X11Codepair{0xfe90, 0x005f},
	X11Codepair{0xfe91, 0x02c8},
	X11Codepair{0xfe92, 0x02cc},
	X11Codepair{0xff80, 0x0020},
	X11Codepair{0xff95, 0x0037},
	X11Codepair{0xff96, 0x0034},
	X11Codepair{0xff97, 0x0038},
	X11Codepair{0xff98, 0x0036},
	X11Codepair{0xff99, 0x0032},
	X11Codepair{0xff9a, 0x0039},
	X11Codepair{0xff9b, 0x0033},
	X11Codepair{0xff9c, 0x0031},
	X11Codepair{0xff9d, 0x0035},
	X11Codepair{0xff9e, 0x0030},
	X11Codepair{0xffaa, 0x002a},
	X11Codepair{0xffab, 0x002b},
	X11Codepair{0xffac, 0x002c},
	X11Codepair{0xffad, 0x002d},
	X11Codepair{0xffae, 0x002e},
	X11Codepair{0xffaf, 0x002f},
	X11Codepair{0xffb0, 0x0030},
	X11Codepair{0xffb1, 0x0031},
	X11Codepair{0xffb2, 0x0032},
	X11Codepair{0xffb3, 0x0033},
	X11Codepair{0xffb4, 0x0034},
	X11Codepair{0xffb5, 0x0035},
	X11Codepair{0xffb6, 0x0036},
	X11Codepair{0xffb7, 0x0037},
	X11Codepair{0xffb8, 0x0038},
	X11Codepair{0xffb9, 0x0039},
	X11Codepair{0xffbd, 0x003d},
]!

// === Error handling ===

fn x11_grab_error_handler() {
	g_sapp_state.x11.error_code = 0 // Success
	C.XSetErrorHandler(voidptr(x11_error_handler))
}

fn x11_release_error_handler() {
	C.XSync(g_sapp_state.x11.display, x_false)
	C.XSetErrorHandler(unsafe { nil })
}

fn x11_error_handler(_display &C.Display, _event voidptr) int {
	// XErrorEvent.error_code is at a known offset
	// For simplicity, just set a non-zero error code
	g_sapp_state.x11.error_code = 1
	return 0
}

// === Atom initialization ===

fn x11_init_extensions() {
	d := g_sapp_state.x11.display
	g_sapp_state.x11.utf8_string = C.XInternAtom(d, c'UTF8_STRING', x_false)
	g_sapp_state.x11.wm_protocols = C.XInternAtom(d, c'WM_PROTOCOLS', x_false)
	g_sapp_state.x11.wm_delete_window = C.XInternAtom(d, c'WM_DELETE_WINDOW', x_false)
	g_sapp_state.x11.wm_state = C.XInternAtom(d, c'WM_STATE', x_false)
	g_sapp_state.x11.net_wm_name = C.XInternAtom(d, c'_NET_WM_NAME', x_false)
	g_sapp_state.x11.net_wm_icon_name = C.XInternAtom(d, c'_NET_WM_ICON_NAME', x_false)
	g_sapp_state.x11.net_wm_icon = C.XInternAtom(d, c'_NET_WM_ICON', x_false)
	g_sapp_state.x11.net_wm_state = C.XInternAtom(d, c'_NET_WM_STATE', x_false)
	g_sapp_state.x11.net_wm_state_fullscreen = C.XInternAtom(d, c'_NET_WM_STATE_FULLSCREEN',
		x_false)
	g_sapp_state.x11.clipboard_atom = C.XInternAtom(d, c'CLIPBOARD', x_false)
	g_sapp_state.x11.targets = C.XInternAtom(d, c'TARGETS', x_false)
	if g_sapp_state.drop.enabled {
		g_sapp_state.x11.xdnd.xdnd_aware = C.XInternAtom(d, c'XdndAware', x_false)
		g_sapp_state.x11.xdnd.xdnd_enter = C.XInternAtom(d, c'XdndEnter', x_false)
		g_sapp_state.x11.xdnd.xdnd_position = C.XInternAtom(d, c'XdndPosition', x_false)
		g_sapp_state.x11.xdnd.xdnd_status = C.XInternAtom(d, c'XdndStatus', x_false)
		g_sapp_state.x11.xdnd.xdnd_action_copy = C.XInternAtom(d, c'XdndActionCopy', x_false)
		g_sapp_state.x11.xdnd.xdnd_drop = C.XInternAtom(d, c'XdndDrop', x_false)
		g_sapp_state.x11.xdnd.xdnd_finished = C.XInternAtom(d, c'XdndFinished', x_false)
		g_sapp_state.x11.xdnd.xdnd_selection = C.XInternAtom(d, c'XdndSelection', x_false)
		g_sapp_state.x11.xdnd.xdnd_type_list = C.XInternAtom(d, c'XdndTypeList', x_false)
		g_sapp_state.x11.xdnd.text_uri_list = C.XInternAtom(d, c'text/uri-list', x_false)
	}
	// Check Xi extension for raw mouse input
	if C.XQueryExtension(d, c'XInputExtension', &g_sapp_state.x11.xi.major_opcode,
		&g_sapp_state.x11.xi.event_base, &g_sapp_state.x11.xi.error_base) != 0 {
		g_sapp_state.x11.xi.major = 2
		g_sapp_state.x11.xi.minor = 0
		if C.XIQueryVersion(d, &g_sapp_state.x11.xi.major, &g_sapp_state.x11.xi.minor) == 0 {
			g_sapp_state.x11.xi.available = true
		}
	}
}

// === Key translation ===

fn x11_translate_keysyms(keysyms &KeySym, width int) KeyCode {
	if width > 1 {
		unsafe {
			match keysyms[1] {
				xk_kp_0 { return .kp_0 }
				xk_kp_1 { return .kp_1 }
				xk_kp_2 { return .kp_2 }
				xk_kp_3 { return .kp_3 }
				xk_kp_4 { return .kp_4 }
				xk_kp_5 { return .kp_5 }
				xk_kp_6 { return .kp_6 }
				xk_kp_7 { return .kp_7 }
				xk_kp_8 { return .kp_8 }
				xk_kp_9 { return .kp_9 }
				xk_kp_separator, xk_kp_decimal { return .kp_decimal }
				xk_kp_equal { return .kp_equal }
				xk_kp_enter { return .kp_enter }
				else {}
			}
		}
	}
	unsafe {
		return match keysyms[0] {
			xk_escape { KeyCode.escape }
			xk_tab { KeyCode.tab }
			xk_shift_l { KeyCode.left_shift }
			xk_shift_r { KeyCode.right_shift }
			xk_control_l { KeyCode.left_control }
			xk_control_r { KeyCode.right_control }
			xk_meta_l, xk_alt_l { KeyCode.left_alt }
			xk_mode_switch, xk_iso_level3_shift, xk_meta_r, xk_alt_r { KeyCode.right_alt }
			xk_super_l { KeyCode.left_super }
			xk_super_r { KeyCode.right_super }
			xk_menu { KeyCode.menu }
			xk_num_lock { KeyCode.num_lock }
			xk_caps_lock { KeyCode.caps_lock }
			xk_print { KeyCode.print_screen }
			xk_scroll_lock { KeyCode.scroll_lock }
			xk_pause { KeyCode.pause }
			xk_delete { KeyCode.delete }
			xk_backspace { KeyCode.backspace }
			xk_return { KeyCode.enter }
			xk_home { KeyCode.home }
			xk_end { KeyCode.end }
			xk_page_up { KeyCode.page_up }
			xk_page_down { KeyCode.page_down }
			xk_insert { KeyCode.insert }
			xk_left { KeyCode.left }
			xk_right { KeyCode.right }
			xk_down { KeyCode.down }
			xk_up { KeyCode.up }
			xk_f1 { KeyCode.f1 }
			xk_f2 { KeyCode.f2 }
			xk_f3 { KeyCode.f3 }
			xk_f4 { KeyCode.f4 }
			xk_f5 { KeyCode.f5 }
			xk_f6 { KeyCode.f6 }
			xk_f7 { KeyCode.f7 }
			xk_f8 { KeyCode.f8 }
			xk_f9 { KeyCode.f9 }
			xk_f10 { KeyCode.f10 }
			xk_f11 { KeyCode.f11 }
			xk_f12 { KeyCode.f12 }
			xk_f13 { KeyCode.f13 }
			xk_f14 { KeyCode.f14 }
			xk_f15 { KeyCode.f15 }
			xk_f16 { KeyCode.f16 }
			xk_f17 { KeyCode.f17 }
			xk_f18 { KeyCode.f18 }
			xk_f19 { KeyCode.f19 }
			xk_f20 { KeyCode.f20 }
			xk_f21 { KeyCode.f21 }
			xk_f22 { KeyCode.f22 }
			xk_f23 { KeyCode.f23 }
			xk_f24 { KeyCode.f24 }
			xk_f25 { KeyCode.f25 }
			xk_kp_divide { KeyCode.kp_divide }
			xk_kp_multiply { KeyCode.kp_multiply }
			xk_kp_subtract { KeyCode.kp_subtract }
			xk_kp_add { KeyCode.kp_add }
			xk_kp_insert { KeyCode.kp_0 }
			xk_kp_end { KeyCode.kp_1 }
			xk_kp_down { KeyCode.kp_2 }
			xk_kp_page_down { KeyCode.kp_3 }
			xk_kp_left { KeyCode.kp_4 }
			xk_kp_right { KeyCode.kp_6 }
			xk_kp_home { KeyCode.kp_7 }
			xk_kp_up { KeyCode.kp_8 }
			xk_kp_page_up { KeyCode.kp_9 }
			xk_kp_delete { KeyCode.kp_decimal }
			xk_kp_equal { KeyCode.kp_equal }
			xk_kp_enter { KeyCode.kp_enter }
			xk_a { KeyCode.a }
			xk_b { KeyCode.b }
			xk_c { KeyCode.c }
			xk_d { KeyCode.d }
			xk_e { KeyCode.e }
			xk_f { KeyCode.f }
			xk_g { KeyCode.g }
			xk_h { KeyCode.h }
			xk_i { KeyCode.i }
			xk_j { KeyCode.j }
			xk_k { KeyCode.k }
			xk_l { KeyCode.l }
			xk_m { KeyCode.m }
			xk_n { KeyCode.n }
			xk_o { KeyCode.o }
			xk_p { KeyCode.p }
			xk_q { KeyCode.q }
			xk_r { KeyCode.r }
			xk_s { KeyCode.s }
			xk_t { KeyCode.t }
			xk_u { KeyCode.u }
			xk_v { KeyCode.v }
			xk_w { KeyCode.w }
			xk_x { KeyCode.x }
			xk_y { KeyCode.y }
			xk_z { KeyCode.z }
			xk_1 { KeyCode._1 }
			xk_2 { KeyCode._2 }
			xk_3 { KeyCode._3 }
			xk_4 { KeyCode._4 }
			xk_5 { KeyCode._5 }
			xk_6 { KeyCode._6 }
			xk_7 { KeyCode._7 }
			xk_8 { KeyCode._8 }
			xk_9 { KeyCode._9 }
			xk_0 { KeyCode._0 }
			xk_space { KeyCode.space }
			xk_minus { KeyCode.minus }
			xk_equal { KeyCode.equal }
			xk_bracketleft { KeyCode.left_bracket }
			xk_bracketright { KeyCode.right_bracket }
			xk_backslash { KeyCode.backslash }
			xk_semicolon { KeyCode.semicolon }
			xk_apostrophe { KeyCode.apostrophe }
			xk_grave { KeyCode.grave_accent }
			xk_comma { KeyCode.comma }
			xk_period { KeyCode.period }
			xk_slash { KeyCode.slash }
			xk_less { KeyCode.world_1 }
			else { KeyCode.invalid }
		}
	}
}

// XKB key name to keycode mapping entry
struct X11KeymapEntry {
mut:
	key  KeyCode
	name [4]u8
}

fn make_keymap_entry(key KeyCode, name string) X11KeymapEntry {
	mut e := X11KeymapEntry{
		key: key
	}
	for i := 0; i < 4 && i < name.len; i++ {
		unsafe {
			e.name[i] = name[i]
		}
	}
	return e
}

fn x11_init_keytable() {
	for i in 0 .. max_keycodes {
		g_sapp_state.keycodes[i] = .invalid
	}
	// Use XKB to determine physical key locations
	desc := C.XkbGetMap(g_sapp_state.x11.display, 0, xkb_use_core_kbd)
	if desc == unsafe { nil } {
		return
	}
	C.XkbGetNames(g_sapp_state.x11.display, xkb_key_names_mask | xkb_key_aliases_mask, desc)

	scancode_min := int(desc.min_key_code)
	scancode_max := int(desc.max_key_code)

	keymap := [
		make_keymap_entry(.grave_accent, 'TLDE'),
		make_keymap_entry(._1, 'AE01'),
		make_keymap_entry(._2, 'AE02'),
		make_keymap_entry(._3, 'AE03'),
		make_keymap_entry(._4, 'AE04'),
		make_keymap_entry(._5, 'AE05'),
		make_keymap_entry(._6, 'AE06'),
		make_keymap_entry(._7, 'AE07'),
		make_keymap_entry(._8, 'AE08'),
		make_keymap_entry(._9, 'AE09'),
		make_keymap_entry(._0, 'AE10'),
		make_keymap_entry(.minus, 'AE11'),
		make_keymap_entry(.equal, 'AE12'),
		make_keymap_entry(.q, 'AD01'),
		make_keymap_entry(.w, 'AD02'),
		make_keymap_entry(.e, 'AD03'),
		make_keymap_entry(.r, 'AD04'),
		make_keymap_entry(.t, 'AD05'),
		make_keymap_entry(.y, 'AD06'),
		make_keymap_entry(.u, 'AD07'),
		make_keymap_entry(.i, 'AD08'),
		make_keymap_entry(.o, 'AD09'),
		make_keymap_entry(.p, 'AD10'),
		make_keymap_entry(.left_bracket, 'AD11'),
		make_keymap_entry(.right_bracket, 'AD12'),
		make_keymap_entry(.a, 'AC01'),
		make_keymap_entry(.s, 'AC02'),
		make_keymap_entry(.d, 'AC03'),
		make_keymap_entry(.f, 'AC04'),
		make_keymap_entry(.g, 'AC05'),
		make_keymap_entry(.h, 'AC06'),
		make_keymap_entry(.j, 'AC07'),
		make_keymap_entry(.k, 'AC08'),
		make_keymap_entry(.l, 'AC09'),
		make_keymap_entry(.semicolon, 'AC10'),
		make_keymap_entry(.apostrophe, 'AC11'),
		make_keymap_entry(.z, 'AB01'),
		make_keymap_entry(.x, 'AB02'),
		make_keymap_entry(.c, 'AB03'),
		make_keymap_entry(.v, 'AB04'),
		make_keymap_entry(.b, 'AB05'),
		make_keymap_entry(.n, 'AB06'),
		make_keymap_entry(.m, 'AB07'),
		make_keymap_entry(.comma, 'AB08'),
		make_keymap_entry(.period, 'AB09'),
		make_keymap_entry(.slash, 'AB10'),
		make_keymap_entry(.backslash, 'BKSL'),
		make_keymap_entry(.world_1, 'LSGT'),
		make_keymap_entry(.space, 'SPCE'),
		make_keymap_entry(.escape, 'ESC\x00'),
		make_keymap_entry(.enter, 'RTRN'),
		make_keymap_entry(.tab, 'TAB\x00'),
		make_keymap_entry(.backspace, 'BKSP'),
		make_keymap_entry(.insert, 'INS\x00'),
		make_keymap_entry(.delete, 'DELE'),
		make_keymap_entry(.right, 'RGHT'),
		make_keymap_entry(.left, 'LEFT'),
		make_keymap_entry(.down, 'DOWN'),
		make_keymap_entry(.up, 'UP\x00\x00'),
		make_keymap_entry(.page_up, 'PGUP'),
		make_keymap_entry(.page_down, 'PGDN'),
		make_keymap_entry(.home, 'HOME'),
		make_keymap_entry(.end, 'END\x00'),
		make_keymap_entry(.caps_lock, 'CAPS'),
		make_keymap_entry(.scroll_lock, 'SCLK'),
		make_keymap_entry(.num_lock, 'NMLK'),
		make_keymap_entry(.print_screen, 'PRSC'),
		make_keymap_entry(.pause, 'PAUS'),
		make_keymap_entry(.f1, 'FK01'),
		make_keymap_entry(.f2, 'FK02'),
		make_keymap_entry(.f3, 'FK03'),
		make_keymap_entry(.f4, 'FK04'),
		make_keymap_entry(.f5, 'FK05'),
		make_keymap_entry(.f6, 'FK06'),
		make_keymap_entry(.f7, 'FK07'),
		make_keymap_entry(.f8, 'FK08'),
		make_keymap_entry(.f9, 'FK09'),
		make_keymap_entry(.f10, 'FK10'),
		make_keymap_entry(.f11, 'FK11'),
		make_keymap_entry(.f12, 'FK12'),
		make_keymap_entry(.f13, 'FK13'),
		make_keymap_entry(.f14, 'FK14'),
		make_keymap_entry(.f15, 'FK15'),
		make_keymap_entry(.f16, 'FK16'),
		make_keymap_entry(.f17, 'FK17'),
		make_keymap_entry(.f18, 'FK18'),
		make_keymap_entry(.f19, 'FK19'),
		make_keymap_entry(.f20, 'FK20'),
		make_keymap_entry(.f21, 'FK21'),
		make_keymap_entry(.f22, 'FK22'),
		make_keymap_entry(.f23, 'FK23'),
		make_keymap_entry(.f24, 'FK24'),
		make_keymap_entry(.f25, 'FK25'),
		make_keymap_entry(.kp_0, 'KP0\x00'),
		make_keymap_entry(.kp_1, 'KP1\x00'),
		make_keymap_entry(.kp_2, 'KP2\x00'),
		make_keymap_entry(.kp_3, 'KP3\x00'),
		make_keymap_entry(.kp_4, 'KP4\x00'),
		make_keymap_entry(.kp_5, 'KP5\x00'),
		make_keymap_entry(.kp_6, 'KP6\x00'),
		make_keymap_entry(.kp_7, 'KP7\x00'),
		make_keymap_entry(.kp_8, 'KP8\x00'),
		make_keymap_entry(.kp_9, 'KP9\x00'),
		make_keymap_entry(.kp_decimal, 'KPDL'),
		make_keymap_entry(.kp_divide, 'KPDV'),
		make_keymap_entry(.kp_multiply, 'KPMU'),
		make_keymap_entry(.kp_subtract, 'KPSU'),
		make_keymap_entry(.kp_add, 'KPAD'),
		make_keymap_entry(.kp_enter, 'KPEN'),
		make_keymap_entry(.kp_equal, 'KPEQ'),
		make_keymap_entry(.left_shift, 'LFSH'),
		make_keymap_entry(.left_control, 'LCTL'),
		make_keymap_entry(.left_alt, 'LALT'),
		make_keymap_entry(.left_super, 'LWIN'),
		make_keymap_entry(.right_shift, 'RTSH'),
		make_keymap_entry(.right_control, 'RCTL'),
		make_keymap_entry(.right_alt, 'RALT'),
		make_keymap_entry(.right_alt, 'LVL3'),
		make_keymap_entry(.right_alt, 'MDSW'),
		make_keymap_entry(.right_super, 'RWIN'),
		make_keymap_entry(.menu, 'MENU'),
	]

	// Find X11 keycode to sokol-app key code mapping
	for scancode in scancode_min .. scancode_max + 1 {
		mut key := KeyCode.invalid
		for km in keymap {
			if unsafe {
				C.strncmp(&char(&desc.names.keys[scancode].name[0]), &char(&km.name[0]),
					usize(xkb_key_name_length))
			} == 0 {
				key = km.key
				break
			}
		}
		// Fall back to key aliases
		if key == .invalid && desc.names.key_aliases != unsafe { nil } {
			for i in 0 .. int(desc.names.num_key_aliases) {
				if unsafe {
					C.strncmp(&char(&desc.names.key_aliases[i].real[0]),
						&char(&desc.names.keys[scancode].name[0]), usize(xkb_key_name_length))
				} != 0 {
					continue
				}
				for km in keymap {
					if unsafe {
						C.strncmp(&char(&desc.names.key_aliases[i].alias[0]), &char(&km.name[0]),
							usize(xkb_key_name_length))
					} == 0 {
						key = km.key
						break
					}
				}
				if key != .invalid {
					break
				}
			}
		}
		g_sapp_state.keycodes[scancode] = key
	}
	C.XkbFreeNames(desc, xkb_key_names_mask, x_true)
	C.XkbFreeKeyboard(desc, 0, x_true)

	// Fall back using traditional keysym lookup
	mut width := 0
	keysyms := C.XGetKeyboardMapping(g_sapp_state.x11.display, u8(scancode_min), scancode_max -
		scancode_min + 1, &width)
	for scancode in scancode_min .. scancode_max + 1 {
		if g_sapp_state.keycodes[scancode] == .invalid {
			base := usize((scancode - scancode_min) * width)
			g_sapp_state.keycodes[scancode] = x11_translate_keysyms(unsafe { keysyms + base },
				width)
		}
	}
	C.XFree(keysyms)
}

// === DPI ===

fn x11_query_system_dpi() {
	mut dpi_ok := false
	rms := C.XResourceManagerString(g_sapp_state.x11.display)
	if rms != unsafe { nil } {
		db := C.XrmGetStringDatabase(rms)
		if db != unsafe { nil } {
			mut value := C.XrmValue{}
			mut @type := &char(unsafe { nil })
			if C.XrmGetResource(db, c'Xft.dpi', c'Xft.Dpi', &@type, &value) != 0 {
				if @type != unsafe { nil } && C.strcmp(@type, c'String') == 0 {
					g_sapp_state.x11.dpi = f32(C.atof(value.addr))
					dpi_ok = true
				}
			}
			C.XrmDestroyDatabase(db)
		}
	}
	if !dpi_ok {
		g_sapp_state.x11.dpi = 96.0
	}
}

// === Keysym to unicode ===

fn x11_keysym_to_unicode(keysym KeySym) i32 {
	// Latin-1 characters (1:1 mapping)
	if (keysym >= 0x0020 && keysym <= 0x007e) || (keysym >= 0x00a0 && keysym <= 0x00ff) {
		return i32(keysym)
	}
	// Directly encoded 24-bit UCS characters
	if (keysym & 0xff000000) == 0x01000000 {
		return i32(keysym & 0x00ffffff)
	}
	// Binary search in table
	mut min := 0
	mut max := x11_keysymtab.len - 1
	for max >= min {
		mid := (min + max) / 2
		if u64(x11_keysymtab[mid].keysym) < keysym {
			min = mid + 1
		} else if u64(x11_keysymtab[mid].keysym) > keysym {
			max = mid - 1
		} else {
			return i32(x11_keysymtab[mid].ucs)
		}
	}
	return -1
}

// === Window management ===

fn x11_send_event(msg_type Atom, a i64, b i64, c_ i64, d i64, e i64) {
	mut event := C.XEvent{}
	unsafe {
		event.@type = x_client_message
		event.xclient.window = g_sapp_state.x11.window
		event.xclient.format = 32
		event.xclient.message_type = msg_type
		event.xclient.data.l[0] = a
		event.xclient.data.l[1] = b
		event.xclient.data.l[2] = c_
		event.xclient.data.l[3] = d
		event.xclient.data.l[4] = e
	}
	C.XSendEvent(g_sapp_state.x11.display, g_sapp_state.x11.root, x_false,
		substructure_notify_mask | substructure_redirect_mask, &event)
}

fn x11_wait_for_event(event_type int, timeout_sec f64, out_event &C.XEvent) bool {
	// Simple polling loop
	for {
		if C.XCheckTypedWindowEvent(g_sapp_state.x11.display, g_sapp_state.x11.window, event_type,
			out_event) != 0 {
			return true
		}
		mut fd := C.pollfd{
			fd:     C.ConnectionNumber(g_sapp_state.x11.display)
			events: pollin
		}
		C.poll(&fd, 1, int(timeout_sec * 1000))
		// Simplified timeout - just try once
		return C.XCheckTypedWindowEvent(g_sapp_state.x11.display, g_sapp_state.x11.window,
			event_type, out_event) != 0
	}
	return false
}

fn x11_app_event(event_type EventType) {
	if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
		init_sapp_event(event_type)
		call_sapp_event(&g_sapp_state.event)
	}
}

fn x11_roundf_gzero(v f32) int {
	if v >= 0.5 {
		return int(v + 0.5)
	} else {
		return int(v)
	}
}

fn x11_update_dimensions(x11_window_width int, x11_window_height int) {
	window_scale := g_sapp_state.x11.dpi / 96.0
	g_sapp_state.window_width = x11_roundf_gzero(f32(x11_window_width) / window_scale)
	g_sapp_state.window_height = x11_roundf_gzero(f32(x11_window_height) / window_scale)
	cur_fb_width := g_sapp_state.framebuffer_width
	cur_fb_height := g_sapp_state.framebuffer_height
	g_sapp_state.framebuffer_width =
		x11_roundf_gzero(f32(g_sapp_state.window_width) * g_sapp_state.dpi_scale)
	g_sapp_state.framebuffer_height =
		x11_roundf_gzero(f32(g_sapp_state.window_height) * g_sapp_state.dpi_scale)
	dim_changed := g_sapp_state.framebuffer_width != cur_fb_width
		|| g_sapp_state.framebuffer_height != cur_fb_height
	if dim_changed && !g_sapp_state.first_frame {
		x11_app_event(.resized)
	}
}

fn x11_update_dimensions_from_window_size() {
	mut attribs := C.XWindowAttributes{}
	C.XGetWindowAttributes(g_sapp_state.x11.display, g_sapp_state.x11.window, &attribs)
	x11_update_dimensions(attribs.width, attribs.height)
}

fn x11_set_fullscreen(enable bool) {
	if g_sapp_state.x11.net_wm_state != x_none && g_sapp_state.x11.net_wm_state_fullscreen != x_none {
		if enable {
			x11_send_event(g_sapp_state.x11.net_wm_state, 1, // _NET_WM_STATE_ADD
			 i64(g_sapp_state.x11.net_wm_state_fullscreen), 0, 1, 0)
		} else {
			x11_send_event(g_sapp_state.x11.net_wm_state, 0, // _NET_WM_STATE_REMOVE
			 i64(g_sapp_state.x11.net_wm_state_fullscreen), 0, 1, 0)
		}
	}
	C.XFlush(g_sapp_state.x11.display)
}

fn x11_create_window(visual_ &C.Visual, depth int) {
	mut vis := unsafe { visual_ }
	mut dep := depth
	if vis == unsafe { nil } {
		vis = C.XDefaultVisual(g_sapp_state.x11.display, g_sapp_state.x11.screen)
		dep = C.XDefaultDepth(g_sapp_state.x11.display, g_sapp_state.x11.screen)
	}
	g_sapp_state.x11.colormap = C.XCreateColormap(g_sapp_state.x11.display, g_sapp_state.x11.root,
		vis, alloc_none)
	mut wa := C.XSetWindowAttributes{}
	wa.colormap = g_sapp_state.x11.colormap
	wa.border_pixel = 0
	wa.event_mask = u64(structure_notify_mask | key_press_mask | key_release_mask | pointer_motion_mask | button_press_mask | button_release_mask | exposure_mask | focus_change_mask | visibility_change_mask | enter_window_mask | leave_window_mask | property_change_mask)

	display_width := C.XDisplayWidth(g_sapp_state.x11.display, g_sapp_state.x11.screen)
	display_height := C.XDisplayHeight(g_sapp_state.x11.display, g_sapp_state.x11.screen)
	window_scale := g_sapp_state.x11.dpi / 96.0
	mut x11_window_width := x11_roundf_gzero(f32(g_sapp_state.window_width) * window_scale)
	mut x11_window_height := x11_roundf_gzero(f32(g_sapp_state.window_height) * window_scale)
	if g_sapp_state.window_width == 0 {
		x11_window_width = (display_width * 4) / 5
	}
	if g_sapp_state.window_height == 0 {
		x11_window_height = (display_height * 4) / 5
	}
	// wamask = CWBorderPixel | CWColormap | CWEventMask
	wamask := u64(0x800 | 0x2000 | 0x800)
	x11_grab_error_handler()
	g_sapp_state.x11.window = C.XCreateWindow(g_sapp_state.x11.display, g_sapp_state.x11.root, 0,
		0, u32(x11_window_width), u32(x11_window_height), 0, dep, input_output, vis, wamask, &wa)
	x11_release_error_handler()
	if g_sapp_state.x11.window == 0 {
		eprintln('sokol_app: X11: failed to create window')
		return
	}
	mut protocols := [1]Atom{}
	protocols[0] = g_sapp_state.x11.wm_delete_window
	C.XSetWMProtocols(g_sapp_state.x11.display, g_sapp_state.x11.window, &protocols[0], 1)
	mut hints := C.XAllocSizeHints()
	hints.flags = pw_gravity
	hints.win_gravity = center_gravity
	C.XSetWMNormalHints(g_sapp_state.x11.display, g_sapp_state.x11.window, hints)
	C.XFree(hints)
	// Announce support for drag'n'drop
	if g_sapp_state.drop.enabled {
		version := Atom(x11_xdnd_version)
		C.XChangeProperty(g_sapp_state.x11.display, g_sapp_state.x11.window,
			g_sapp_state.x11.xdnd.xdnd_aware, xa_atom, 32, prop_mode_replace,
			unsafe { &u8(&version) }, 1)
	}
	x11_update_window_title()
	x11_update_dimensions_from_window_size()
}

fn x11_destroy_window() {
	if g_sapp_state.x11.window != 0 {
		C.XUnmapWindow(g_sapp_state.x11.display, g_sapp_state.x11.window)
		C.XDestroyWindow(g_sapp_state.x11.display, g_sapp_state.x11.window)
		g_sapp_state.x11.window = 0
	}
	if g_sapp_state.x11.colormap != 0 {
		C.XFreeColormap(g_sapp_state.x11.display, g_sapp_state.x11.colormap)
		g_sapp_state.x11.colormap = 0
	}
	C.XFlush(g_sapp_state.x11.display)
}

fn x11_window_visible() bool {
	mut wa := C.XWindowAttributes{}
	C.XGetWindowAttributes(g_sapp_state.x11.display, g_sapp_state.x11.window, &wa)
	return wa.map_state == is_viewable
}

fn x11_show_window() {
	if !x11_window_visible() {
		C.XMapWindow(g_sapp_state.x11.display, g_sapp_state.x11.window)
		mut dummy := C.XEvent{}
		x11_wait_for_event(visibility_notify, 0.1, &dummy)
		C.XRaiseWindow(g_sapp_state.x11.display, g_sapp_state.x11.window)
		C.XFlush(g_sapp_state.x11.display)
	}
}

fn x11_update_window_title() {
	title := &char(&g_sapp_state.window_title[0])
	C.Xutf8SetWMProperties(g_sapp_state.x11.display, g_sapp_state.x11.window, title, title,
		unsafe { nil }, 0, unsafe { nil }, unsafe { nil }, unsafe { nil })
	C.XChangeProperty(g_sapp_state.x11.display, g_sapp_state.x11.window,
		g_sapp_state.x11.net_wm_name, g_sapp_state.x11.utf8_string, 8, prop_mode_replace,
		&g_sapp_state.window_title[0], int(C.strlen(title)))
	C.XChangeProperty(g_sapp_state.x11.display, g_sapp_state.x11.window,
		g_sapp_state.x11.net_wm_icon_name, g_sapp_state.x11.utf8_string, 8, prop_mode_replace,
		&g_sapp_state.window_title[0], int(C.strlen(title)))
	C.XFlush(g_sapp_state.x11.display)
}

// === Cursors ===

fn x11_create_hidden_cursor() {
	mut img := C.XcursorImageCreate(16, 16)
	if img == unsafe { nil } {
		return
	}
	img.xhot = 0
	img.yhot = 0
	unsafe { C.memset(img.pixels, 0, usize(16 * 16 * 4)) }
	g_sapp_state.x11.hidden_cursor = C.XcursorImageLoadCursor(g_sapp_state.x11.display, img)
	C.XcursorImageDestroy(img)
}

fn x11_create_standard_cursor(cursor MouseCursor, name &char, theme &char, size int, fallback_native u32) {
	idx := int(cursor)
	if theme != unsafe { nil } {
		img := C.XcursorLibraryLoadImage(name, theme, size)
		if img != unsafe { nil } {
			g_sapp_state.x11.standard_cursors[idx] =
				C.XcursorImageLoadCursor(g_sapp_state.x11.display, img)
			C.XcursorImageDestroy(img)
		}
	}
	if g_sapp_state.x11.standard_cursors[idx] == 0 && fallback_native != 0 {
		g_sapp_state.x11.standard_cursors[idx] = C.XCreateFontCursor(g_sapp_state.x11.display,
			fallback_native)
	}
}

fn x11_create_standard_cursors() {
	cursor_theme := C.XcursorGetTheme(g_sapp_state.x11.display)
	size := C.XcursorGetDefaultSize(g_sapp_state.x11.display)
	x11_create_standard_cursor(.arrow, c'default', cursor_theme, size, xc_left_ptr)
	x11_create_standard_cursor(.ibeam, c'text', cursor_theme, size, xc_xterm)
	x11_create_standard_cursor(.crosshair, c'crosshair', cursor_theme, size, xc_crosshair)
	x11_create_standard_cursor(.pointing_hand, c'pointer', cursor_theme, size, xc_hand2)
	x11_create_standard_cursor(.resize_ew, c'ew-resize', cursor_theme, size, xc_sb_h_double_arrow)
	x11_create_standard_cursor(.resize_ns, c'ns-resize', cursor_theme, size, xc_sb_v_double_arrow)
	x11_create_standard_cursor(.resize_nwse, c'nwse-resize', cursor_theme, size, 0)
	x11_create_standard_cursor(.resize_nesw, c'nesw-resize', cursor_theme, size, 0)
	x11_create_standard_cursor(.resize_all, c'all-scroll', cursor_theme, size, xc_fleur)
	x11_create_standard_cursor(.not_allowed, c'no-allowed', cursor_theme, size, 0)
	x11_create_hidden_cursor()
}

fn x11_destroy_standard_cursors() {
	if g_sapp_state.x11.hidden_cursor != 0 {
		C.XFreeCursor(g_sapp_state.x11.display, g_sapp_state.x11.hidden_cursor)
		g_sapp_state.x11.hidden_cursor = 0
	}
	for i in 0 .. mousecursor_num {
		if g_sapp_state.x11.standard_cursors[i] != 0 {
			C.XFreeCursor(g_sapp_state.x11.display, g_sapp_state.x11.standard_cursors[i])
			g_sapp_state.x11.standard_cursors[i] = 0
		}
	}
}

fn x11_toggle_fullscreen() {
	g_sapp_state.fullscreen = !g_sapp_state.fullscreen
	x11_set_fullscreen(g_sapp_state.fullscreen)
	x11_update_dimensions_from_window_size()
}

fn x11_update_cursor(cursor MouseCursor, shown bool) {
	idx := int(cursor)
	if shown {
		if g_sapp_state.custom_cursor_bound[idx] {
			xcursor := g_sapp_state.x11.custom_cursors[idx]
			if xcursor != 0 {
				C.XDefineCursor(g_sapp_state.x11.display, g_sapp_state.x11.window, xcursor)
			}
		} else if g_sapp_state.x11.standard_cursors[idx] != 0 {
			C.XDefineCursor(g_sapp_state.x11.display, g_sapp_state.x11.window,
				g_sapp_state.x11.standard_cursors[idx])
		} else {
			C.XUndefineCursor(g_sapp_state.x11.display, g_sapp_state.x11.window)
		}
	} else {
		C.XDefineCursor(g_sapp_state.x11.display, g_sapp_state.x11.window,
			g_sapp_state.x11.hidden_cursor)
	}
	C.XFlush(g_sapp_state.x11.display)
}

// === Mouse lock ===

fn x11_lock_mouse(do_lock bool) {
	if do_lock == g_sapp_state.mouse.locked {
		return
	}
	g_sapp_state.mouse.dx = 0.0
	g_sapp_state.mouse.dy = 0.0
	g_sapp_state.mouse.locked = do_lock
	if g_sapp_state.mouse.locked {
		if g_sapp_state.x11.xi.available {
			mut em := C.XIEventMask{}
			mut mask := [4]u8{}
			em.deviceid = xi_all_master_devices
			em.mask_len = int(sizeof(mask))
			em.mask = unsafe { &mask[0] }
			xi_set_mask(unsafe { &mask[0] }, xi_raw_motion)
			C.XISelectEvents(g_sapp_state.x11.display, g_sapp_state.x11.root, &em, 1)
		}
		C.XGrabPointer(g_sapp_state.x11.display, g_sapp_state.x11.window, x_true,
			u32(button_press_mask | button_release_mask | pointer_motion_mask), grab_mode_async,
			grab_mode_async, g_sapp_state.x11.window, g_sapp_state.x11.hidden_cursor, current_time)
	} else {
		if g_sapp_state.x11.xi.available {
			mut em := C.XIEventMask{}
			mut mask := [1]u8{}
			em.deviceid = xi_all_master_devices
			em.mask_len = int(sizeof(mask))
			em.mask = unsafe { &mask[0] }
			C.XISelectEvents(g_sapp_state.x11.display, g_sapp_state.x11.root, &em, 1)
		}
		C.XWarpPointer(g_sapp_state.x11.display, Window(0), g_sapp_state.x11.window, 0, 0, 0, 0,
			int(g_sapp_state.mouse.x), int(g_sapp_state.mouse.y))
		C.XUngrabPointer(g_sapp_state.x11.display, current_time)
	}
	C.XFlush(g_sapp_state.x11.display)
}

// === Clipboard ===

fn x11_set_clipboard_string(_str &char) {
	if !g_sapp_state.clipboard.enabled || g_sapp_state.clipboard.buffer == unsafe { nil } {
		return
	}
	C.XSetSelectionOwner(g_sapp_state.x11.display, g_sapp_state.x11.clipboard_atom,
		g_sapp_state.x11.window, current_time)
}

fn x11_get_clipboard_string() &char {
	if !g_sapp_state.clipboard.enabled || g_sapp_state.clipboard.buffer == unsafe { nil } {
		return unsafe { nil }
	}
	// If we own the selection, just return our buffer
	if C.XGetSelectionOwner(g_sapp_state.x11.display, g_sapp_state.x11.clipboard_atom) == g_sapp_state.x11.window {
		return g_sapp_state.clipboard.buffer
	}
	sapp_selection := C.XInternAtom(g_sapp_state.x11.display, c'SAPP_SELECTION', x_false)
	C.XConvertSelection(g_sapp_state.x11.display, g_sapp_state.x11.clipboard_atom,
		g_sapp_state.x11.utf8_string, sapp_selection, g_sapp_state.x11.window, current_time)
	mut event := C.XEvent{}
	if !x11_wait_for_event(x_selection_notify, 0.1, &event) {
		return unsafe { nil }
	}
	unsafe {
		if event.xselection.property == x_none {
			return nil
		}
		mut data := &u8(nil)
		mut actual_type := Atom(0)
		mut actual_format := 0
		mut item_count := u64(0)
		mut bytes_after := u64(0)
		C.XGetWindowProperty(g_sapp_state.x11.display, event.xselection.requestor,
			event.xselection.property, 0, 0x7fffffff, x_true, g_sapp_state.x11.utf8_string,
			&actual_type, &actual_format, &item_count, &bytes_after, &&u8(&data))
		if data == nil {
			return nil
		}
		if item_count >= usize(g_sapp_state.clipboard.buf_size) {
			C.XFree(data)
			return nil
		}
		C.memcpy(g_sapp_state.clipboard.buffer, data, usize(item_count))
		g_sapp_state.clipboard.buffer[item_count] = 0
		C.XFree(data)
		return g_sapp_state.clipboard.buffer
	}
}

// === Modifier helpers ===

fn x11_key_modifier_bit(key KeyCode) u32 {
	return match key {
		.left_shift, .right_shift { u32(Modifier.shift) }
		.left_control, .right_control { u32(Modifier.ctrl) }
		.left_alt, .right_alt { u32(Modifier.alt) }
		.left_super, .right_super { u32(Modifier.super) }
		else { u32(0) }
	}
}

fn x11_button_modifier_bit(btn MouseButton) u32 {
	return match btn {
		.left { u32(Modifier.lmb) }
		.right { u32(Modifier.rmb) }
		.middle { u32(Modifier.mmb) }
		else { u32(0) }
	}
}

fn x11_mods(x11_mods_ u32) u32 {
	mut mods := u32(0)
	if x11_mods_ & shift_mask != 0 {
		mods |= u32(Modifier.shift)
	}
	if x11_mods_ & control_mask != 0 {
		mods |= u32(Modifier.ctrl)
	}
	if x11_mods_ & mod1_mask != 0 {
		mods |= u32(Modifier.alt)
	}
	if x11_mods_ & mod4_mask != 0 {
		mods |= u32(Modifier.super)
	}
	if x11_mods_ & button1_mask != 0 {
		mods |= u32(Modifier.lmb)
	}
	if x11_mods_ & button2_mask != 0 {
		mods |= u32(Modifier.mmb)
	}
	if x11_mods_ & button3_mask != 0 {
		mods |= u32(Modifier.rmb)
	}
	return mods
}

fn x11_translate_button(event &C.XEvent) MouseButton {
	return match unsafe { event.xbutton.button } {
		x_button1 { MouseButton.left }
		x_button2 { MouseButton.middle }
		x_button3 { MouseButton.right }
		else { MouseButton.invalid }
	}
}

// === Mouse/key event helpers ===

fn x11_mouse_update(x int, y int, clear_dxdy bool) {
	if !g_sapp_state.mouse.locked {
		new_x := f32(x)
		new_y := f32(y)
		if clear_dxdy {
			g_sapp_state.mouse.dx = 0.0
			g_sapp_state.mouse.dy = 0.0
		} else if g_sapp_state.mouse.pos_valid {
			g_sapp_state.mouse.dx = new_x - g_sapp_state.mouse.x
			g_sapp_state.mouse.dy = new_y - g_sapp_state.mouse.y
		}
		g_sapp_state.mouse.x = new_x
		g_sapp_state.mouse.y = new_y
		g_sapp_state.mouse.pos_valid = true
	}
}

fn x11_mouse_event(event_type EventType, btn MouseButton, mods u32) {
	if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
		init_sapp_event(event_type)
		g_sapp_state.event.mouse_button = btn
		g_sapp_state.event.modifiers = mods
		call_sapp_event(&g_sapp_state.event)
	}
}

fn x11_scroll_event(x f32, y f32, mods u32) {
	if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
		init_sapp_event(.mouse_scroll)
		g_sapp_state.event.modifiers = mods
		g_sapp_state.event.scroll_x = x
		g_sapp_state.event.scroll_y = y
		call_sapp_event(&g_sapp_state.event)
	}
}

fn x11_key_event(event_type EventType, key KeyCode, repeat bool, mods u32) {
	if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
		init_sapp_event(event_type)
		g_sapp_state.event.key_code = key
		g_sapp_state.event.key_repeat = repeat
		g_sapp_state.event.modifiers = mods
		call_sapp_event(&g_sapp_state.event)
		// Check if a CLIPBOARD_PASTED event must be sent too
		if g_sapp_state.clipboard.enabled && event_type == .key_down
			&& g_sapp_state.event.modifiers == u32(Modifier.ctrl)
			&& g_sapp_state.event.key_code == .v {
			init_sapp_event(.clipboard_pasted)
			call_sapp_event(&g_sapp_state.event)
		}
	}
}

fn x11_char_event(chr u32, repeat bool, mods u32) {
	if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
		init_sapp_event(.char)
		g_sapp_state.event.char_code = chr
		g_sapp_state.event.key_repeat = repeat
		g_sapp_state.event.modifiers = mods
		call_sapp_event(&g_sapp_state.event)
	}
}

fn x11_translate_key(scancode int) KeyCode {
	if scancode >= 0 && scancode < x11_max_keycodes {
		return g_sapp_state.keycodes[scancode]
	}
	return .invalid
}

fn x11_keypress_repeat(keycode int) bool {
	mut repeat := false
	if keycode >= 0 && keycode < x11_max_keycodes {
		repeat = g_sapp_state.x11.key_repeat[keycode]
		g_sapp_state.x11.key_repeat[keycode] = true
	}
	return repeat
}

fn x11_keyrelease_repeat(keycode int) {
	if keycode >= 0 && keycode < x11_max_keycodes {
		g_sapp_state.x11.key_repeat[keycode] = false
	}
}

// === Window property helpers ===

fn x11_get_window_property(window Window, property Atom, req_type Atom, value &&u8) u64 {
	mut actual_type := Atom(0)
	mut actual_format := 0
	mut item_count := u64(0)
	mut bytes_after := u64(0)
	C.XGetWindowProperty(g_sapp_state.x11.display, window, property, 0, 0x7fffffff, x_false,
		req_type, &actual_type, &actual_format, &item_count, &bytes_after, unsafe { &&&u8(value) })
	return item_count
}

fn x11_get_window_state() int {
	mut result := withdrawn_state
	mut state := &u8(unsafe { nil })
	if x11_get_window_property(g_sapp_state.x11.window, g_sapp_state.x11.wm_state,
		g_sapp_state.x11.wm_state, &&u8(&state)) >= 2 {
		unsafe {
			result = int(*(&u32(state)))
		}
	}
	if state != unsafe { nil } {
		C.XFree(state)
	}
	return result
}

// === Dropped files ===

fn x11_parse_dropped_files_list(src &char) bool {
	if src == unsafe { nil } || g_sapp_state.drop.buffer == unsafe { nil } {
		return false
	}
	// Clear drop buffer
	unsafe { C.memset(g_sapp_state.drop.buffer, 0, usize(g_sapp_state.drop.buf_size)) }
	g_sapp_state.drop.num_files = 0

	mut err := false
	mut src_count := 0
	mut dst_ptr := g_sapp_state.drop.buffer
	dst_end_ptr := unsafe { g_sapp_state.drop.buffer + (g_sapp_state.drop.max_path_length - 1) }
	mut p := unsafe { src }
	for {
		src_chr := unsafe { *p }
		if src_chr == 0 {
			break
		}
		unsafe { p++ }
		src_count++
		mut dst_chr := u8(0)
		// Check leading 'file://'
		if src_count <= 7 {
			valid := (src_count == 1 && src_chr == `f`)
				|| (src_count == 2 && src_chr == `i`)
				|| (src_count == 3 && src_chr == `l`)
				|| (src_count == 4 && src_chr == `e`)
				|| (src_count == 5 && src_chr == `:`)
				|| (src_count == 6 && src_chr == `/`)
				|| (src_count == 7 && src_chr == `/`)
			if !valid {
				err = true
				break
			}
		} else if src_chr == `\r` {
			// skip
		} else if src_chr == `\n` {
			src_count = 0
			g_sapp_state.drop.num_files++
			if g_sapp_state.drop.num_files >= g_sapp_state.drop.max_files {
				break
			}
			dst_ptr = unsafe {
				g_sapp_state.drop.buffer +
					g_sapp_state.drop.num_files * g_sapp_state.drop.max_path_length
			}
		} else if src_chr == `%` {
			unsafe {
				if *p != 0 && *(p + 1) != 0 {
					mut digits := [3]u8{}
					digits[0] = u8(*p)
					digits[1] = u8(*(p + 1))
					digits[2] = 0
					p = &char(usize(p) + 2)
					dst_chr = u8(C.strtol(&char(&digits[0]), &char(0), 16))
				}
			}
		} else {
			dst_chr = u8(src_chr)
		}
		if dst_chr != 0 {
			if usize(dst_ptr) < usize(dst_end_ptr) {
				unsafe {
					*dst_ptr = char(dst_chr)
					dst_ptr++
				}
			} else {
				err = true
				break
			}
		}
	}
	if err {
		unsafe { C.memset(g_sapp_state.drop.buffer, 0, usize(g_sapp_state.drop.buf_size)) }
		g_sapp_state.drop.num_files = 0
		return false
	}
	return true
}

// === Event handlers ===

fn x11_on_genericevent(event &C.XEvent) {
	if g_sapp_state.mouse.locked && g_sapp_state.x11.xi.available {
		unsafe {
			if event.xcookie.extension == g_sapp_state.x11.xi.major_opcode {
				if C.XGetEventData(g_sapp_state.x11.display, &event.xcookie) != 0 {
					if event.xcookie.evtype == xi_raw_motion {
						re := &C.XIRawEvent(event.xcookie.data)
						if re.valuators.mask_len > 0 {
							mut values := re.raw_values
							if xi_mask_is_set(re.valuators.mask, 0) {
								g_sapp_state.mouse.dx = f32(*values)
								values = values + 1
							}
							if xi_mask_is_set(re.valuators.mask, 1) {
								g_sapp_state.mouse.dy = f32(*values)
							}
							x11_mouse_event(.mouse_move, .invalid, x11_mods(event.xmotion.state))
						}
					}
					C.XFreeEventData(g_sapp_state.x11.display, &event.xcookie)
				}
			}
		}
	}
}

fn x11_on_focusin(event &C.XEvent) {
	unsafe {
		if event.xfocus.mode != notify_grab && event.xfocus.mode != notify_ungrab {
			x11_app_event(.focused)
		}
	}
}

fn x11_on_focusout(event &C.XEvent) {
	if g_sapp_state.mouse.locked {
		x11_lock_mouse(false)
	}
	unsafe {
		if event.xfocus.mode != notify_grab && event.xfocus.mode != notify_ungrab {
			x11_app_event(.unfocused)
		}
	}
}

fn x11_on_keypress(event &C.XEvent) {
	unsafe {
		keycode := int(event.xkey.keycode)
		key := x11_translate_key(keycode)
		repeat := x11_keypress_repeat(keycode)
		mut mods := x11_mods(event.xkey.state)
		// X11 doesn't set modifier bit on key down, so emulate that
		mods |= x11_key_modifier_bit(key)
		if key != .invalid {
			x11_key_event(.key_down, key, repeat, mods)
		}
		mut keysym := KeySym(0)
		C.XLookupString(&event.xkey, nil, 0, &keysym, nil)
		chr := x11_keysym_to_unicode(keysym)
		if chr > 0 {
			x11_char_event(u32(chr), repeat, mods)
		}
	}
}

fn x11_on_keyrelease(event &C.XEvent) {
	unsafe {
		keycode := int(event.xkey.keycode)
		key := x11_translate_key(keycode)
		x11_keyrelease_repeat(keycode)
		if key != .invalid {
			mut mods := x11_mods(event.xkey.state)
			// X11 doesn't clear modifier bit on key up, so emulate that
			mods &= ~x11_key_modifier_bit(key)
			x11_key_event(.key_up, key, false, mods)
		}
	}
}

fn x11_on_buttonpress(event &C.XEvent) {
	unsafe {
		x11_mouse_update(event.xbutton.x, event.xbutton.y, false)
		btn := x11_translate_button(event)
		mut mods := x11_mods(event.xbutton.state)
		mods |= x11_button_modifier_bit(btn)
		if btn != .invalid {
			x11_mouse_event(.mouse_down, btn, mods)
			g_sapp_state.x11.mouse_buttons |= u8(1 << int(btn))
		} else {
			// Might be a scroll event
			match event.xbutton.button {
				4 { x11_scroll_event(0.0, 1.0, mods) }
				5 { x11_scroll_event(0.0, -1.0, mods) }
				6 { x11_scroll_event(1.0, 0.0, mods) }
				7 { x11_scroll_event(-1.0, 0.0, mods) }
				else {}
			}
		}
	}
}

fn x11_on_buttonrelease(event &C.XEvent) {
	unsafe {
		x11_mouse_update(event.xbutton.x, event.xbutton.y, false)
		btn := x11_translate_button(event)
		if btn != .invalid {
			mut mods := x11_mods(event.xbutton.state)
			mods &= ~x11_button_modifier_bit(btn)
			x11_mouse_event(.mouse_up, btn, mods)
			g_sapp_state.x11.mouse_buttons &= ~u8(1 << int(btn))
		}
	}
}

fn x11_on_enternotify(event &C.XEvent) {
	if g_sapp_state.x11.mouse_buttons == 0 {
		unsafe {
			x11_mouse_update(event.xcrossing.x, event.xcrossing.y, true)
			x11_mouse_event(.mouse_enter, .invalid, x11_mods(event.xcrossing.state))
		}
	}
}

fn x11_on_leavenotify(event &C.XEvent) {
	if g_sapp_state.x11.mouse_buttons == 0 {
		unsafe {
			x11_mouse_update(event.xcrossing.x, event.xcrossing.y, true)
			x11_mouse_event(.mouse_leave, .invalid, x11_mods(event.xcrossing.state))
		}
	}
}

fn x11_on_motionnotify(event &C.XEvent) {
	if !g_sapp_state.mouse.locked {
		unsafe {
			x11_mouse_update(event.xmotion.x, event.xmotion.y, false)
			x11_mouse_event(.mouse_move, .invalid, x11_mods(event.xmotion.state))
		}
	}
}

fn x11_on_propertynotify(event &C.XEvent) {
	unsafe {
		if event.xproperty.state == property_new_value {
			if event.xproperty.atom == g_sapp_state.x11.wm_state {
				state := x11_get_window_state()
				if state != g_sapp_state.x11.window_state {
					g_sapp_state.x11.window_state = state
					if state == iconic_state {
						x11_app_event(.iconified)
					} else if state == normal_state {
						x11_app_event(.restored)
					}
				}
			}
		}
	}
}

fn x11_on_selectionnotify(event &C.XEvent) {
	unsafe {
		if event.xselection.property == g_sapp_state.x11.xdnd.xdnd_selection {
			mut data := &u8(nil)
			result := x11_get_window_property(event.xselection.requestor,
				event.xselection.property, event.xselection.target, &&u8(&data))
			if g_sapp_state.drop.enabled && result > 0 {
				if x11_parse_dropped_files_list(&char(data)) {
					g_sapp_state.mouse.dx = 0.0
					g_sapp_state.mouse.dy = 0.0
					if g_sapp_state.init_called && !g_sapp_state.cleanup_called {
						init_sapp_event(.files_dropped)
						call_sapp_event(&g_sapp_state.event)
					}
				}
			}
			if g_sapp_state.x11.xdnd.version >= 2 {
				mut reply := C.XEvent{}
				reply.@type = x_client_message
				reply.xclient.window = g_sapp_state.x11.xdnd.source
				reply.xclient.message_type = g_sapp_state.x11.xdnd.xdnd_finished
				reply.xclient.format = 32
				reply.xclient.data.l[0] = i64(g_sapp_state.x11.window)
				reply.xclient.data.l[1] = i64(result)
				reply.xclient.data.l[2] = i64(g_sapp_state.x11.xdnd.xdnd_action_copy)
				C.XSendEvent(g_sapp_state.x11.display, g_sapp_state.x11.xdnd.source, x_false,
					no_event_mask, &reply)
				C.XFlush(g_sapp_state.x11.display)
			}
			if data != nil {
				C.XFree(data)
			}
		}
	}
}

fn x11_on_selectionrequest(event &C.XEvent) {
	unsafe {
		req := &event.xselectionrequest
		if req.selection != g_sapp_state.x11.clipboard_atom {
			return
		}
		if !g_sapp_state.clipboard.enabled {
			return
		}
		mut reply := C.XEvent{}
		reply.@type = x_selection_notify
		reply.xselection.requestor = req.requestor
		reply.xselection.property = req.property
		reply.xselection.target = req.target

		if req.target == g_sapp_state.x11.utf8_string {
			C.XChangeProperty(g_sapp_state.x11.display, req.requestor, req.property,
				g_sapp_state.x11.utf8_string, 8, prop_mode_replace,
				&u8(g_sapp_state.clipboard.buffer), int(C.strlen(g_sapp_state.clipboard.buffer)))
		} else if req.target == g_sapp_state.x11.targets {
			C.XChangeProperty(g_sapp_state.x11.display, req.requestor, req.property, xa_atom, 32,
				prop_mode_replace, &u8(&g_sapp_state.x11.utf8_string), 1)
		} else {
			reply.xselection.property = x_none
		}
		C.XSendEvent(g_sapp_state.x11.display, req.requestor, x_false, 0, &reply)
	}
}

fn x11_on_clientmessage(event &C.XEvent) {
	if C.XFilterEvent(event, Window(0)) != 0 {
		return
	}
	unsafe {
		if event.xclient.message_type == g_sapp_state.x11.wm_protocols {
			protocol := Atom(u64(event.xclient.data.l[0]))
			if protocol == g_sapp_state.x11.wm_delete_window {
				g_sapp_state.quit_requested = true
			}
		} else if event.xclient.message_type == g_sapp_state.x11.xdnd.xdnd_enter {
			is_list := (event.xclient.data.l[1] & 1) != 0
			g_sapp_state.x11.xdnd.source = Window(u64(event.xclient.data.l[0]))
			g_sapp_state.x11.xdnd.version = event.xclient.data.l[1] >> 24
			g_sapp_state.x11.xdnd.format = x_none
			if g_sapp_state.x11.xdnd.version > x11_xdnd_version {
				return
			}
			mut count := u64(0)
			mut formats := &Atom(nil)
			if is_list {
				count = x11_get_window_property(g_sapp_state.x11.xdnd.source,
					g_sapp_state.x11.xdnd.xdnd_type_list, xa_atom, &&u8(&formats))
			} else {
				count = 3
				formats = &Atom(voidptr(&event.xclient.data.l[2]))
			}
			for i in 0 .. count {
				if formats[i] == g_sapp_state.x11.xdnd.text_uri_list {
					g_sapp_state.x11.xdnd.format = g_sapp_state.x11.xdnd.text_uri_list
					break
				}
			}
			if is_list && formats != nil {
				C.XFree(formats)
			}
		} else if event.xclient.message_type == g_sapp_state.x11.xdnd.xdnd_drop {
			if g_sapp_state.x11.xdnd.version > x11_xdnd_version {
				return
			}
			if g_sapp_state.x11.xdnd.format != x_none {
				mut time := current_time
				if g_sapp_state.x11.xdnd.version >= 1 {
					time = Time(u64(event.xclient.data.l[2]))
				}
				C.XConvertSelection(g_sapp_state.x11.display, g_sapp_state.x11.xdnd.xdnd_selection,
					g_sapp_state.x11.xdnd.format, g_sapp_state.x11.xdnd.xdnd_selection,
					g_sapp_state.x11.window, time)
			} else if g_sapp_state.x11.xdnd.version >= 2 {
				mut reply := C.XEvent{}
				reply.@type = x_client_message
				reply.xclient.window = g_sapp_state.x11.xdnd.source
				reply.xclient.message_type = g_sapp_state.x11.xdnd.xdnd_finished
				reply.xclient.format = 32
				reply.xclient.data.l[0] = i64(g_sapp_state.x11.window)
				reply.xclient.data.l[1] = 0
				reply.xclient.data.l[2] = i64(x_none)
				C.XSendEvent(g_sapp_state.x11.display, g_sapp_state.x11.xdnd.source, x_false,
					no_event_mask, &reply)
				C.XFlush(g_sapp_state.x11.display)
			}
		} else if event.xclient.message_type == g_sapp_state.x11.xdnd.xdnd_position {
			if g_sapp_state.x11.xdnd.version > x11_xdnd_version {
				return
			}
			mut reply := C.XEvent{}
			reply.@type = x_client_message
			reply.xclient.window = g_sapp_state.x11.xdnd.source
			reply.xclient.message_type = g_sapp_state.x11.xdnd.xdnd_status
			reply.xclient.format = 32
			reply.xclient.data.l[0] = i64(g_sapp_state.x11.window)
			if g_sapp_state.x11.xdnd.format != x_none {
				reply.xclient.data.l[1] = 1
				if g_sapp_state.x11.xdnd.version >= 2 {
					reply.xclient.data.l[4] = i64(g_sapp_state.x11.xdnd.xdnd_action_copy)
				}
			}
			C.XSendEvent(g_sapp_state.x11.display, g_sapp_state.x11.xdnd.source, x_false,
				no_event_mask, &reply)
			C.XFlush(g_sapp_state.x11.display)
		}
	}
}

// === Main event dispatcher ===

fn x11_process_event(event &C.XEvent) {
	unsafe {
		match event.@type {
			x_generic_event { x11_on_genericevent(event) }
			x_focus_in { x11_on_focusin(event) }
			x_focus_out { x11_on_focusout(event) }
			x_key_press { x11_on_keypress(event) }
			x_key_release { x11_on_keyrelease(event) }
			x_button_press { x11_on_buttonpress(event) }
			x_button_release { x11_on_buttonrelease(event) }
			x_enter_notify { x11_on_enternotify(event) }
			x_leave_notify { x11_on_leavenotify(event) }
			x_motion_notify { x11_on_motionnotify(event) }
			x_property_notify { x11_on_propertynotify(event) }
			x_selection_notify { x11_on_selectionnotify(event) }
			x_selection_request { x11_on_selectionrequest(event) }
			x_destroy_notify {}
			x_client_message { x11_on_clientmessage(event) }
			else {}
		}
	}
}

// === Frame and run functions ===

fn x11_linux_frame() {
	x11_update_dimensions_from_window_size()
	sapp_do_frame()
	C.eglSwapBuffers(g_sapp_state.egl.display, g_sapp_state.egl.surface)
}

fn x11_run() {
	C.XInitThreads()
	C.XrmInitialize()
	g_sapp_state.x11.display = C.XOpenDisplay(unsafe { nil })
	if g_sapp_state.x11.display == unsafe { nil } {
		eprintln('sokol_app: X11: failed to open display')
		return
	}
	g_sapp_state.x11.screen = C.XDefaultScreen(g_sapp_state.x11.display)
	g_sapp_state.x11.root = C.XDefaultRootWindow(g_sapp_state.x11.display)
	g_sapp_state.x11.window_state = normal_state
	x11_query_system_dpi()
	g_sapp_state.dpi_scale = g_sapp_state.x11.dpi / 96.0
	x11_init_extensions()
	x11_create_standard_cursors()
	C.XkbSetDetectableAutoRepeat(g_sapp_state.x11.display, x_true, unsafe { nil })
	x11_init_keytable()

	// EGL init (creates window internally)
	sapp_egl_init_x11()

	g_sapp_state.valid = true
	x11_show_window()
	if g_sapp_state.fullscreen {
		x11_set_fullscreen(true)
	}
	C.XFlush(g_sapp_state.x11.display)

	for !g_sapp_state.quit_ordered {
		count := C.XPending(g_sapp_state.x11.display)
		for _ in 0 .. count {
			mut event := C.XEvent{}
			C.XNextEvent(g_sapp_state.x11.display, &event)
			x11_process_event(&event)
		}
		x11_linux_frame()
		C.XFlush(g_sapp_state.x11.display)
		// Handle quit-requested
		if g_sapp_state.quit_requested && !g_sapp_state.quit_ordered {
			x11_app_event(.quit_requested)
			if g_sapp_state.quit_requested {
				g_sapp_state.quit_ordered = true
			}
		}
	}
	sapp_call_cleanup()
	sapp_egl_destroy()
	x11_destroy_window()
	x11_destroy_standard_cursors()
	C.XCloseDisplay(g_sapp_state.x11.display)
	sapp_discard_state()
}
