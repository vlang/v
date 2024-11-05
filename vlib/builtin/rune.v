// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module builtin

import strings

const upper_lower = 5000000
// Quotes from golang source code `src/unicode/tables.go`
const rune_maps = [
	RuneMap{0x0041, 0x005A, 0, 32, 0},
	RuneMap{0x0061, 0x007A, -32, 0, -32},
	RuneMap{0x00B5, 0x00B5, 743, 0, 743},
	RuneMap{0x00C0, 0x00D6, 0, 32, 0},
	RuneMap{0x00D8, 0x00DE, 0, 32, 0},
	RuneMap{0x00E0, 0x00F6, -32, 0, -32},
	RuneMap{0x00F8, 0x00FE, -32, 0, -32},
	RuneMap{0x00FF, 0x00FF, 121, 0, 121},
	RuneMap{0x0100, 0x012F, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0130, 0x0130, 0, -199, 0},
	RuneMap{0x0131, 0x0131, -232, 0, -232},
	RuneMap{0x0132, 0x0137, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0139, 0x0148, upper_lower, upper_lower, upper_lower},
	RuneMap{0x014A, 0x0177, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0178, 0x0178, 0, -121, 0},
	RuneMap{0x0179, 0x017E, upper_lower, upper_lower, upper_lower},
	RuneMap{0x017F, 0x017F, -300, 0, -300},
	RuneMap{0x0180, 0x0180, 195, 0, 195},
	RuneMap{0x0181, 0x0181, 0, 210, 0},
	RuneMap{0x0182, 0x0185, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0186, 0x0186, 0, 206, 0},
	RuneMap{0x0187, 0x0188, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0189, 0x018A, 0, 205, 0},
	RuneMap{0x018B, 0x018C, upper_lower, upper_lower, upper_lower},
	RuneMap{0x018E, 0x018E, 0, 79, 0},
	RuneMap{0x018F, 0x018F, 0, 202, 0},
	RuneMap{0x0190, 0x0190, 0, 203, 0},
	RuneMap{0x0191, 0x0192, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0193, 0x0193, 0, 205, 0},
	RuneMap{0x0194, 0x0194, 0, 207, 0},
	RuneMap{0x0195, 0x0195, 97, 0, 97},
	RuneMap{0x0196, 0x0196, 0, 211, 0},
	RuneMap{0x0197, 0x0197, 0, 209, 0},
	RuneMap{0x0198, 0x0199, upper_lower, upper_lower, upper_lower},
	RuneMap{0x019A, 0x019A, 163, 0, 163},
	RuneMap{0x019C, 0x019C, 0, 211, 0},
	RuneMap{0x019D, 0x019D, 0, 213, 0},
	RuneMap{0x019E, 0x019E, 130, 0, 130},
	RuneMap{0x019F, 0x019F, 0, 214, 0},
	RuneMap{0x01A0, 0x01A5, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01A6, 0x01A6, 0, 218, 0},
	RuneMap{0x01A7, 0x01A8, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01A9, 0x01A9, 0, 218, 0},
	RuneMap{0x01AC, 0x01AD, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01AE, 0x01AE, 0, 218, 0},
	RuneMap{0x01AF, 0x01B0, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01B1, 0x01B2, 0, 217, 0},
	RuneMap{0x01B3, 0x01B6, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01B7, 0x01B7, 0, 219, 0},
	RuneMap{0x01B8, 0x01B9, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01BC, 0x01BD, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01BF, 0x01BF, 56, 0, 56},
	RuneMap{0x01C4, 0x01C4, 0, 2, 1},
	RuneMap{0x01C5, 0x01C5, -1, 1, 0},
	RuneMap{0x01C6, 0x01C6, -2, 0, -1},
	RuneMap{0x01C7, 0x01C7, 0, 2, 1},
	RuneMap{0x01C8, 0x01C8, -1, 1, 0},
	RuneMap{0x01C9, 0x01C9, -2, 0, -1},
	RuneMap{0x01CA, 0x01CA, 0, 2, 1},
	RuneMap{0x01CB, 0x01CB, -1, 1, 0},
	RuneMap{0x01CC, 0x01CC, -2, 0, -1},
	RuneMap{0x01CD, 0x01DC, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01DD, 0x01DD, -79, 0, -79},
	RuneMap{0x01DE, 0x01EF, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01F1, 0x01F1, 0, 2, 1},
	RuneMap{0x01F2, 0x01F2, -1, 1, 0},
	RuneMap{0x01F3, 0x01F3, -2, 0, -1},
	RuneMap{0x01F4, 0x01F5, upper_lower, upper_lower, upper_lower},
	RuneMap{0x01F6, 0x01F6, 0, -97, 0},
	RuneMap{0x01F7, 0x01F7, 0, -56, 0},
	RuneMap{0x01F8, 0x021F, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0220, 0x0220, 0, -130, 0},
	RuneMap{0x0222, 0x0233, upper_lower, upper_lower, upper_lower},
	RuneMap{0x023A, 0x023A, 0, 10795, 0},
	RuneMap{0x023B, 0x023C, upper_lower, upper_lower, upper_lower},
	RuneMap{0x023D, 0x023D, 0, -163, 0},
	RuneMap{0x023E, 0x023E, 0, 10792, 0},
	RuneMap{0x023F, 0x0240, 10815, 0, 10815},
	RuneMap{0x0241, 0x0242, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0243, 0x0243, 0, -195, 0},
	RuneMap{0x0244, 0x0244, 0, 69, 0},
	RuneMap{0x0245, 0x0245, 0, 71, 0},
	RuneMap{0x0246, 0x024F, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0250, 0x0250, 10783, 0, 10783},
	RuneMap{0x0251, 0x0251, 10780, 0, 10780},
	RuneMap{0x0252, 0x0252, 10782, 0, 10782},
	RuneMap{0x0253, 0x0253, -210, 0, -210},
	RuneMap{0x0254, 0x0254, -206, 0, -206},
	RuneMap{0x0256, 0x0257, -205, 0, -205},
	RuneMap{0x0259, 0x0259, -202, 0, -202},
	RuneMap{0x025B, 0x025B, -203, 0, -203},
	RuneMap{0x025C, 0x025C, 42319, 0, 42319},
	RuneMap{0x0260, 0x0260, -205, 0, -205},
	RuneMap{0x0261, 0x0261, 42315, 0, 42315},
	RuneMap{0x0263, 0x0263, -207, 0, -207},
	RuneMap{0x0265, 0x0265, 42280, 0, 42280},
	RuneMap{0x0266, 0x0266, 42308, 0, 42308},
	RuneMap{0x0268, 0x0268, -209, 0, -209},
	RuneMap{0x0269, 0x0269, -211, 0, -211},
	RuneMap{0x026A, 0x026A, 42308, 0, 42308},
	RuneMap{0x026B, 0x026B, 10743, 0, 10743},
	RuneMap{0x026C, 0x026C, 42305, 0, 42305},
	RuneMap{0x026F, 0x026F, -211, 0, -211},
	RuneMap{0x0271, 0x0271, 10749, 0, 10749},
	RuneMap{0x0272, 0x0272, -213, 0, -213},
	RuneMap{0x0275, 0x0275, -214, 0, -214},
	RuneMap{0x027D, 0x027D, 10727, 0, 10727},
	RuneMap{0x0280, 0x0280, -218, 0, -218},
	RuneMap{0x0282, 0x0282, 42307, 0, 42307},
	RuneMap{0x0283, 0x0283, -218, 0, -218},
	RuneMap{0x0287, 0x0287, 42282, 0, 42282},
	RuneMap{0x0288, 0x0288, -218, 0, -218},
	RuneMap{0x0289, 0x0289, -69, 0, -69},
	RuneMap{0x028A, 0x028B, -217, 0, -217},
	RuneMap{0x028C, 0x028C, -71, 0, -71},
	RuneMap{0x0292, 0x0292, -219, 0, -219},
	RuneMap{0x029D, 0x029D, 42261, 0, 42261},
	RuneMap{0x029E, 0x029E, 42258, 0, 42258},
	RuneMap{0x0345, 0x0345, 84, 0, 84},
	RuneMap{0x0370, 0x0373, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0376, 0x0377, upper_lower, upper_lower, upper_lower},
	RuneMap{0x037B, 0x037D, 130, 0, 130},
	RuneMap{0x037F, 0x037F, 0, 116, 0},
	RuneMap{0x0386, 0x0386, 0, 38, 0},
	RuneMap{0x0388, 0x038A, 0, 37, 0},
	RuneMap{0x038C, 0x038C, 0, 64, 0},
	RuneMap{0x038E, 0x038F, 0, 63, 0},
	RuneMap{0x0391, 0x03A1, 0, 32, 0},
	RuneMap{0x03A3, 0x03AB, 0, 32, 0},
	RuneMap{0x03AC, 0x03AC, -38, 0, -38},
	RuneMap{0x03AD, 0x03AF, -37, 0, -37},
	RuneMap{0x03B1, 0x03C1, -32, 0, -32},
	RuneMap{0x03C2, 0x03C2, -31, 0, -31},
	RuneMap{0x03C3, 0x03CB, -32, 0, -32},
	RuneMap{0x03CC, 0x03CC, -64, 0, -64},
	RuneMap{0x03CD, 0x03CE, -63, 0, -63},
	RuneMap{0x03CF, 0x03CF, 0, 8, 0},
	RuneMap{0x03D0, 0x03D0, -62, 0, -62},
	RuneMap{0x03D1, 0x03D1, -57, 0, -57},
	RuneMap{0x03D5, 0x03D5, -47, 0, -47},
	RuneMap{0x03D6, 0x03D6, -54, 0, -54},
	RuneMap{0x03D7, 0x03D7, -8, 0, -8},
	RuneMap{0x03D8, 0x03EF, upper_lower, upper_lower, upper_lower},
	RuneMap{0x03F0, 0x03F0, -86, 0, -86},
	RuneMap{0x03F1, 0x03F1, -80, 0, -80},
	RuneMap{0x03F2, 0x03F2, 7, 0, 7},
	RuneMap{0x03F3, 0x03F3, -116, 0, -116},
	RuneMap{0x03F4, 0x03F4, 0, -60, 0},
	RuneMap{0x03F5, 0x03F5, -96, 0, -96},
	RuneMap{0x03F7, 0x03F8, upper_lower, upper_lower, upper_lower},
	RuneMap{0x03F9, 0x03F9, 0, -7, 0},
	RuneMap{0x03FA, 0x03FB, upper_lower, upper_lower, upper_lower},
	RuneMap{0x03FD, 0x03FF, 0, -130, 0},
	RuneMap{0x0400, 0x040F, 0, 80, 0},
	RuneMap{0x0410, 0x042F, 0, 32, 0},
	RuneMap{0x0430, 0x044F, -32, 0, -32},
	RuneMap{0x0450, 0x045F, -80, 0, -80},
	RuneMap{0x0460, 0x0481, upper_lower, upper_lower, upper_lower},
	RuneMap{0x048A, 0x04BF, upper_lower, upper_lower, upper_lower},
	RuneMap{0x04C0, 0x04C0, 0, 15, 0},
	RuneMap{0x04C1, 0x04CE, upper_lower, upper_lower, upper_lower},
	RuneMap{0x04CF, 0x04CF, -15, 0, -15},
	RuneMap{0x04D0, 0x052F, upper_lower, upper_lower, upper_lower},
	RuneMap{0x0531, 0x0556, 0, 48, 0},
	RuneMap{0x0561, 0x0586, -48, 0, -48},
	RuneMap{0x10A0, 0x10C5, 0, 7264, 0},
	RuneMap{0x10C7, 0x10C7, 0, 7264, 0},
	RuneMap{0x10CD, 0x10CD, 0, 7264, 0},
	RuneMap{0x10D0, 0x10FA, 3008, 0, 0},
	RuneMap{0x10FD, 0x10FF, 3008, 0, 0},
	RuneMap{0x13A0, 0x13EF, 0, 38864, 0},
	RuneMap{0x13F0, 0x13F5, 0, 8, 0},
	RuneMap{0x13F8, 0x13FD, -8, 0, -8},
	RuneMap{0x1C80, 0x1C80, -6254, 0, -6254},
	RuneMap{0x1C81, 0x1C81, -6253, 0, -6253},
	RuneMap{0x1C82, 0x1C82, -6244, 0, -6244},
	RuneMap{0x1C83, 0x1C84, -6242, 0, -6242},
	RuneMap{0x1C85, 0x1C85, -6243, 0, -6243},
	RuneMap{0x1C86, 0x1C86, -6236, 0, -6236},
	RuneMap{0x1C87, 0x1C87, -6181, 0, -6181},
	RuneMap{0x1C88, 0x1C88, 35266, 0, 35266},
	RuneMap{0x1C90, 0x1CBA, 0, -3008, 0},
	RuneMap{0x1CBD, 0x1CBF, 0, -3008, 0},
	RuneMap{0x1D79, 0x1D79, 35332, 0, 35332},
	RuneMap{0x1D7D, 0x1D7D, 3814, 0, 3814},
	RuneMap{0x1D8E, 0x1D8E, 35384, 0, 35384},
	RuneMap{0x1E00, 0x1E95, upper_lower, upper_lower, upper_lower},
	RuneMap{0x1E9B, 0x1E9B, -59, 0, -59},
	RuneMap{0x1E9E, 0x1E9E, 0, -7615, 0},
	RuneMap{0x1EA0, 0x1EFF, upper_lower, upper_lower, upper_lower},
	RuneMap{0x1F00, 0x1F07, 8, 0, 8},
	RuneMap{0x1F08, 0x1F0F, 0, -8, 0},
	RuneMap{0x1F10, 0x1F15, 8, 0, 8},
	RuneMap{0x1F18, 0x1F1D, 0, -8, 0},
	RuneMap{0x1F20, 0x1F27, 8, 0, 8},
	RuneMap{0x1F28, 0x1F2F, 0, -8, 0},
	RuneMap{0x1F30, 0x1F37, 8, 0, 8},
	RuneMap{0x1F38, 0x1F3F, 0, -8, 0},
	RuneMap{0x1F40, 0x1F45, 8, 0, 8},
	RuneMap{0x1F48, 0x1F4D, 0, -8, 0},
	RuneMap{0x1F51, 0x1F51, 8, 0, 8},
	RuneMap{0x1F53, 0x1F53, 8, 0, 8},
	RuneMap{0x1F55, 0x1F55, 8, 0, 8},
	RuneMap{0x1F57, 0x1F57, 8, 0, 8},
	RuneMap{0x1F59, 0x1F59, 0, -8, 0},
	RuneMap{0x1F5B, 0x1F5B, 0, -8, 0},
	RuneMap{0x1F5D, 0x1F5D, 0, -8, 0},
	RuneMap{0x1F5F, 0x1F5F, 0, -8, 0},
	RuneMap{0x1F60, 0x1F67, 8, 0, 8},
	RuneMap{0x1F68, 0x1F6F, 0, -8, 0},
	RuneMap{0x1F70, 0x1F71, 74, 0, 74},
	RuneMap{0x1F72, 0x1F75, 86, 0, 86},
	RuneMap{0x1F76, 0x1F77, 100, 0, 100},
	RuneMap{0x1F78, 0x1F79, 128, 0, 128},
	RuneMap{0x1F7A, 0x1F7B, 112, 0, 112},
	RuneMap{0x1F7C, 0x1F7D, 126, 0, 126},
	RuneMap{0x1F80, 0x1F87, 8, 0, 8},
	RuneMap{0x1F88, 0x1F8F, 0, -8, 0},
	RuneMap{0x1F90, 0x1F97, 8, 0, 8},
	RuneMap{0x1F98, 0x1F9F, 0, -8, 0},
	RuneMap{0x1FA0, 0x1FA7, 8, 0, 8},
	RuneMap{0x1FA8, 0x1FAF, 0, -8, 0},
	RuneMap{0x1FB0, 0x1FB1, 8, 0, 8},
	RuneMap{0x1FB3, 0x1FB3, 9, 0, 9},
	RuneMap{0x1FB8, 0x1FB9, 0, -8, 0},
	RuneMap{0x1FBA, 0x1FBB, 0, -74, 0},
	RuneMap{0x1FBC, 0x1FBC, 0, -9, 0},
	RuneMap{0x1FBE, 0x1FBE, -7205, 0, -7205},
	RuneMap{0x1FC3, 0x1FC3, 9, 0, 9},
	RuneMap{0x1FC8, 0x1FCB, 0, -86, 0},
	RuneMap{0x1FCC, 0x1FCC, 0, -9, 0},
	RuneMap{0x1FD0, 0x1FD1, 8, 0, 8},
	RuneMap{0x1FD8, 0x1FD9, 0, -8, 0},
	RuneMap{0x1FDA, 0x1FDB, 0, -100, 0},
	RuneMap{0x1FE0, 0x1FE1, 8, 0, 8},
	RuneMap{0x1FE5, 0x1FE5, 7, 0, 7},
	RuneMap{0x1FE8, 0x1FE9, 0, -8, 0},
	RuneMap{0x1FEA, 0x1FEB, 0, -112, 0},
	RuneMap{0x1FEC, 0x1FEC, 0, -7, 0},
	RuneMap{0x1FF3, 0x1FF3, 9, 0, 9},
	RuneMap{0x1FF8, 0x1FF9, 0, -128, 0},
	RuneMap{0x1FFA, 0x1FFB, 0, -126, 0},
	RuneMap{0x1FFC, 0x1FFC, 0, -9, 0},
	RuneMap{0x2126, 0x2126, 0, -7517, 0},
	RuneMap{0x212A, 0x212A, 0, -8383, 0},
	RuneMap{0x212B, 0x212B, 0, -8262, 0},
	RuneMap{0x2132, 0x2132, 0, 28, 0},
	RuneMap{0x214E, 0x214E, -28, 0, -28},
	RuneMap{0x2160, 0x216F, 0, 16, 0},
	RuneMap{0x2170, 0x217F, -16, 0, -16},
	RuneMap{0x2183, 0x2184, upper_lower, upper_lower, upper_lower},
	RuneMap{0x24B6, 0x24CF, 0, 26, 0},
	RuneMap{0x24D0, 0x24E9, -26, 0, -26},
	RuneMap{0x2C00, 0x2C2F, 0, 48, 0},
	RuneMap{0x2C30, 0x2C5F, -48, 0, -48},
	RuneMap{0x2C60, 0x2C61, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2C62, 0x2C62, 0, -10743, 0},
	RuneMap{0x2C63, 0x2C63, 0, -3814, 0},
	RuneMap{0x2C64, 0x2C64, 0, -10727, 0},
	RuneMap{0x2C65, 0x2C65, -10795, 0, -10795},
	RuneMap{0x2C66, 0x2C66, -10792, 0, -10792},
	RuneMap{0x2C67, 0x2C6C, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2C6D, 0x2C6D, 0, -10780, 0},
	RuneMap{0x2C6E, 0x2C6E, 0, -10749, 0},
	RuneMap{0x2C6F, 0x2C6F, 0, -10783, 0},
	RuneMap{0x2C70, 0x2C70, 0, -10782, 0},
	RuneMap{0x2C72, 0x2C73, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2C75, 0x2C76, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2C7E, 0x2C7F, 0, -10815, 0},
	RuneMap{0x2C80, 0x2CE3, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2CEB, 0x2CEE, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2CF2, 0x2CF3, upper_lower, upper_lower, upper_lower},
	RuneMap{0x2D00, 0x2D25, -7264, 0, -7264},
	RuneMap{0x2D27, 0x2D27, -7264, 0, -7264},
	RuneMap{0x2D2D, 0x2D2D, -7264, 0, -7264},
	RuneMap{0xA640, 0xA66D, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA680, 0xA69B, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA722, 0xA72F, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA732, 0xA76F, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA779, 0xA77C, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA77D, 0xA77D, 0, -35332, 0},
	RuneMap{0xA77E, 0xA787, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA78B, 0xA78C, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA78D, 0xA78D, 0, -42280, 0},
	RuneMap{0xA790, 0xA793, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA794, 0xA794, 48, 0, 48},
	RuneMap{0xA796, 0xA7A9, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA7AA, 0xA7AA, 0, -42308, 0},
	RuneMap{0xA7AB, 0xA7AB, 0, -42319, 0},
	RuneMap{0xA7AC, 0xA7AC, 0, -42315, 0},
	RuneMap{0xA7AD, 0xA7AD, 0, -42305, 0},
	RuneMap{0xA7AE, 0xA7AE, 0, -42308, 0},
	RuneMap{0xA7B0, 0xA7B0, 0, -42258, 0},
	RuneMap{0xA7B1, 0xA7B1, 0, -42282, 0},
	RuneMap{0xA7B2, 0xA7B2, 0, -42261, 0},
	RuneMap{0xA7B3, 0xA7B3, 0, 928, 0},
	RuneMap{0xA7B4, 0xA7C3, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA7C4, 0xA7C4, 0, -48, 0},
	RuneMap{0xA7C5, 0xA7C5, 0, -42307, 0},
	RuneMap{0xA7C6, 0xA7C6, 0, -35384, 0},
	RuneMap{0xA7C7, 0xA7CA, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA7D0, 0xA7D1, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA7D6, 0xA7D9, upper_lower, upper_lower, upper_lower},
	RuneMap{0xA7F5, 0xA7F6, upper_lower, upper_lower, upper_lower},
	RuneMap{0xAB53, 0xAB53, -928, 0, -928},
	RuneMap{0xAB70, 0xABBF, -38864, 0, -38864},
	RuneMap{0xFF21, 0xFF3A, 0, 32, 0},
	RuneMap{0xFF41, 0xFF5A, -32, 0, -32},
	RuneMap{0x10400, 0x10427, 0, 40, 0},
	RuneMap{0x10428, 0x1044F, -40, 0, -40},
	RuneMap{0x104B0, 0x104D3, 0, 40, 0},
	RuneMap{0x104D8, 0x104FB, -40, 0, -40},
	RuneMap{0x10570, 0x1057A, 0, 39, 0},
	RuneMap{0x1057C, 0x1058A, 0, 39, 0},
	RuneMap{0x1058C, 0x10592, 0, 39, 0},
	RuneMap{0x10594, 0x10595, 0, 39, 0},
	RuneMap{0x10597, 0x105A1, -39, 0, -39},
	RuneMap{0x105A3, 0x105B1, -39, 0, -39},
	RuneMap{0x105B3, 0x105B9, -39, 0, -39},
	RuneMap{0x105BB, 0x105BC, -39, 0, -39},
	RuneMap{0x10C80, 0x10CB2, 0, 64, 0},
	RuneMap{0x10CC0, 0x10CF2, -64, 0, -64},
	RuneMap{0x118A0, 0x118BF, 0, 32, 0},
	RuneMap{0x118C0, 0x118DF, -32, 0, -32},
	RuneMap{0x16E40, 0x16E5F, 0, 32, 0},
	RuneMap{0x16E60, 0x16E7F, -32, 0, -32},
	RuneMap{0x1E900, 0x1E921, 0, 34, 0},
	RuneMap{0x1E922, 0x1E943, -34, 0, -34},
]

struct RuneMap {
	start        int
	end          int
	upper_offset int
	lower_offset int
	title_offset int
}

enum MapMode {
	to_upper
	to_lower
	to_title
}

// This was never working correctly, the issue is now
// fixed however the type checks in checker need to be
// updated. if you uncomment it you will see the issue
// type rune = int

// str converts a rune to string
pub fn (c rune) str() string {
	return utf32_to_str(u32(c))
	/*
	unsafe {
		fst_byte := int(c)>>8 * 3 & 0xff
		len := utf8_char_len(u8(fst_byte))
		println('len=$len')
		mut str := string{
			len: len
			str: malloc_noscan(len + 1)
		}
		for i in 0..len {
			str.str[i] = u8(int(c)>>8 * (3 - i) & 0xff)
		}
		str.str[len] = `\0`
		println(str)
		return str
	}
	*/
}

// string converts a rune array to a string
@[manualfree]
pub fn (ra []rune) string() string {
	mut sb := strings.new_builder(ra.len)
	sb.write_runes(ra)
	res := sb.str()
	unsafe { sb.free() }
	return res
}

// repeat returns a new string with `count` number of copies of the rune it was called on.
pub fn (c rune) repeat(count int) string {
	if count <= 0 {
		return ''
	} else if count == 1 {
		return c.str()
	}
	mut buffer := [5]u8{}
	res := unsafe { utf32_to_str_no_malloc(u32(c), mut &buffer[0]) }
	return res.repeat(count)
}

// bytes converts a rune to an array of bytes
@[manualfree]
pub fn (c rune) bytes() []u8 {
	mut res := []u8{cap: 5}
	mut buf := &u8(res.data)
	res.len = unsafe { utf32_decode_to_buffer(u32(c), mut buf) }
	return res
}

// length_in_bytes returns the number of bytes needed to store the code point.
// Returns -1 if the data is not a valid code point.
pub fn (c rune) length_in_bytes() int {
	code := u32(c)
	if code <= 0x7F {
		return 1
	} else if code <= 0x7FF {
		return 2
	} else if 0xD800 <= code && code <= 0xDFFF {
		// between min and max for surrogates
		return -1
	} else if code <= 0xFFFF {
		return 3
	} else if code <= 0x10FFFF {
		// 0x10FFFF is the maximum valid unicode code point
		return 4
	}
	return -1
}

// `to_upper` convert to uppercase mode.
pub fn (c rune) to_upper() rune {
	if c < 0x80 {
		if c >= `a` && c <= `z` {
			return c - 32
		}
		return c
	}
	return c.map_to(.to_upper)
}

// `to_lower` convert to lowercase mode.
pub fn (c rune) to_lower() rune {
	if c < 0x80 {
		if c >= `A` && c <= `Z` {
			return c + 32
		}
		return c
	}
	return c.map_to(.to_lower)
}

// `to_title` convert to title mode.
pub fn (c rune) to_title() rune {
	if c < 0x80 {
		if c >= `a` && c <= `z` {
			return c - 32
		}
		return c
	}
	return c.map_to(.to_title)
}

// `map_to` rune map mode: .to_upper/.to_lower/.to_title
fn (c rune) map_to(mode MapMode) rune {
	mut start := 0
	mut end := rune_maps.len
	// Binary search
	for start < end {
		middle := (start + end) / 2
		cur_map := rune_maps[middle]
		if c >= u32(cur_map.start) && c <= u32(cur_map.end) {
			offset := if mode == .to_upper {
				cur_map.upper_offset
			} else if mode == .to_lower {
				cur_map.lower_offset
			} else {
				cur_map.title_offset
			}
			if offset == upper_lower {
				is_odd := (c - cur_map.start) % 2 == 1
				if mode == .to_upper && is_odd {
					return c - 1
				} else if mode == .to_lower && !is_odd {
					return c + 1
				}
				return c
			}
			return c + offset
		}
		if c < u32(cur_map.start) {
			end = middle
		} else {
			start = middle + 1
		}
	}
	return c
}
