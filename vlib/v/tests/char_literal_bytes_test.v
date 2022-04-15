fn test_all_byte_char_literals() {
	mut a := ` `
	a = `\x00`
	assert u8(a) == u8(0)
	a = `\x01`
	assert u8(a) == u8(1)
	a = `\x02`
	assert u8(a) == u8(2)
	a = `\x03`
	assert u8(a) == u8(3)
	a = `\x04`
	assert u8(a) == u8(4)
	a = `\x05`
	assert u8(a) == u8(5)
	a = `\x06`
	assert u8(a) == u8(6)
	a = `\x07`
	assert u8(a) == u8(7)
	a = `\x08`
	assert u8(a) == u8(8)
	a = `\x09`
	assert u8(a) == u8(9)
	a = `\x0a`
	assert u8(a) == u8(10)
	a = `\x0b`
	assert u8(a) == u8(11)
	a = `\x0c`
	assert u8(a) == u8(12)
	a = `\x0d`
	assert u8(a) == u8(13)
	a = `\x0e`
	assert u8(a) == u8(14)
	a = `\x0f`
	assert u8(a) == u8(15)
	a = `\x10`
	assert u8(a) == u8(16)
	a = `\x11`
	assert u8(a) == u8(17)
	a = `\x12`
	assert u8(a) == u8(18)
	a = `\x13`
	assert u8(a) == u8(19)
	a = `\x14`
	assert u8(a) == u8(20)
	a = `\x15`
	assert u8(a) == u8(21)
	a = `\x16`
	assert u8(a) == u8(22)
	a = `\x17`
	assert u8(a) == u8(23)
	a = `\x18`
	assert u8(a) == u8(24)
	a = `\x19`
	assert u8(a) == u8(25)
	a = `\x1a`
	assert u8(a) == u8(26)
	a = `\x1b`
	assert u8(a) == u8(27)
	a = `\x1c`
	assert u8(a) == u8(28)
	a = `\x1d`
	assert u8(a) == u8(29)
	a = `\x1e`
	assert u8(a) == u8(30)
	a = `\x1f`
	assert u8(a) == u8(31)
	a = `\x20`
	assert u8(a) == u8(32)
	a = `\x21`
	assert u8(a) == u8(33)
	a = `\x22`
	assert u8(a) == u8(34)
	a = `\x23`
	assert u8(a) == u8(35)
	a = `\x24`
	assert u8(a) == u8(36)
	a = `\x25`
	assert u8(a) == u8(37)
	a = `\x26`
	assert u8(a) == u8(38)
	a = `\x27`
	assert u8(a) == u8(39)
	a = `\x28`
	assert u8(a) == u8(40)
	a = `\x29`
	assert u8(a) == u8(41)
	a = `\x2a`
	assert u8(a) == u8(42)
	a = `\x2b`
	assert u8(a) == u8(43)
	a = `\x2c`
	assert u8(a) == u8(44)
	a = `\x2d`
	assert u8(a) == u8(45)
	a = `\x2e`
	assert u8(a) == u8(46)
	a = `\x2f`
	assert u8(a) == u8(47)
	a = `\x30`
	assert u8(a) == u8(48)
	a = `\x31`
	assert u8(a) == u8(49)
	a = `\x32`
	assert u8(a) == u8(50)
	a = `\x33`
	assert u8(a) == u8(51)
	a = `\x34`
	assert u8(a) == u8(52)
	a = `\x35`
	assert u8(a) == u8(53)
	a = `\x36`
	assert u8(a) == u8(54)
	a = `\x37`
	assert u8(a) == u8(55)
	a = `\x38`
	assert u8(a) == u8(56)
	a = `\x39`
	assert u8(a) == u8(57)
	a = `\x3a`
	assert u8(a) == u8(58)
	a = `\x3b`
	assert u8(a) == u8(59)
	a = `\x3c`
	assert u8(a) == u8(60)
	a = `\x3d`
	assert u8(a) == u8(61)
	a = `\x3e`
	assert u8(a) == u8(62)
	a = `\x3f`
	assert u8(a) == u8(63)
	a = `\x40`
	assert u8(a) == u8(64)
	a = `\x41`
	assert u8(a) == u8(65)
	a = `\x42`
	assert u8(a) == u8(66)
	a = `\x43`
	assert u8(a) == u8(67)
	a = `\x44`
	assert u8(a) == u8(68)
	a = `\x45`
	assert u8(a) == u8(69)
	a = `\x46`
	assert u8(a) == u8(70)
	a = `\x47`
	assert u8(a) == u8(71)
	a = `\x48`
	assert u8(a) == u8(72)
	a = `\x49`
	assert u8(a) == u8(73)
	a = `\x4a`
	assert u8(a) == u8(74)
	a = `\x4b`
	assert u8(a) == u8(75)
	a = `\x4c`
	assert u8(a) == u8(76)
	a = `\x4d`
	assert u8(a) == u8(77)
	a = `\x4e`
	assert u8(a) == u8(78)
	a = `\x4f`
	assert u8(a) == u8(79)
	a = `\x50`
	assert u8(a) == u8(80)
	a = `\x51`
	assert u8(a) == u8(81)
	a = `\x52`
	assert u8(a) == u8(82)
	a = `\x53`
	assert u8(a) == u8(83)
	a = `\x54`
	assert u8(a) == u8(84)
	a = `\x55`
	assert u8(a) == u8(85)
	a = `\x56`
	assert u8(a) == u8(86)
	a = `\x57`
	assert u8(a) == u8(87)
	a = `\x58`
	assert u8(a) == u8(88)
	a = `\x59`
	assert u8(a) == u8(89)
	a = `\x5a`
	assert u8(a) == u8(90)
	a = `\x5b`
	assert u8(a) == u8(91)
	a = `\x5c`
	assert u8(a) == u8(92)
	a = `\x5d`
	assert u8(a) == u8(93)
	a = `\x5e`
	assert u8(a) == u8(94)
	a = `\x5f`
	assert u8(a) == u8(95)
	a = `\x60`
	assert u8(a) == u8(96)
	a = `\x61`
	assert u8(a) == u8(97)
	a = `\x62`
	assert u8(a) == u8(98)
	a = `\x63`
	assert u8(a) == u8(99)
	a = `\x64`
	assert u8(a) == u8(100)
	a = `\x65`
	assert u8(a) == u8(101)
	a = `\x66`
	assert u8(a) == u8(102)
	a = `\x67`
	assert u8(a) == u8(103)
	a = `\x68`
	assert u8(a) == u8(104)
	a = `\x69`
	assert u8(a) == u8(105)
	a = `\x6a`
	assert u8(a) == u8(106)
	a = `\x6b`
	assert u8(a) == u8(107)
	a = `\x6c`
	assert u8(a) == u8(108)
	a = `\x6d`
	assert u8(a) == u8(109)
	a = `\x6e`
	assert u8(a) == u8(110)
	a = `\x6f`
	assert u8(a) == u8(111)
	a = `\x70`
	assert u8(a) == u8(112)
	a = `\x71`
	assert u8(a) == u8(113)
	a = `\x72`
	assert u8(a) == u8(114)
	a = `\x73`
	assert u8(a) == u8(115)
	a = `\x74`
	assert u8(a) == u8(116)
	a = `\x75`
	assert u8(a) == u8(117)
	a = `\x76`
	assert u8(a) == u8(118)
	a = `\x77`
	assert u8(a) == u8(119)
	a = `\x78`
	assert u8(a) == u8(120)
	a = `\x79`
	assert u8(a) == u8(121)
	a = `\x7a`
	assert u8(a) == u8(122)
	a = `\x7b`
	assert u8(a) == u8(123)
	a = `\x7c`
	assert u8(a) == u8(124)
	a = `\x7d`
	assert u8(a) == u8(125)
	a = `\x7e`
	assert u8(a) == u8(126)
	a = `\x7f`
	assert u8(a) == u8(127)
	a = `\x80`
	assert u8(a) == u8(128)
	a = `\x81`
	assert u8(a) == u8(129)
	a = `\x82`
	assert u8(a) == u8(130)
	a = `\x83`
	assert u8(a) == u8(131)
	a = `\x84`
	assert u8(a) == u8(132)
	a = `\x85`
	assert u8(a) == u8(133)
	a = `\x86`
	assert u8(a) == u8(134)
	a = `\x87`
	assert u8(a) == u8(135)
	a = `\x88`
	assert u8(a) == u8(136)
	a = `\x89`
	assert u8(a) == u8(137)
	a = `\x8a`
	assert u8(a) == u8(138)
	a = `\x8b`
	assert u8(a) == u8(139)
	a = `\x8c`
	assert u8(a) == u8(140)
	a = `\x8d`
	assert u8(a) == u8(141)
	a = `\x8e`
	assert u8(a) == u8(142)
	a = `\x8f`
	assert u8(a) == u8(143)
	a = `\x90`
	assert u8(a) == u8(144)
	a = `\x91`
	assert u8(a) == u8(145)
	a = `\x92`
	assert u8(a) == u8(146)
	a = `\x93`
	assert u8(a) == u8(147)
	a = `\x94`
	assert u8(a) == u8(148)
	a = `\x95`
	assert u8(a) == u8(149)
	a = `\x96`
	assert u8(a) == u8(150)
	a = `\x97`
	assert u8(a) == u8(151)
	a = `\x98`
	assert u8(a) == u8(152)
	a = `\x99`
	assert u8(a) == u8(153)
	a = `\x9a`
	assert u8(a) == u8(154)
	a = `\x9b`
	assert u8(a) == u8(155)
	a = `\x9c`
	assert u8(a) == u8(156)
	a = `\x9d`
	assert u8(a) == u8(157)
	a = `\x9e`
	assert u8(a) == u8(158)
	a = `\x9f`
	assert u8(a) == u8(159)
	a = `\xa0`
	assert u8(a) == u8(160)
	a = `\xa1`
	assert u8(a) == u8(161)
	a = `\xa2`
	assert u8(a) == u8(162)
	a = `\xa3`
	assert u8(a) == u8(163)
	a = `\xa4`
	assert u8(a) == u8(164)
	a = `\xa5`
	assert u8(a) == u8(165)
	a = `\xa6`
	assert u8(a) == u8(166)
	a = `\xa7`
	assert u8(a) == u8(167)
	a = `\xa8`
	assert u8(a) == u8(168)
	a = `\xa9`
	assert u8(a) == u8(169)
	a = `\xaa`
	assert u8(a) == u8(170)
	a = `\xab`
	assert u8(a) == u8(171)
	a = `\xac`
	assert u8(a) == u8(172)
	a = `\xad`
	assert u8(a) == u8(173)
	a = `\xae`
	assert u8(a) == u8(174)
	a = `\xaf`
	assert u8(a) == u8(175)
	a = `\xb0`
	assert u8(a) == u8(176)
	a = `\xb1`
	assert u8(a) == u8(177)
	a = `\xb2`
	assert u8(a) == u8(178)
	a = `\xb3`
	assert u8(a) == u8(179)
	a = `\xb4`
	assert u8(a) == u8(180)
	a = `\xb5`
	assert u8(a) == u8(181)
	a = `\xb6`
	assert u8(a) == u8(182)
	a = `\xb7`
	assert u8(a) == u8(183)
	a = `\xb8`
	assert u8(a) == u8(184)
	a = `\xb9`
	assert u8(a) == u8(185)
	a = `\xba`
	assert u8(a) == u8(186)
	a = `\xbb`
	assert u8(a) == u8(187)
	a = `\xbc`
	assert u8(a) == u8(188)
	a = `\xbd`
	assert u8(a) == u8(189)
	a = `\xbe`
	assert u8(a) == u8(190)
	a = `\xbf`
	assert u8(a) == u8(191)
	a = `\xc0`
	assert u8(a) == u8(192)
	a = `\xc1`
	assert u8(a) == u8(193)
	a = `\xc2`
	assert u8(a) == u8(194)
	a = `\xc3`
	assert u8(a) == u8(195)
	a = `\xc4`
	assert u8(a) == u8(196)
	a = `\xc5`
	assert u8(a) == u8(197)
	a = `\xc6`
	assert u8(a) == u8(198)
	a = `\xc7`
	assert u8(a) == u8(199)
	a = `\xc8`
	assert u8(a) == u8(200)
	a = `\xc9`
	assert u8(a) == u8(201)
	a = `\xca`
	assert u8(a) == u8(202)
	a = `\xcb`
	assert u8(a) == u8(203)
	a = `\xcc`
	assert u8(a) == u8(204)
	a = `\xcd`
	assert u8(a) == u8(205)
	a = `\xce`
	assert u8(a) == u8(206)
	a = `\xcf`
	assert u8(a) == u8(207)
	a = `\xd0`
	assert u8(a) == u8(208)
	a = `\xd1`
	assert u8(a) == u8(209)
	a = `\xd2`
	assert u8(a) == u8(210)
	a = `\xd3`
	assert u8(a) == u8(211)
	a = `\xd4`
	assert u8(a) == u8(212)
	a = `\xd5`
	assert u8(a) == u8(213)
	a = `\xd6`
	assert u8(a) == u8(214)
	a = `\xd7`
	assert u8(a) == u8(215)
	a = `\xd8`
	assert u8(a) == u8(216)
	a = `\xd9`
	assert u8(a) == u8(217)
	a = `\xda`
	assert u8(a) == u8(218)
	a = `\xdb`
	assert u8(a) == u8(219)
	a = `\xdc`
	assert u8(a) == u8(220)
	a = `\xdd`
	assert u8(a) == u8(221)
	a = `\xde`
	assert u8(a) == u8(222)
	a = `\xdf`
	assert u8(a) == u8(223)
	a = `\xe0`
	assert u8(a) == u8(224)
	a = `\xe1`
	assert u8(a) == u8(225)
	a = `\xe2`
	assert u8(a) == u8(226)
	a = `\xe3`
	assert u8(a) == u8(227)
	a = `\xe4`
	assert u8(a) == u8(228)
	a = `\xe5`
	assert u8(a) == u8(229)
	a = `\xe6`
	assert u8(a) == u8(230)
	a = `\xe7`
	assert u8(a) == u8(231)
	a = `\xe8`
	assert u8(a) == u8(232)
	a = `\xe9`
	assert u8(a) == u8(233)
	a = `\xea`
	assert u8(a) == u8(234)
	a = `\xeb`
	assert u8(a) == u8(235)
	a = `\xec`
	assert u8(a) == u8(236)
	a = `\xed`
	assert u8(a) == u8(237)
	a = `\xee`
	assert u8(a) == u8(238)
	a = `\xef`
	assert u8(a) == u8(239)
	a = `\xf0`
	assert u8(a) == u8(240)
	a = `\xf1`
	assert u8(a) == u8(241)
	a = `\xf2`
	assert u8(a) == u8(242)
	a = `\xf3`
	assert u8(a) == u8(243)
	a = `\xf4`
	assert u8(a) == u8(244)
	a = `\xf5`
	assert u8(a) == u8(245)
	a = `\xf6`
	assert u8(a) == u8(246)
	a = `\xf7`
	assert u8(a) == u8(247)
	a = `\xf8`
	assert u8(a) == u8(248)
	a = `\xf9`
	assert u8(a) == u8(249)
	a = `\xfa`
	assert u8(a) == u8(250)
	a = `\xfb`
	assert u8(a) == u8(251)
	a = `\xfc`
	assert u8(a) == u8(252)
	a = `\xfd`
	assert u8(a) == u8(253)
	a = `\xfe`
	assert u8(a) == u8(254)
	a = `\xff`
	assert u8(a) == u8(255)
}
