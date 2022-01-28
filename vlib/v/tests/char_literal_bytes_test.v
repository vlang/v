fn test_all_byte_char_literals() {
	mut a := ` `
	a = `\x00`
	assert byte(a) == byte(0)
	a = `\x01`
	assert byte(a) == byte(1)
	a = `\x02`
	assert byte(a) == byte(2)
	a = `\x03`
	assert byte(a) == byte(3)
	a = `\x04`
	assert byte(a) == byte(4)
	a = `\x05`
	assert byte(a) == byte(5)
	a = `\x06`
	assert byte(a) == byte(6)
	a = `\x07`
	assert byte(a) == byte(7)
	a = `\x08`
	assert byte(a) == byte(8)
	a = `\x09`
	assert byte(a) == byte(9)
	a = `\x0a`
	assert byte(a) == byte(10)
	a = `\x0b`
	assert byte(a) == byte(11)
	a = `\x0c`
	assert byte(a) == byte(12)
	a = `\x0d`
	assert byte(a) == byte(13)
	a = `\x0e`
	assert byte(a) == byte(14)
	a = `\x0f`
	assert byte(a) == byte(15)
	a = `\x10`
	assert byte(a) == byte(16)
	a = `\x11`
	assert byte(a) == byte(17)
	a = `\x12`
	assert byte(a) == byte(18)
	a = `\x13`
	assert byte(a) == byte(19)
	a = `\x14`
	assert byte(a) == byte(20)
	a = `\x15`
	assert byte(a) == byte(21)
	a = `\x16`
	assert byte(a) == byte(22)
	a = `\x17`
	assert byte(a) == byte(23)
	a = `\x18`
	assert byte(a) == byte(24)
	a = `\x19`
	assert byte(a) == byte(25)
	a = `\x1a`
	assert byte(a) == byte(26)
	a = `\x1b`
	assert byte(a) == byte(27)
	a = `\x1c`
	assert byte(a) == byte(28)
	a = `\x1d`
	assert byte(a) == byte(29)
	a = `\x1e`
	assert byte(a) == byte(30)
	a = `\x1f`
	assert byte(a) == byte(31)
	a = `\x20`
	assert byte(a) == byte(32)
	a = `\x21`
	assert byte(a) == byte(33)
	a = `\x22`
	assert byte(a) == byte(34)
	a = `\x23`
	assert byte(a) == byte(35)
	a = `\x24`
	assert byte(a) == byte(36)
	a = `\x25`
	assert byte(a) == byte(37)
	a = `\x26`
	assert byte(a) == byte(38)
	a = `\x27`
	assert byte(a) == byte(39)
	a = `\x28`
	assert byte(a) == byte(40)
	a = `\x29`
	assert byte(a) == byte(41)
	a = `\x2a`
	assert byte(a) == byte(42)
	a = `\x2b`
	assert byte(a) == byte(43)
	a = `\x2c`
	assert byte(a) == byte(44)
	a = `\x2d`
	assert byte(a) == byte(45)
	a = `\x2e`
	assert byte(a) == byte(46)
	a = `\x2f`
	assert byte(a) == byte(47)
	a = `\x30`
	assert byte(a) == byte(48)
	a = `\x31`
	assert byte(a) == byte(49)
	a = `\x32`
	assert byte(a) == byte(50)
	a = `\x33`
	assert byte(a) == byte(51)
	a = `\x34`
	assert byte(a) == byte(52)
	a = `\x35`
	assert byte(a) == byte(53)
	a = `\x36`
	assert byte(a) == byte(54)
	a = `\x37`
	assert byte(a) == byte(55)
	a = `\x38`
	assert byte(a) == byte(56)
	a = `\x39`
	assert byte(a) == byte(57)
	a = `\x3a`
	assert byte(a) == byte(58)
	a = `\x3b`
	assert byte(a) == byte(59)
	a = `\x3c`
	assert byte(a) == byte(60)
	a = `\x3d`
	assert byte(a) == byte(61)
	a = `\x3e`
	assert byte(a) == byte(62)
	a = `\x3f`
	assert byte(a) == byte(63)
	a = `\x40`
	assert byte(a) == byte(64)
	a = `\x41`
	assert byte(a) == byte(65)
	a = `\x42`
	assert byte(a) == byte(66)
	a = `\x43`
	assert byte(a) == byte(67)
	a = `\x44`
	assert byte(a) == byte(68)
	a = `\x45`
	assert byte(a) == byte(69)
	a = `\x46`
	assert byte(a) == byte(70)
	a = `\x47`
	assert byte(a) == byte(71)
	a = `\x48`
	assert byte(a) == byte(72)
	a = `\x49`
	assert byte(a) == byte(73)
	a = `\x4a`
	assert byte(a) == byte(74)
	a = `\x4b`
	assert byte(a) == byte(75)
	a = `\x4c`
	assert byte(a) == byte(76)
	a = `\x4d`
	assert byte(a) == byte(77)
	a = `\x4e`
	assert byte(a) == byte(78)
	a = `\x4f`
	assert byte(a) == byte(79)
	a = `\x50`
	assert byte(a) == byte(80)
	a = `\x51`
	assert byte(a) == byte(81)
	a = `\x52`
	assert byte(a) == byte(82)
	a = `\x53`
	assert byte(a) == byte(83)
	a = `\x54`
	assert byte(a) == byte(84)
	a = `\x55`
	assert byte(a) == byte(85)
	a = `\x56`
	assert byte(a) == byte(86)
	a = `\x57`
	assert byte(a) == byte(87)
	a = `\x58`
	assert byte(a) == byte(88)
	a = `\x59`
	assert byte(a) == byte(89)
	a = `\x5a`
	assert byte(a) == byte(90)
	a = `\x5b`
	assert byte(a) == byte(91)
	a = `\x5c`
	assert byte(a) == byte(92)
	a = `\x5d`
	assert byte(a) == byte(93)
	a = `\x5e`
	assert byte(a) == byte(94)
	a = `\x5f`
	assert byte(a) == byte(95)
	a = `\x60`
	assert byte(a) == byte(96)
	a = `\x61`
	assert byte(a) == byte(97)
	a = `\x62`
	assert byte(a) == byte(98)
	a = `\x63`
	assert byte(a) == byte(99)
	a = `\x64`
	assert byte(a) == byte(100)
	a = `\x65`
	assert byte(a) == byte(101)
	a = `\x66`
	assert byte(a) == byte(102)
	a = `\x67`
	assert byte(a) == byte(103)
	a = `\x68`
	assert byte(a) == byte(104)
	a = `\x69`
	assert byte(a) == byte(105)
	a = `\x6a`
	assert byte(a) == byte(106)
	a = `\x6b`
	assert byte(a) == byte(107)
	a = `\x6c`
	assert byte(a) == byte(108)
	a = `\x6d`
	assert byte(a) == byte(109)
	a = `\x6e`
	assert byte(a) == byte(110)
	a = `\x6f`
	assert byte(a) == byte(111)
	a = `\x70`
	assert byte(a) == byte(112)
	a = `\x71`
	assert byte(a) == byte(113)
	a = `\x72`
	assert byte(a) == byte(114)
	a = `\x73`
	assert byte(a) == byte(115)
	a = `\x74`
	assert byte(a) == byte(116)
	a = `\x75`
	assert byte(a) == byte(117)
	a = `\x76`
	assert byte(a) == byte(118)
	a = `\x77`
	assert byte(a) == byte(119)
	a = `\x78`
	assert byte(a) == byte(120)
	a = `\x79`
	assert byte(a) == byte(121)
	a = `\x7a`
	assert byte(a) == byte(122)
	a = `\x7b`
	assert byte(a) == byte(123)
	a = `\x7c`
	assert byte(a) == byte(124)
	a = `\x7d`
	assert byte(a) == byte(125)
	a = `\x7e`
	assert byte(a) == byte(126)
	a = `\x7f`
	assert byte(a) == byte(127)
	a = `\x80`
	assert byte(a) == byte(128)
	a = `\x81`
	assert byte(a) == byte(129)
	a = `\x82`
	assert byte(a) == byte(130)
	a = `\x83`
	assert byte(a) == byte(131)
	a = `\x84`
	assert byte(a) == byte(132)
	a = `\x85`
	assert byte(a) == byte(133)
	a = `\x86`
	assert byte(a) == byte(134)
	a = `\x87`
	assert byte(a) == byte(135)
	a = `\x88`
	assert byte(a) == byte(136)
	a = `\x89`
	assert byte(a) == byte(137)
	a = `\x8a`
	assert byte(a) == byte(138)
	a = `\x8b`
	assert byte(a) == byte(139)
	a = `\x8c`
	assert byte(a) == byte(140)
	a = `\x8d`
	assert byte(a) == byte(141)
	a = `\x8e`
	assert byte(a) == byte(142)
	a = `\x8f`
	assert byte(a) == byte(143)
	a = `\x90`
	assert byte(a) == byte(144)
	a = `\x91`
	assert byte(a) == byte(145)
	a = `\x92`
	assert byte(a) == byte(146)
	a = `\x93`
	assert byte(a) == byte(147)
	a = `\x94`
	assert byte(a) == byte(148)
	a = `\x95`
	assert byte(a) == byte(149)
	a = `\x96`
	assert byte(a) == byte(150)
	a = `\x97`
	assert byte(a) == byte(151)
	a = `\x98`
	assert byte(a) == byte(152)
	a = `\x99`
	assert byte(a) == byte(153)
	a = `\x9a`
	assert byte(a) == byte(154)
	a = `\x9b`
	assert byte(a) == byte(155)
	a = `\x9c`
	assert byte(a) == byte(156)
	a = `\x9d`
	assert byte(a) == byte(157)
	a = `\x9e`
	assert byte(a) == byte(158)
	a = `\x9f`
	assert byte(a) == byte(159)
	a = `\xa0`
	assert byte(a) == byte(160)
	a = `\xa1`
	assert byte(a) == byte(161)
	a = `\xa2`
	assert byte(a) == byte(162)
	a = `\xa3`
	assert byte(a) == byte(163)
	a = `\xa4`
	assert byte(a) == byte(164)
	a = `\xa5`
	assert byte(a) == byte(165)
	a = `\xa6`
	assert byte(a) == byte(166)
	a = `\xa7`
	assert byte(a) == byte(167)
	a = `\xa8`
	assert byte(a) == byte(168)
	a = `\xa9`
	assert byte(a) == byte(169)
	a = `\xaa`
	assert byte(a) == byte(170)
	a = `\xab`
	assert byte(a) == byte(171)
	a = `\xac`
	assert byte(a) == byte(172)
	a = `\xad`
	assert byte(a) == byte(173)
	a = `\xae`
	assert byte(a) == byte(174)
	a = `\xaf`
	assert byte(a) == byte(175)
	a = `\xb0`
	assert byte(a) == byte(176)
	a = `\xb1`
	assert byte(a) == byte(177)
	a = `\xb2`
	assert byte(a) == byte(178)
	a = `\xb3`
	assert byte(a) == byte(179)
	a = `\xb4`
	assert byte(a) == byte(180)
	a = `\xb5`
	assert byte(a) == byte(181)
	a = `\xb6`
	assert byte(a) == byte(182)
	a = `\xb7`
	assert byte(a) == byte(183)
	a = `\xb8`
	assert byte(a) == byte(184)
	a = `\xb9`
	assert byte(a) == byte(185)
	a = `\xba`
	assert byte(a) == byte(186)
	a = `\xbb`
	assert byte(a) == byte(187)
	a = `\xbc`
	assert byte(a) == byte(188)
	a = `\xbd`
	assert byte(a) == byte(189)
	a = `\xbe`
	assert byte(a) == byte(190)
	a = `\xbf`
	assert byte(a) == byte(191)
	a = `\xc0`
	assert byte(a) == byte(192)
	a = `\xc1`
	assert byte(a) == byte(193)
	a = `\xc2`
	assert byte(a) == byte(194)
	a = `\xc3`
	assert byte(a) == byte(195)
	a = `\xc4`
	assert byte(a) == byte(196)
	a = `\xc5`
	assert byte(a) == byte(197)
	a = `\xc6`
	assert byte(a) == byte(198)
	a = `\xc7`
	assert byte(a) == byte(199)
	a = `\xc8`
	assert byte(a) == byte(200)
	a = `\xc9`
	assert byte(a) == byte(201)
	a = `\xca`
	assert byte(a) == byte(202)
	a = `\xcb`
	assert byte(a) == byte(203)
	a = `\xcc`
	assert byte(a) == byte(204)
	a = `\xcd`
	assert byte(a) == byte(205)
	a = `\xce`
	assert byte(a) == byte(206)
	a = `\xcf`
	assert byte(a) == byte(207)
	a = `\xd0`
	assert byte(a) == byte(208)
	a = `\xd1`
	assert byte(a) == byte(209)
	a = `\xd2`
	assert byte(a) == byte(210)
	a = `\xd3`
	assert byte(a) == byte(211)
	a = `\xd4`
	assert byte(a) == byte(212)
	a = `\xd5`
	assert byte(a) == byte(213)
	a = `\xd6`
	assert byte(a) == byte(214)
	a = `\xd7`
	assert byte(a) == byte(215)
	a = `\xd8`
	assert byte(a) == byte(216)
	a = `\xd9`
	assert byte(a) == byte(217)
	a = `\xda`
	assert byte(a) == byte(218)
	a = `\xdb`
	assert byte(a) == byte(219)
	a = `\xdc`
	assert byte(a) == byte(220)
	a = `\xdd`
	assert byte(a) == byte(221)
	a = `\xde`
	assert byte(a) == byte(222)
	a = `\xdf`
	assert byte(a) == byte(223)
	a = `\xe0`
	assert byte(a) == byte(224)
	a = `\xe1`
	assert byte(a) == byte(225)
	a = `\xe2`
	assert byte(a) == byte(226)
	a = `\xe3`
	assert byte(a) == byte(227)
	a = `\xe4`
	assert byte(a) == byte(228)
	a = `\xe5`
	assert byte(a) == byte(229)
	a = `\xe6`
	assert byte(a) == byte(230)
	a = `\xe7`
	assert byte(a) == byte(231)
	a = `\xe8`
	assert byte(a) == byte(232)
	a = `\xe9`
	assert byte(a) == byte(233)
	a = `\xea`
	assert byte(a) == byte(234)
	a = `\xeb`
	assert byte(a) == byte(235)
	a = `\xec`
	assert byte(a) == byte(236)
	a = `\xed`
	assert byte(a) == byte(237)
	a = `\xee`
	assert byte(a) == byte(238)
	a = `\xef`
	assert byte(a) == byte(239)
	a = `\xf0`
	assert byte(a) == byte(240)
	a = `\xf1`
	assert byte(a) == byte(241)
	a = `\xf2`
	assert byte(a) == byte(242)
	a = `\xf3`
	assert byte(a) == byte(243)
	a = `\xf4`
	assert byte(a) == byte(244)
	a = `\xf5`
	assert byte(a) == byte(245)
	a = `\xf6`
	assert byte(a) == byte(246)
	a = `\xf7`
	assert byte(a) == byte(247)
	a = `\xf8`
	assert byte(a) == byte(248)
	a = `\xf9`
	assert byte(a) == byte(249)
	a = `\xfa`
	assert byte(a) == byte(250)
	a = `\xfb`
	assert byte(a) == byte(251)
	a = `\xfc`
	assert byte(a) == byte(252)
	a = `\xfd`
	assert byte(a) == byte(253)
	a = `\xfe`
	assert byte(a) == byte(254)
	a = `\xff`
	assert byte(a) == byte(255)
}
