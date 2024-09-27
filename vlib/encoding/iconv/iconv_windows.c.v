module iconv

// Module iconv provides functions conversion between vstring(UTF8) to/from difference encodings.
// iconv implementation using Win32 API to convert
// Idear from https://github.com/win-iconv/win-iconv

fn C.GetACP() int
fn C.WideCharToMultiByte(codepage u32, dwflags u32, src &u8, src_len int, dst &u8, dst_len int, default_char &u8, used_default_char &bool) int
fn C.MultiByteToWideChar(codepage u32, dwflags u32, src &u8, src_len int, dst &u8, dst_len int) int

struct Codepage_Alias {
	codepage int
	name     string
}

const codepage_alias = [
	// vfmt off
	Codepage_Alias{65001, 'CP65001'},
    Codepage_Alias{65001, 'UTF8'},
    Codepage_Alias{65001, 'UTF-8'},

    Codepage_Alias{1200, 'CP1200'},
    Codepage_Alias{1200, 'UTF16LE'},
    Codepage_Alias{1200, 'UTF-16LE'},
    Codepage_Alias{1200, 'UCS2LE'},
    Codepage_Alias{1200, 'UCS-2LE'},
    Codepage_Alias{1200, 'UCS-2-INTERNAL'},

    Codepage_Alias{1201, 'CP1201'},
    Codepage_Alias{1201, 'UTF16BE'},
    Codepage_Alias{1201, 'UTF-16BE'},
    Codepage_Alias{1201, 'UCS2BE'},
    Codepage_Alias{1201, 'UCS-2BE'},
    Codepage_Alias{1201, 'unicodeFFFE'},

    Codepage_Alias{12000, 'CP12000'},
    Codepage_Alias{12000, 'UTF32LE'},
    Codepage_Alias{12000, 'UTF-32LE'},
    Codepage_Alias{12000, 'UCS4LE'},
    Codepage_Alias{12000, 'UCS-4LE'},

    Codepage_Alias{12001, 'CP12001'},
    Codepage_Alias{12001, 'UTF32BE'},
    Codepage_Alias{12001, 'UTF-32BE'},
    Codepage_Alias{12001, 'UCS4BE'},
    Codepage_Alias{12001, 'UCS-4BE'},

//#ifndef GLIB_COMPILATION
//    //
//     * Default is big endian.
//     * See rfc2781 4.3 Interpreting text labelled as UTF-16.
//     
//    Codepage_Alias{1201, 'UTF16'},
//    Codepage_Alias{1201, 'UTF-16'},
//    Codepage_Alias{1201, 'UCS2'},
//    Codepage_Alias{1201, 'UCS-2'},
//    Codepage_Alias{12001, 'UTF32'},
//    Codepage_Alias{12001, 'UTF-32'},
//    Codepage_Alias{12001, 'UCS-4'},
//    Codepage_Alias{12001, 'UCS4'},
//#else
    // Default is little endian, because the platform is
    Codepage_Alias{1200, 'UTF16'},
    Codepage_Alias{1200, 'UTF-16'},
    Codepage_Alias{1200, 'UCS2'},
    Codepage_Alias{1200, 'UCS-2'},
    Codepage_Alias{12000, 'UTF32'},
    Codepage_Alias{12000, 'UTF-32'},
    Codepage_Alias{12000, 'UCS4'},
    Codepage_Alias{12000, 'UCS-4'},
//#endif

    // copy from libiconv `iconv -l`
    // !IsValidCodePage(367)
    Codepage_Alias{20127, 'ANSI_X3.4-1968'},
    Codepage_Alias{20127, 'ANSI_X3.4-1986'},
    Codepage_Alias{20127, 'ASCII'},
    Codepage_Alias{20127, 'CP367'},
    Codepage_Alias{20127, 'IBM367'},
    Codepage_Alias{20127, 'ISO-IR-6'},
    Codepage_Alias{20127, 'ISO646-US'},
    Codepage_Alias{20127, 'ISO_646.IRV:1991'},
    Codepage_Alias{20127, 'US'},
    Codepage_Alias{20127, 'US-ASCII'},
    Codepage_Alias{20127, 'CSASCII'},

    // !IsValidCodePage(819)
    Codepage_Alias{1252, 'CP819'},
    Codepage_Alias{1252, 'IBM819'},
    Codepage_Alias{28591, 'ISO-8859-1'},
    Codepage_Alias{28591, 'ISO-IR-100'},
    Codepage_Alias{28591, 'ISO8859-1'},
    Codepage_Alias{28591, 'ISO_8859-1'},
    Codepage_Alias{28591, 'ISO_8859-1:1987'},
    Codepage_Alias{28591, 'L1'},
    Codepage_Alias{28591, 'LATIN1'},
    Codepage_Alias{28591, 'CSISOLATIN1'},

    Codepage_Alias{1250, 'CP1250'},
    Codepage_Alias{1250, 'MS-EE'},
    Codepage_Alias{1250, 'WINDOWS-1250'},

    Codepage_Alias{1251, 'CP1251'},
    Codepage_Alias{1251, 'MS-CYRL'},
    Codepage_Alias{1251, 'WINDOWS-1251'},

    Codepage_Alias{1252, 'CP1252'},
    Codepage_Alias{1252, 'MS-ANSI'},
    Codepage_Alias{1252, 'WINDOWS-1252'},

    Codepage_Alias{1253, 'CP1253'},
    Codepage_Alias{1253, 'MS-GREEK'},
    Codepage_Alias{1253, 'WINDOWS-1253'},

    Codepage_Alias{1254, 'CP1254'},
    Codepage_Alias{1254, 'MS-TURK'},
    Codepage_Alias{1254, 'WINDOWS-1254'},

    Codepage_Alias{1255, 'CP1255'},
    Codepage_Alias{1255, 'MS-HEBR'},
    Codepage_Alias{1255, 'WINDOWS-1255'},

    Codepage_Alias{1256, 'CP1256'},
    Codepage_Alias{1256, 'MS-ARAB'},
    Codepage_Alias{1256, 'WINDOWS-1256'},

    Codepage_Alias{1257, 'CP1257'},
    Codepage_Alias{1257, 'WINBALTRIM'},
    Codepage_Alias{1257, 'WINDOWS-1257'},

    Codepage_Alias{1258, 'CP1258'},
    Codepage_Alias{1258, 'WINDOWS-1258'},

    Codepage_Alias{850, '850'},
    Codepage_Alias{850, 'CP850'},
    Codepage_Alias{850, 'IBM850'},
    Codepage_Alias{850, 'CSPC850MULTILINGUAL'},

    // !IsValidCodePage(862)
    Codepage_Alias{862, '862'},
    Codepage_Alias{862, 'CP862'},
    Codepage_Alias{862, 'IBM862'},
    Codepage_Alias{862, 'CSPC862LATINHEBREW'},

    Codepage_Alias{866, '866'},
    Codepage_Alias{866, 'CP866'},
    Codepage_Alias{866, 'IBM866'},
    Codepage_Alias{866, 'CSIBM866'},

    // !IsValidCodePage(154) 
    Codepage_Alias{154, 'CP154'},
    Codepage_Alias{154, 'CYRILLIC-ASIAN'},
    Codepage_Alias{154, 'PT154'},
    Codepage_Alias{154, 'PTCP154'},
    Codepage_Alias{154, 'CSPTCP154'},

    // !IsValidCodePage(1133) 
    Codepage_Alias{1133, 'CP1133'},
    Codepage_Alias{1133, 'IBM-CP1133'},

    Codepage_Alias{874, 'CP874'},
    Codepage_Alias{874, 'WINDOWS-874'},

    // !IsValidCodePage(51932) 
    Codepage_Alias{51932, 'CP51932'},
    Codepage_Alias{51932, 'MS51932'},
    Codepage_Alias{51932, 'WINDOWS-51932'},
    Codepage_Alias{51932, 'EUC-JP'},

    Codepage_Alias{932, 'CP932'},
    Codepage_Alias{932, 'MS932'},
    Codepage_Alias{932, 'SHIFFT_JIS'},
    Codepage_Alias{932, 'SHIFFT_JIS-MS'},
    Codepage_Alias{932, 'SJIS'},
    Codepage_Alias{932, 'SJIS-MS'},
    Codepage_Alias{932, 'SJIS-OPEN'},
    Codepage_Alias{932, 'SJIS-WIN'},
    Codepage_Alias{932, 'WINDOWS-31J'},
    Codepage_Alias{932, 'WINDOWS-932'},
    Codepage_Alias{932, 'CSWINDOWS31J'},

    Codepage_Alias{50221, 'CP50221'},
    Codepage_Alias{50221, 'ISO-2022-JP'},
    Codepage_Alias{50221, 'ISO-2022-JP-MS'},
    Codepage_Alias{50221, 'ISO2022-JP'},
    Codepage_Alias{50221, 'ISO2022-JP-MS'},
    Codepage_Alias{50221, 'MS50221'},
    Codepage_Alias{50221, 'WINDOWS-50221'},

    Codepage_Alias{936, 'CP936'},
    Codepage_Alias{936, 'GBK'},
    Codepage_Alias{936, 'MS936'},
    Codepage_Alias{936, 'WINDOWS-936'},

    Codepage_Alias{950, 'CP950'},
    Codepage_Alias{950, 'BIG5'},
    Codepage_Alias{950, 'BIG5HKSCS'},
    Codepage_Alias{950, 'BIG5-HKSCS'},

    Codepage_Alias{949, 'CP949'},
    Codepage_Alias{949, 'UHC'},
    Codepage_Alias{949, 'EUC-KR'},

    Codepage_Alias{1361, 'CP1361'},
    Codepage_Alias{1361, 'JOHAB'},

    Codepage_Alias{437, '437'},
    Codepage_Alias{437, 'CP437'},
    Codepage_Alias{437, 'IBM437'},
    Codepage_Alias{437, 'CSPC8CODEPAGE437'},

    Codepage_Alias{737, 'CP737'},

    Codepage_Alias{775, 'CP775'},
    Codepage_Alias{775, 'IBM775'},
    Codepage_Alias{775, 'CSPC775BALTIC'},

    Codepage_Alias{852, '852'},
    Codepage_Alias{852, 'CP852'},
    Codepage_Alias{852, 'IBM852'},
    Codepage_Alias{852, 'CSPCP852'},

    // !IsValidCodePage(853) 
    Codepage_Alias{853, 'CP853'},

    Codepage_Alias{855, '855'},
    Codepage_Alias{855, 'CP855'},
    Codepage_Alias{855, 'IBM855'},
    Codepage_Alias{855, 'CSIBM855'},

    Codepage_Alias{857, '857'},
    Codepage_Alias{857, 'CP857'},
    Codepage_Alias{857, 'IBM857'},
    Codepage_Alias{857, 'CSIBM857'},

    // !IsValidCodePage(858) 
    Codepage_Alias{858, 'CP858'},

    Codepage_Alias{860, '860'},
    Codepage_Alias{860, 'CP860'},
    Codepage_Alias{860, 'IBM860'},
    Codepage_Alias{860, 'CSIBM860'},

    Codepage_Alias{861, '861'},
    Codepage_Alias{861, 'CP-IS'},
    Codepage_Alias{861, 'CP861'},
    Codepage_Alias{861, 'IBM861'},
    Codepage_Alias{861, 'CSIBM861'},

    Codepage_Alias{863, '863'},
    Codepage_Alias{863, 'CP863'},
    Codepage_Alias{863, 'IBM863'},
    Codepage_Alias{863, 'CSIBM863'},

    Codepage_Alias{864, 'CP864'},
    Codepage_Alias{864, 'IBM864'},
    Codepage_Alias{864, 'CSIBM864'},

    Codepage_Alias{865, '865'},
    Codepage_Alias{865, 'CP865'},
    Codepage_Alias{865, 'IBM865'},
    Codepage_Alias{865, 'CSIBM865'},

    Codepage_Alias{869, '869'},
    Codepage_Alias{869, 'CP-GR'},
    Codepage_Alias{869, 'CP869'},
    Codepage_Alias{869, 'IBM869'},
    Codepage_Alias{869, 'CSIBM869'},

    // !IsValidCodePage(1152) 
    Codepage_Alias{1125, 'CP1125'},

    //
    // * Code Page Identifiers
    // * https://learn.microsoft.com/en-us/windows/win32/intl/code-page-identifiers
     
    Codepage_Alias{37, 'IBM037'}, // IBM EBCDIC US-Canada 
    Codepage_Alias{437, 'IBM437'}, // OEM United States 
    Codepage_Alias{500, 'IBM500'}, // IBM EBCDIC International 
    Codepage_Alias{708, 'ASMO-708'}, // Arabic (ASMO 708) 
    // 709 		Arabic (ASMO-449+, BCON V4) 
    // 710 		Arabic - Transparent Arabic 
    Codepage_Alias{720, 'DOS-720'}, // Arabic (Transparent ASMO); Arabic (DOS) 
    Codepage_Alias{737, 'ibm737'}, // OEM Greek (formerly 437G); Greek (DOS) 
    Codepage_Alias{775, 'ibm775'}, // OEM Baltic; Baltic (DOS) 
    Codepage_Alias{850, 'ibm850'}, // OEM Multilingual Latin 1; Western European (DOS) 
    Codepage_Alias{852, 'ibm852'}, // OEM Latin 2; Central European (DOS) 
    Codepage_Alias{855, 'IBM855'}, // OEM Cyrillic (primarily Russian) 
    Codepage_Alias{857, 'ibm857'}, // OEM Turkish; Turkish (DOS) 
    Codepage_Alias{858, 'IBM00858'}, // OEM Multilingual Latin 1 + Euro symbol 
    Codepage_Alias{860, 'IBM860'}, // OEM Portuguese; Portuguese (DOS) 
    Codepage_Alias{861, 'ibm861'}, // OEM Icelandic; Icelandic (DOS) 
    Codepage_Alias{862, 'DOS-862'}, // OEM Hebrew; Hebrew (DOS) 
    Codepage_Alias{863, 'IBM863'}, // OEM French Canadian; French Canadian (DOS) 
    Codepage_Alias{864, 'IBM864'}, // OEM Arabic; Arabic (864) 
    Codepage_Alias{865, 'IBM865'}, // OEM Nordic; Nordic (DOS) 
    Codepage_Alias{866, 'cp866'}, // OEM Russian; Cyrillic (DOS) 
    Codepage_Alias{869, 'ibm869'}, // OEM Modern Greek; Greek, Modern (DOS) 
    Codepage_Alias{870, 'IBM870'}, // IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2 
    Codepage_Alias{874, 'windows-874'}, // ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows) 
    Codepage_Alias{875, 'cp875'}, // IBM EBCDIC Greek Modern 
    Codepage_Alias{932, 'shift_jis'}, // ANSI/OEM Japanese; Japanese (Shift-JIS) 
    Codepage_Alias{932, 'shift-jis'}, // alternative name for it 
    Codepage_Alias{936, 'gb2312'}, // ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312) 
    Codepage_Alias{949, 'ks_c_5601-1987'}, // ANSI/OEM Korean (Unified Hangul Code) 
    Codepage_Alias{950, 'big5'}, // ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5) 
    Codepage_Alias{950, 'big5hkscs'}, // ANSI/OEM Traditional Chinese (Hong Kong SAR); Chinese Traditional (Big5-HKSCS) 
    Codepage_Alias{950, 'big5-hkscs'}, // alternative name for it 
    Codepage_Alias{1026, 'IBM1026'}, // IBM EBCDIC Turkish (Latin 5) 
    Codepage_Alias{1047, 'IBM01047'}, // IBM EBCDIC Latin 1/Open System 
    Codepage_Alias{1140, 'IBM01140'}, // IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro) 
    Codepage_Alias{1141, 'IBM01141'}, // IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro) 
    Codepage_Alias{1142, 'IBM01142'}, // IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro) 
    Codepage_Alias{1143, 'IBM01143'}, // IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro) 
    Codepage_Alias{1144, 'IBM01144'}, // IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro) 
    Codepage_Alias{1145, 'IBM01145'}, // IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro) 
    Codepage_Alias{1146, 'IBM01146'}, // IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro) 
    Codepage_Alias{1147, 'IBM01147'}, // IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro) 
    Codepage_Alias{1148, 'IBM01148'}, // IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro) 
    Codepage_Alias{1149, 'IBM01149'}, // IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro) 
    Codepage_Alias{1200, 'utf-16'}, // Unicode UTF-16, little endian byte order (BMP of ISO 10646); available only to managed applications 
    Codepage_Alias{1201, 'unicodeFFFE'}, // Unicode UTF-16, big endian byte order; available only to managed applications 
    Codepage_Alias{1250, 'windows-1250'}, // ANSI Central European; Central European (Windows) 
    Codepage_Alias{1251, 'windows-1251'}, // ANSI Cyrillic; Cyrillic (Windows) 
    Codepage_Alias{1252, 'windows-1252'}, // ANSI Latin 1; Western European (Windows) 
    Codepage_Alias{1253, 'windows-1253'}, // ANSI Greek; Greek (Windows) 
    Codepage_Alias{1254, 'windows-1254'}, // ANSI Turkish; Turkish (Windows) 
    Codepage_Alias{1255, 'windows-1255'}, // ANSI Hebrew; Hebrew (Windows) 
    Codepage_Alias{1256, 'windows-1256'}, // ANSI Arabic; Arabic (Windows) 
    Codepage_Alias{1257, 'windows-1257'}, // ANSI Baltic; Baltic (Windows) 
    Codepage_Alias{1258, 'windows-1258'}, // ANSI/OEM Vietnamese; Vietnamese (Windows) 
    Codepage_Alias{1361, 'Johab'}, // Korean (Johab) 
    Codepage_Alias{10000, 'macintosh'}, // MAC Roman; Western European (Mac) 
    Codepage_Alias{10001, 'x-mac-japanese'}, // Japanese (Mac) 
    Codepage_Alias{10002, 'x-mac-chinesetrad'}, // MAC Traditional Chinese (Big5); Chinese Traditional (Mac) 
    Codepage_Alias{10003, 'x-mac-korean'}, // Korean (Mac) 
    Codepage_Alias{10004, 'x-mac-arabic'}, // Arabic (Mac) 
    Codepage_Alias{10005, 'x-mac-hebrew'}, // Hebrew (Mac) 
    Codepage_Alias{10006, 'x-mac-greek'}, // Greek (Mac) 
    Codepage_Alias{10007, 'x-mac-cyrillic'}, // Cyrillic (Mac) 
    Codepage_Alias{10008, 'x-mac-chinesesimp'}, // MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac) 
    Codepage_Alias{10010, 'x-mac-romanian'}, // Romanian (Mac) 
    Codepage_Alias{10017, 'x-mac-ukrainian'}, // Ukrainian (Mac) 
    Codepage_Alias{10021, 'x-mac-thai'}, // Thai (Mac) 
    Codepage_Alias{10029, 'x-mac-ce'}, // MAC Latin 2; Central European (Mac) 
    Codepage_Alias{10079, 'x-mac-icelandic'}, // Icelandic (Mac) 
    Codepage_Alias{10081, 'x-mac-turkish'}, // Turkish (Mac) 
    Codepage_Alias{10082, 'x-mac-croatian'}, // Croatian (Mac) 
    Codepage_Alias{12000, 'utf-32'}, // Unicode UTF-32, little endian byte order; available only to managed applications 
    Codepage_Alias{12001, 'utf-32BE'}, // Unicode UTF-32, big endian byte order; available only to managed applications 
	Codepage_Alias{20000, 'x-Chinese_CNS'}, // CNS Taiwan; Chinese Traditional (CNS) 
    Codepage_Alias{20001, 'x-cp20001'}, // TCA Taiwan 
    Codepage_Alias{20002, 'x_Chinese-Eten'}, // Eten Taiwan; Chinese Traditional (Eten) 
    Codepage_Alias{20003, 'x-cp20003'}, // IBM5550 Taiwan 
    Codepage_Alias{20004, 'x-cp20004'}, // TeleText Taiwan 
    Codepage_Alias{20005, 'x-cp20005'}, // Wang Taiwan 
    Codepage_Alias{20105, 'x-IA5'}, // IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5) 
    Codepage_Alias{20106, 'x-IA5-German'}, // IA5 German (7-bit) 
    Codepage_Alias{20107, 'x-IA5-Swedish'}, // IA5 Swedish (7-bit) 
    Codepage_Alias{20108, 'x-IA5-Norwegian'}, // IA5 Norwegian (7-bit) 
    Codepage_Alias{20127, 'us-ascii'}, // US-ASCII (7-bit) 
    Codepage_Alias{20261, 'x-cp20261'}, // T.61 
    Codepage_Alias{20269, 'x-cp20269'}, // ISO 6937 Non-Spacing Accent 
    Codepage_Alias{20273, 'IBM273'}, // IBM EBCDIC Germany 
    Codepage_Alias{20277, 'IBM277'}, // IBM EBCDIC Denmark-Norway 
    Codepage_Alias{20278, 'IBM278'}, // IBM EBCDIC Finland-Sweden 
    Codepage_Alias{20280, 'IBM280'}, // IBM EBCDIC Italy 
    Codepage_Alias{20284, 'IBM284'}, // IBM EBCDIC Latin America-Spain 
    Codepage_Alias{20285, 'IBM285'}, // IBM EBCDIC United Kingdom 
    Codepage_Alias{20290, 'IBM290'}, // IBM EBCDIC Japanese Katakana Extended 
    Codepage_Alias{20297, 'IBM297'}, // IBM EBCDIC France 
    Codepage_Alias{20420, 'IBM420'}, // IBM EBCDIC Arabic 
    Codepage_Alias{20423, 'IBM423'}, // IBM EBCDIC Greek 
    Codepage_Alias{20424, 'IBM424'}, // IBM EBCDIC Hebrew 
    Codepage_Alias{20833, 'x-EBCDIC-KoreanExtended'}, // IBM EBCDIC Korean Extended 
    Codepage_Alias{20838, 'IBM-Thai'}, // IBM EBCDIC Thai 
    Codepage_Alias{20866, 'koi8-r'}, // Russian (KOI8-R); Cyrillic (KOI8-R) 
    Codepage_Alias{20871, 'IBM871'}, // IBM EBCDIC Icelandic 
    Codepage_Alias{20880, 'IBM880'}, // IBM EBCDIC Cyrillic Russian 
    Codepage_Alias{20905, 'IBM905'}, // IBM EBCDIC Turkish 
    Codepage_Alias{20924, 'IBM00924'}, // IBM EBCDIC Latin 1/Open System (1047 + Euro symbol) 
    Codepage_Alias{20932, 'EUC-JP'}, // Japanese (JIS 0208-1990 and 0121-1990) 
    Codepage_Alias{20936, 'x-cp20936'}, // Simplified Chinese (GB2312); Chinese Simplified (GB2312-80) 
    Codepage_Alias{20949, 'x-cp20949'}, // Korean Wansung 
    Codepage_Alias{21025, 'cp1025'}, // IBM EBCDIC Cyrillic Serbian-Bulgarian 
    // 21027 		(deprecated) 
    Codepage_Alias{21866, 'koi8-u'}, // Ukrainian (KOI8-U); Cyrillic (KOI8-U) 
    Codepage_Alias{28591, 'iso-8859-1'}, // ISO 8859-1 Latin 1; Western European (ISO) 
    Codepage_Alias{28591, 'iso8859-1'}, // ISO 8859-1 Latin 1; Western European (ISO) 
    Codepage_Alias{28591, 'iso_8859-1'},
    Codepage_Alias{28591, 'iso_8859_1'},
    Codepage_Alias{28592, 'iso-8859-2'}, // ISO 8859-2 Central European; Central European (ISO) 
    Codepage_Alias{28592, 'iso8859-2'}, // ISO 8859-2 Central European; Central European (ISO) 
    Codepage_Alias{28592, 'iso_8859-2'},
    Codepage_Alias{28592, 'iso_8859_2'},
    Codepage_Alias{28593, 'iso-8859-3'}, // ISO 8859-3 Latin 3 
    Codepage_Alias{28593, 'iso8859-3'}, // ISO 8859-3 Latin 3 
    Codepage_Alias{28593, 'iso_8859-3'},
    Codepage_Alias{28593, 'iso_8859_3'},
    Codepage_Alias{28594, 'iso-8859-4'}, // ISO 8859-4 Baltic 
    Codepage_Alias{28594, 'iso8859-4'}, // ISO 8859-4 Baltic 
    Codepage_Alias{28594, 'iso_8859-4'},
    Codepage_Alias{28594, 'iso_8859_4'},
    Codepage_Alias{28595, 'iso-8859-5'}, // ISO 8859-5 Cyrillic 
    Codepage_Alias{28595, 'iso8859-5'}, // ISO 8859-5 Cyrillic 
    Codepage_Alias{28595, 'iso_8859-5'},
    Codepage_Alias{28595, 'iso_8859_5'},
    Codepage_Alias{28596, 'iso-8859-6'}, // ISO 8859-6 Arabic 
    Codepage_Alias{28596, 'iso8859-6'}, // ISO 8859-6 Arabic 
    Codepage_Alias{28596, 'iso_8859-6'},
    Codepage_Alias{28596, 'iso_8859_6'},
    Codepage_Alias{28597, 'iso-8859-7'}, // ISO 8859-7 Greek 
    Codepage_Alias{28597, 'iso8859-7'}, // ISO 8859-7 Greek 
    Codepage_Alias{28597, 'iso_8859-7'},
    Codepage_Alias{28597, 'iso_8859_7'},
    Codepage_Alias{28598, 'iso-8859-8'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Visual) 
    Codepage_Alias{28598, 'iso8859-8'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Visual) 
    Codepage_Alias{28598, 'iso_8859-8'},
    Codepage_Alias{28598, 'iso_8859_8'},
    Codepage_Alias{28599, 'iso-8859-9'}, // ISO 8859-9 Turkish 
    Codepage_Alias{28599, 'iso8859-9'}, // ISO 8859-9 Turkish 
    Codepage_Alias{28599, 'iso_8859-9'},
    Codepage_Alias{28599, 'iso_8859_9'},
    Codepage_Alias{28603, 'iso-8859-13'}, // ISO 8859-13 Estonian 
    Codepage_Alias{28603, 'iso8859-13'}, // ISO 8859-13 Estonian 
    Codepage_Alias{28603, 'iso_8859-13'},
    Codepage_Alias{28603, 'iso_8859_13'},
    Codepage_Alias{28605, 'iso-8859-15'}, // ISO 8859-15 Latin 9 
    Codepage_Alias{28605, 'iso8859-15'}, // ISO 8859-15 Latin 9 
    Codepage_Alias{28605, 'iso_8859-15'},
    Codepage_Alias{28605, 'iso_8859_15'},
    Codepage_Alias{29001, 'x-Europa'}, // Europa 3 
    Codepage_Alias{38598, 'iso-8859-8-i'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Logical) 
    Codepage_Alias{38598, 'iso8859-8-i'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Logical) 
    Codepage_Alias{38598, 'iso_8859-8-i'},
    Codepage_Alias{38598, 'iso_8859_8-i'},
    Codepage_Alias{50220, 'iso-2022-jp'}, // ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS) 
    Codepage_Alias{50221, 'csISO2022JP'}, // ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana) 
    Codepage_Alias{50222, 'iso-2022-jp'}, // ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI) 
    Codepage_Alias{50225, 'iso-2022-kr'}, // ISO 2022 Korean 
    Codepage_Alias{50225, 'iso2022-kr'}, // ISO 2022 Korean 
    Codepage_Alias{50227, 'x-cp50227'}, // ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022) 
    // 50229 		ISO 2022 Traditional Chinese 
    // 50930 		EBCDIC Japanese (Katakana) Extended 
    // 50931 		EBCDIC US-Canada and Japanese 
    // 50933 		EBCDIC Korean Extended and Korean 
    // 50935 		EBCDIC Simplified Chinese Extended and Simplified Chinese 
    // 50936 		EBCDIC Simplified Chinese 
    // 50937 		EBCDIC US-Canada and Traditional Chinese 
    // 50939 		EBCDIC Japanese (Latin) Extended and Japanese 
    Codepage_Alias{51932, 'euc-jp'}, // EUC Japanese 
    Codepage_Alias{51936, 'EUC-CN'}, // EUC Simplified Chinese; Chinese Simplified (EUC) 
    Codepage_Alias{51949, 'euc-kr'}, // EUC Korean 
    // 51950 		EUC Traditional Chinese 
    Codepage_Alias{52936, 'hz-gb-2312'}, // HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ) 
    Codepage_Alias{54936, 'GB18030'}, // Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030) 
    Codepage_Alias{57002, 'x-iscii-de'}, // ISCII Devanagari 
    Codepage_Alias{57003, 'x-iscii-be'}, // ISCII Bengali 
    Codepage_Alias{57004, 'x-iscii-ta'}, // ISCII Tamil 
    Codepage_Alias{57005, 'x-iscii-te'}, // ISCII Telugu 
    Codepage_Alias{57006, 'x-iscii-as'}, // ISCII Assamese 
    Codepage_Alias{57007, 'x-iscii-or'}, // ISCII Oriya 
    Codepage_Alias{57008, 'x-iscii-ka'}, // ISCII Kannada 
    Codepage_Alias{57009, 'x-iscii-ma'}, // ISCII Malayalam 
    Codepage_Alias{57010, 'x-iscii-gu'}, // ISCII Gujarati 
    Codepage_Alias{57011, 'x-iscii-pa'}, // ISCII Punjabi 
    Codepage_Alias{65000, 'utf-7'}, // Unicode (UTF-7) 
    Codepage_Alias{65001, 'utf-8'}, // Unicode (UTF-8) 
	// vfmt on
]

fn name_to_codepage(name string) int {
	if name == '' || name == 'CP_ACP' {
		return C.GetACP()
	}
	if name.len < 2 {
		return -1
	}
	name_lower := name.to_lower()
	if name_lower == 'wchar_t' {
		return 1200
	}
	// CP123
	if name_lower.starts_with('cp') {
		return name_lower[2..].int()
	}
	if name_lower.is_int() {
		return name_lower.int()
	}
	// XX123 for debug
	if name_lower.starts_with('xx') {
		return name_lower[2..].int()
	}

	for x in codepage_alias {
		if x.name.to_lower() == name_lower {
			return x.codepage
		}
	}
	return -1
}

// conv convert `fromcode` encoding string to `tocode` encoding string
fn conv(tocode string, fromcode string, src &u8, src_len int) []u8 {
	if isnil(src) || src_len <= 0 {
		return []u8{}
	}
	src_codepage := name_to_codepage(fromcode)
	dst_codepage := name_to_codepage(tocode)
	if dst_codepage <= 0 || src_codepage <= 0 {
		return []u8{}
	}
	if src_codepage == dst_codepage {
		// clone src
		mut dst_buf := []u8{len: src_len}
		unsafe { vmemcpy(dst_buf.data, src, src_len) }
		return dst_buf
	}
	// src codepage => Unicode
	unicode_len := C.MultiByteToWideChar(src_codepage, 0, src, src_len, 0, 0)
	mut unicode := []u8{len: unicode_len}
	C.MultiByteToWideChar(src_codepage, 0, src, src_len, unicode.data, unicode.len)

	// Unicode => dst codepage
	dst_len := C.WideCharToMultiByte(dst_codepage, 0, unicode.data, unicode.len, 0, 0,
		0, 0)
	mut dst := []u8{len: dst_len}
	C.WideCharToMultiByte(dst_codepage, 0, unicode.data, unicode.len, dst.data, dst.len,
		0, 0)
	unsafe { unicode.free() }
	return dst
}

// vstring_to_encoding convert vstring `str` to `tocode` encoding string
pub fn vstring_to_encoding(tocode string, str string) []u8 {
	return conv(tocode, 'UTF-8', str.str, str.len)
}

// encoding_to_vstring convert `fromcode` encoding string to vstring
pub fn encoding_to_vstring(fromcode string, str []u8) string {
	dst := conv('UTF-8', fromcode, str.data, str.len)
	defer {
		unsafe { dst.free() }
	}
	if dst.len == 0 {
		return ''
	}
	return unsafe { cstring_to_vstring(dst.data) }
}
