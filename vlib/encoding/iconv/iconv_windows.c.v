module iconv

// Module iconv provides functions convert between vstring(UTF8) to/from different encodings.
// iconv implementation using Win32 API to convert
// Idear from https://github.com/win-iconv/win-iconv

fn C.GetACP() int
fn C.GetOEMCP() int
fn C.WideCharToMultiByte(codepage u32, dwflags u32, src &u8, src_len int, dst &u8, dst_len int, default_char &u8, used_default_char &bool) int
fn C.MultiByteToWideChar(codepage u32, dwflags u32, src &u8, src_len int, dst &u8, dst_len int) int

struct Codepage_Alias {
	codepage int
	name     string
}

const codepage_alias = [
	// NOTE! name field string MUST in uppercase!
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
    Codepage_Alias{1200, 'UNICODE'},	// for iconv 

    Codepage_Alias{1201, 'CP1201'},
    Codepage_Alias{1201, 'UTF16BE'},
    Codepage_Alias{1201, 'UTF-16BE'},
    Codepage_Alias{1201, 'UCS2BE'},
    Codepage_Alias{1201, 'UCS-2BE'},
    Codepage_Alias{1201, 'UNICODEFFFE'},

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
    Codepage_Alias{737, 'IBM737'}, // OEM Greek (formerly 437G); Greek (DOS) 
    Codepage_Alias{775, 'IBM775'}, // OEM Baltic; Baltic (DOS) 
    Codepage_Alias{850, 'IBM850'}, // OEM Multilingual Latin 1; Western European (DOS) 
    Codepage_Alias{852, 'IBM852'}, // OEM Latin 2; Central European (DOS) 
    Codepage_Alias{855, 'IBM855'}, // OEM Cyrillic (primarily Russian) 
    Codepage_Alias{857, 'IBM857'}, // OEM Turkish; Turkish (DOS) 
    Codepage_Alias{858, 'IBM00858'}, // OEM Multilingual Latin 1 + Euro symbol 
    Codepage_Alias{860, 'IBM860'}, // OEM Portuguese; Portuguese (DOS) 
    Codepage_Alias{861, 'IBM861'}, // OEM Icelandic; Icelandic (DOS) 
    Codepage_Alias{862, 'DOS-862'}, // OEM Hebrew; Hebrew (DOS) 
    Codepage_Alias{863, 'IBM863'}, // OEM French Canadian; French Canadian (DOS) 
    Codepage_Alias{864, 'IBM864'}, // OEM Arabic; Arabic (864) 
    Codepage_Alias{865, 'IBM865'}, // OEM Nordic; Nordic (DOS) 
    Codepage_Alias{866, 'CP866'}, // OEM Russian; Cyrillic (DOS) 
    Codepage_Alias{869, 'IBM869'}, // OEM Modern Greek; Greek, Modern (DOS) 
    Codepage_Alias{870, 'IBM870'}, // IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2 
    Codepage_Alias{874, 'WINDOWS-874'}, // ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows) 
    Codepage_Alias{875, 'CP875'}, // IBM EBCDIC Greek Modern 
    Codepage_Alias{932, 'SHIFT_JIS'}, // ANSI/OEM Japanese; Japanese (Shift-JIS) 
    Codepage_Alias{932, 'SHIFT-JIS'}, // alternative name for it 
    Codepage_Alias{936, 'GB2312'}, // ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312) 
    Codepage_Alias{949, 'KS_C_5601-1987'}, // ANSI/OEM Korean (Unified Hangul Code) 
    Codepage_Alias{950, 'BIG5'}, // ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5) 
    Codepage_Alias{950, 'BIG5HKSCS'}, // ANSI/OEM Traditional Chinese (Hong Kong SAR); Chinese Traditional (Big5-HKSCS) 
    Codepage_Alias{950, 'BIG5-HKSCS'}, // alternative name for it 
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
    Codepage_Alias{1200, 'UTF-16'}, // Unicode UTF-16, little endian byte order (BMP of ISO 10646); available only to managed applications 
    Codepage_Alias{1201, 'UNICODEFFFE'}, // Unicode UTF-16, big endian byte order; available only to managed applications 
    Codepage_Alias{1250, 'WINDOWS-1250'}, // ANSI Central European; Central European (Windows) 
    Codepage_Alias{1251, 'WINDOWS-1251'}, // ANSI Cyrillic; Cyrillic (Windows) 
    Codepage_Alias{1252, 'WINDOWS-1252'}, // ANSI Latin 1; Western European (Windows) 
    Codepage_Alias{1253, 'WINDOWS-1253'}, // ANSI Greek; Greek (Windows) 
    Codepage_Alias{1254, 'WINDOWS-1254'}, // ANSI Turkish; Turkish (Windows) 
    Codepage_Alias{1255, 'WINDOWS-1255'}, // ANSI Hebrew; Hebrew (Windows) 
    Codepage_Alias{1256, 'WINDOWS-1256'}, // ANSI Arabic; Arabic (Windows) 
    Codepage_Alias{1257, 'WINDOWS-1257'}, // ANSI Baltic; Baltic (Windows) 
    Codepage_Alias{1258, 'WINDOWS-1258'}, // ANSI/OEM Vietnamese; Vietnamese (Windows) 
    Codepage_Alias{1361, 'JOHAB'}, // Korean (Johab) 
    Codepage_Alias{10000, 'MACINTOSH'}, // MAC Roman; Western European (Mac) 
    Codepage_Alias{10001, 'X-MAC-JAPANESE'}, // Japanese (Mac) 
    Codepage_Alias{10002, 'X-MAC-CHINESETRAD'}, // MAC Traditional Chinese (Big5); Chinese Traditional (Mac) 
    Codepage_Alias{10003, 'X-MAC-KOREAN'}, // Korean (Mac) 
    Codepage_Alias{10004, 'X-MAC-ARABIC'}, // Arabic (Mac) 
    Codepage_Alias{10005, 'X-MAC-HEBREW'}, // Hebrew (Mac) 
    Codepage_Alias{10006, 'X-MAC-GREEK'}, // Greek (Mac) 
    Codepage_Alias{10007, 'X-MAC-CYRILLIC'}, // Cyrillic (Mac) 
    Codepage_Alias{10008, 'X-MAC-CHINESESIMP'}, // MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac) 
    Codepage_Alias{10010, 'X-MAC-ROMANIAN'}, // Romanian (Mac) 
    Codepage_Alias{10017, 'X-MAC-UKRAINIAN'}, // Ukrainian (Mac) 
    Codepage_Alias{10021, 'X-MAC-THAI'}, // Thai (Mac) 
    Codepage_Alias{10029, 'X-MAC-CE'}, // MAC Latin 2; Central European (Mac) 
    Codepage_Alias{10079, 'X-MAC-ICELANDIC'}, // Icelandic (Mac) 
    Codepage_Alias{10081, 'X-MAC-TURKISH'}, // Turkish (Mac) 
    Codepage_Alias{10082, 'X-MAC-CROATIAN'}, // Croatian (Mac) 
    Codepage_Alias{12000, 'UTF-32'}, // Unicode UTF-32, little endian byte order; available only to managed applications 
    Codepage_Alias{12001, 'UTF-32BE'}, // Unicode UTF-32, big endian byte order; available only to managed applications 
	Codepage_Alias{20000, 'X-CHINESE_CNS'}, // CNS Taiwan; Chinese Traditional (CNS) 
    Codepage_Alias{20001, 'X-CP20001'}, // TCA Taiwan 
    Codepage_Alias{20002, 'X_CHINESE-ETEN'}, // Eten Taiwan; Chinese Traditional (Eten) 
    Codepage_Alias{20003, 'X-CP20003'}, // IBM5550 Taiwan 
    Codepage_Alias{20004, 'X-CP20004'}, // TeleText Taiwan 
    Codepage_Alias{20005, 'X-CP20005'}, // Wang Taiwan 
    Codepage_Alias{20105, 'X-IA5'}, // IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5) 
    Codepage_Alias{20106, 'X-IA5-GERMAN'}, // IA5 German (7-bit) 
    Codepage_Alias{20107, 'X-IA5-SWEDISH'}, // IA5 Swedish (7-bit) 
    Codepage_Alias{20108, 'X-IA5-NORWEGIAN'}, // IA5 Norwegian (7-bit) 
    Codepage_Alias{20127, 'US-ASCII'}, // US-ASCII (7-bit) 
    Codepage_Alias{20261, 'X-CP20261'}, // T.61 
    Codepage_Alias{20269, 'X-CP20269'}, // ISO 6937 Non-Spacing Accent 
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
    Codepage_Alias{20833, 'X-EBCDIC-KOREANEXTENDED'}, // IBM EBCDIC Korean Extended 
    Codepage_Alias{20838, 'IBM-THAI'}, // IBM EBCDIC Thai 
    Codepage_Alias{20866, 'KOI8-R'}, // Russian (KOI8-R); Cyrillic (KOI8-R) 
    Codepage_Alias{20871, 'IBM871'}, // IBM EBCDIC Icelandic 
    Codepage_Alias{20880, 'IBM880'}, // IBM EBCDIC Cyrillic Russian 
    Codepage_Alias{20905, 'IBM905'}, // IBM EBCDIC Turkish 
    Codepage_Alias{20924, 'IBM00924'}, // IBM EBCDIC Latin 1/Open System (1047 + Euro symbol) 
    Codepage_Alias{20932, 'EUC-JP'}, // Japanese (JIS 0208-1990 and 0121-1990) 
    Codepage_Alias{20936, 'X-CP20936'}, // Simplified Chinese (GB2312); Chinese Simplified (GB2312-80) 
    Codepage_Alias{20949, 'X-CP20949'}, // Korean Wansung 
    Codepage_Alias{21025, 'CP1025'}, // IBM EBCDIC Cyrillic Serbian-Bulgarian 
    // 21027 		(deprecated) 
    Codepage_Alias{21866, 'KOI8-U'}, // Ukrainian (KOI8-U); Cyrillic (KOI8-U) 
    Codepage_Alias{28591, 'ISO-8859-1'}, // ISO 8859-1 Latin 1; Western European (ISO) 
    Codepage_Alias{28591, 'ISO8859-1'}, // ISO 8859-1 Latin 1; Western European (ISO) 
    Codepage_Alias{28591, 'ISO_8859-1'},
    Codepage_Alias{28591, 'ISO_8859_1'},
    Codepage_Alias{28592, 'ISO-8859-2'}, // ISO 8859-2 Central European; Central European (ISO) 
    Codepage_Alias{28592, 'ISO8859-2'}, // ISO 8859-2 Central European; Central European (ISO) 
    Codepage_Alias{28592, 'ISO_8859-2'},
    Codepage_Alias{28592, 'ISO_8859_2'},
    Codepage_Alias{28593, 'ISO-8859-3'}, // ISO 8859-3 Latin 3 
    Codepage_Alias{28593, 'ISO8859-3'}, // ISO 8859-3 Latin 3 
    Codepage_Alias{28593, 'ISO_8859-3'},
    Codepage_Alias{28593, 'ISO_8859_3'},
    Codepage_Alias{28594, 'ISO-8859-4'}, // ISO 8859-4 Baltic 
    Codepage_Alias{28594, 'ISO8859-4'}, // ISO 8859-4 Baltic 
    Codepage_Alias{28594, 'ISO_8859-4'},
    Codepage_Alias{28594, 'ISO_8859_4'},
    Codepage_Alias{28595, 'ISO-8859-5'}, // ISO 8859-5 Cyrillic 
    Codepage_Alias{28595, 'ISO8859-5'}, // ISO 8859-5 Cyrillic 
    Codepage_Alias{28595, 'ISO_8859-5'},
    Codepage_Alias{28595, 'ISO_8859_5'},
    Codepage_Alias{28596, 'ISO-8859-6'}, // ISO 8859-6 Arabic 
    Codepage_Alias{28596, 'ISO8859-6'}, // ISO 8859-6 Arabic 
    Codepage_Alias{28596, 'ISO_8859-6'},
    Codepage_Alias{28596, 'ISO_8859_6'},
    Codepage_Alias{28597, 'ISO-8859-7'}, // ISO 8859-7 Greek 
    Codepage_Alias{28597, 'ISO8859-7'}, // ISO 8859-7 Greek 
    Codepage_Alias{28597, 'ISO_8859-7'},
    Codepage_Alias{28597, 'ISO_8859_7'},
    Codepage_Alias{28598, 'ISO-8859-8'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Visual) 
    Codepage_Alias{28598, 'ISO8859-8'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Visual) 
    Codepage_Alias{28598, 'ISO_8859-8'},
    Codepage_Alias{28598, 'ISO_8859_8'},
    Codepage_Alias{28599, 'ISO-8859-9'}, // ISO 8859-9 Turkish 
    Codepage_Alias{28599, 'ISO8859-9'}, // ISO 8859-9 Turkish 
    Codepage_Alias{28599, 'ISO_8859-9'},
    Codepage_Alias{28599, 'ISO_8859_9'},
    Codepage_Alias{28603, 'ISO-8859-13'}, // ISO 8859-13 Estonian 
    Codepage_Alias{28603, 'ISO8859-13'}, // ISO 8859-13 Estonian 
    Codepage_Alias{28603, 'ISO_8859-13'},
    Codepage_Alias{28603, 'ISO_8859_13'},
    Codepage_Alias{28605, 'ISO-8859-15'}, // ISO 8859-15 Latin 9 
    Codepage_Alias{28605, 'ISO8859-15'}, // ISO 8859-15 Latin 9 
    Codepage_Alias{28605, 'ISO_8859-15'},
    Codepage_Alias{28605, 'ISO_8859_15'},
    Codepage_Alias{29001, 'X-EUROPA'}, // Europa 3 
    Codepage_Alias{38598, 'ISO-8859-8-I'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Logical) 
    Codepage_Alias{38598, 'ISO8859-8-I'}, // ISO 8859-8 Hebrew; Hebrew (ISO-Logical) 
    Codepage_Alias{38598, 'ISO_8859-8-I'},
    Codepage_Alias{38598, 'ISO_8859_8-I'},
    Codepage_Alias{50220, 'ISO-2022-JP'}, // ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS) 
    Codepage_Alias{50221, 'CSISO2022JP'}, // ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana) 
    Codepage_Alias{50222, 'ISO-2022-JP'}, // ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI) 
    Codepage_Alias{50225, 'ISO-2022-KR'}, // ISO 2022 Korean 
    Codepage_Alias{50225, 'ISO2022-KR'}, // ISO 2022 Korean 
    Codepage_Alias{50227, 'X-CP50227'}, // ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022) 
    // 50229 		ISO 2022 Traditional Chinese 
    // 50930 		EBCDIC Japanese (Katakana) Extended 
    // 50931 		EBCDIC US-Canada and Japanese 
    // 50933 		EBCDIC Korean Extended and Korean 
    // 50935 		EBCDIC Simplified Chinese Extended and Simplified Chinese 
    // 50936 		EBCDIC Simplified Chinese 
    // 50937 		EBCDIC US-Canada and Traditional Chinese 
    // 50939 		EBCDIC Japanese (Latin) Extended and Japanese 
    Codepage_Alias{51932, 'EUC-JP'}, // EUC Japanese 
    Codepage_Alias{51936, 'EUC-CN'}, // EUC Simplified Chinese; Chinese Simplified (EUC) 
    Codepage_Alias{51949, 'EUC-KR'}, // EUC Korean 
    // 51950 		EUC Traditional Chinese 
    Codepage_Alias{52936, 'HZ-GB-2312'}, // HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ) 
    Codepage_Alias{54936, 'GB18030'}, // Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030) 
    Codepage_Alias{57002, 'X-ISCII-DE'}, // ISCII Devanagari 
    Codepage_Alias{57003, 'X-ISCII-BE'}, // ISCII Bengali 
    Codepage_Alias{57004, 'X-ISCII-TA'}, // ISCII Tamil 
    Codepage_Alias{57005, 'X-ISCII-TE'}, // ISCII Telugu 
    Codepage_Alias{57006, 'X-ISCII-AS'}, // ISCII Assamese 
    Codepage_Alias{57007, 'X-ISCII-OR'}, // ISCII Oriya 
    Codepage_Alias{57008, 'X-ISCII-KA'}, // ISCII Kannada 
    Codepage_Alias{57009, 'X-ISCII-MA'}, // ISCII Malayalam 
    Codepage_Alias{57010, 'X-ISCII-GU'}, // ISCII Gujarati 
    Codepage_Alias{57011, 'X-ISCII-PA'}, // ISCII Punjabi 
    Codepage_Alias{65000, 'UTF-7'}, // Unicode (UTF-7) 
    Codepage_Alias{65001, 'UTF-8'}, // Unicode (UTF-8) 
	// vfmt on
]

fn name_to_codepage(name string) int {
	// performance hack
	if name == 'UTF-8' {
		return 65001
	}

	name_upper := name.to_upper()
	if name_upper == '' || name_upper == 'CP_ACP' || name_upper == 'ANSI' {
		return C.GetACP()
	}
	if name_upper == 'CP_OEMCP' {
		return C.GetOEMCP()
	}
	if name_upper.len < 2 {
		return -1
	}
	if name_upper == 'WCHAR_T' {
		return 1200
	}
	// CP123
	if name_upper.starts_with('CP') {
		return name_upper[2..].int()
	}
	if name_upper.is_int() {
		return name_upper.int()
	}
	// XX123 for debug
	if name_upper.starts_with('xx') {
		return name_upper[2..].int()
	}

	for x in codepage_alias {
		if x.name == name_upper {
			return x.codepage
		}
	}
	return -1
}

// https://www.cnblogs.com/findumars/p/6376034.html
@[direct_array_access]
fn utf32_to_utf16(src &u8, src_len int, is_src_little_endian bool, is_dst_little_endian bool) ![]u8 {
	mut dst := []u8{len: src_len}
	mut sptr := unsafe { &u32(src) }
	mut dptr := &u16(dst.data)
	mut src_idx := 0
	mut dst_idx := 0
	mut c := u32(0)
	mut t := u16(0)
	for {
		if src_idx == src_len / 4 {
			break
		}
		unsafe {
			c = sptr[src_idx]
		}
		if !is_src_little_endian {
			c = reverse_u32(c)
		}
		src_idx++
		if c <= 0xFFFF {
			t = u16(c)
			if !is_dst_little_endian {
				t = reverse_u16(t)
			}
			unsafe {
				dptr[dst_idx] = t
			}
			dst_idx++
		} else if c <= 0xEFFFF {
			t = u16((0xD800 + (c >> 10) - 0x40)) // high

			if !is_dst_little_endian {
				t = reverse_u16(t)
			}
			unsafe {
				dptr[dst_idx] = t
			}
			dst_idx++
			t = u16(0xDC00 + (c & 0x03FF)) // low
			if !is_dst_little_endian {
				t = reverse_u16(t)
			}
			unsafe {
				dptr[dst_idx] = t
			}
			dst_idx++
		} else {
			return error('invalid UTF-32LE encoding')
		}
	}
	dst.trim(dst_idx * 2)
	return dst
}

// https://www.cnblogs.com/findumars/p/6376034.html
@[direct_array_access]
fn utf16_to_utf32(src &u8, src_len int, is_src_little_endian bool, is_dst_little_endian bool) ![]u8 {
	mut dst := []u8{len: src_len * 2}
	mut sptr := unsafe { &u16(src) }
	mut dptr := &u32(dst.data)
	mut w1 := u16(0)
	mut w2 := u16(0)
	mut t := u32(0)
	mut src_idx := 0
	mut dst_idx := 0
	for {
		if src_idx == src_len / 2 {
			break
		}
		unsafe {
			w1 = sptr[src_idx]
		}
		if !is_src_little_endian {
			w1 = reverse_u16(w1)
		}
		src_idx++
		if w1 >= 0xD800 && w1 <= 0xDFFF {
			if w1 < 0xDC00 {
				if src_idx == src_len / 2 {
					return error('invalid UTF-16LE encoding')
				}
				unsafe {
					w2 = sptr[src_idx]
				}
				if !is_src_little_endian {
					w2 = reverse_u16(w2)
				}
				if w2 >= 0xDC00 && w2 <= 0xDFFF {
					t = (w2 & 0x03FF) + (((w1 & 0x03FF) + 0x40) << 10)
					if !is_dst_little_endian {
						t = reverse_u32(t)
					}
					unsafe {
						dptr[dst_idx] = t
					}
					dst_idx++
				}
			} else {
				return error('invalid UTF-16LE encoding')
			}
		} else {
			t = w1
			if !is_dst_little_endian {
				t = reverse_u32(t)
			}
			unsafe {
				dptr[dst_idx] = t
			}
			dst_idx++
		}
	}
	dst.trim(dst_idx * 4)
	return dst
}

// conv convert `fromcode` encoding string to `tocode` encoding string
@[direct_array_access]
fn conv(tocode string, fromcode string, src &u8, src_len int) ![]u8 {
	if src_len < 0 {
		return error('src length error')
	}
	src_codepage := name_to_codepage(fromcode)
	dst_codepage := name_to_codepage(tocode)
	if src_codepage <= 0 {
		return error('fromcode ${fromcode} does not exist')
	}
	if dst_codepage <= 0 {
		return error('tocode ${tocode} does not exist')
	}

	if src_codepage == dst_codepage {
		// clone src
		mut dst_buf := []u8{len: src_len}
		unsafe { vmemcpy(dst_buf.data, src, src_len) }
		return dst_buf
	}

	mut unicode := []u8{}
	// src codepage => Unicode
	match src_codepage {
		1200 {
			// src already in Unicode(UTF-16LE) encoding, just clone src
			unsafe {
				unicode.grow_len(src_len)
				vmemcpy(unicode.data, src, src_len)
			}
		}
		1201 {
			// Windows does not support UTF-16BE
			// byte swap each 16 bit character element
			unsafe {
				unicode.grow_len(src_len)
				vmemcpy(unicode.data, src, src_len)
			}
			mut eptr := &u16(unicode.data)
			for i in 0 .. src_len / 2 {
				unsafe {
					eptr[i] = reverse_u16(eptr[i])
				}
			}
		}
		12000 {
			// Windows does not support UTF-32LE
			unicode = utf32_to_utf16(src, src_len, true, true)!
		}
		12001 {
			// Windows does not support UTF-32BE
			unicode = utf32_to_utf16(src, src_len, false, true)!
		}
		else {
			char_num := C.MultiByteToWideChar(src_codepage, 0, src, src_len, 0, 0)
			if char_num == 0 {
				return error('MultiByteToWideChar fail: src contain zero ${fromcode} character')
			}
			unsafe { unicode.grow_len(char_num * 2) } // every char take 2 bytes
			C.MultiByteToWideChar(src_codepage, 0, src, src_len, unicode.data, unicode.len)
		}
	}

	mut dst := []u8{}
	// Unicode => dst codepage
	match dst_codepage {
		1200 {
			// dst codepage is Unicode, just return unicode
			return unicode
		}
		1201 {
			// Windows does not support UTF-16BE
			// byte swap each 16 bit character element
			mut eptr := &u16(unicode.data)
			for i in 0 .. unicode.len / 2 {
				unsafe {
					eptr[i] = reverse_u16(eptr[i])
				}
			}
			return unicode
		}
		12000 {
			// Windows does not support UTF-32LE
			dst = utf16_to_utf32(unicode.data, unicode.len, true, true)!
		}
		12001 {
			// Windows does not support UTF-32BE
			dst = utf16_to_utf32(unicode.data, unicode.len, true, false)!
		}
		else {
			dst_len := C.WideCharToMultiByte(dst_codepage, 0, unicode.data, unicode.len / 2,
				0, 0, 0, 0)
			if dst_len == 0 {
				return error('WideCharToMultiByte fail: src contain zero unicode character')
			}
			unsafe { dst.grow_len(dst_len) }
			C.WideCharToMultiByte(dst_codepage, 0, unicode.data, unicode.len, dst.data,
				dst.len, 0, 0)
		}
	}
	return dst
}
