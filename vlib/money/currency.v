module money

// Currency describes a monetary currency using its ISO 4217 code and the
// number of minor units (decimal places) it uses for storage and display.
pub struct Currency {
pub:
	code     string // ISO 4217 code, e.g. "USD"
	decimals int    // number of minor units, e.g. 2 for USD
	symbol   string // display symbol, e.g. "$"
	name     string // human readable name, e.g. "US Dollar"
}

// Common currencies. Add more as needed; custom currencies can be created
// directly with the `Currency` struct.
pub const usd = Currency{ code: 'USD', decimals: 2, symbol: '$', name: 'US Dollar' }
pub const eur = Currency{ code: 'EUR', decimals: 2, symbol: '€', name: 'Euro' }
pub const gbp = Currency{ code: 'GBP', decimals: 2, symbol: '£', name: 'Pound Sterling' }
pub const jpy = Currency{ code: 'JPY', decimals: 0, symbol: '¥', name: 'Japanese Yen' }
pub const chf = Currency{ code: 'CHF', decimals: 2, symbol: 'CHF', name: 'Swiss Franc' }
pub const cad = Currency{ code: 'CAD', decimals: 2, symbol: 'C$', name: 'Canadian Dollar' }
pub const aud = Currency{ code: 'AUD', decimals: 2, symbol: 'A$', name: 'Australian Dollar' }
pub const nzd = Currency{ code: 'NZD', decimals: 2, symbol: 'NZ$', name: 'New Zealand Dollar' }
pub const cny = Currency{ code: 'CNY', decimals: 2, symbol: '¥', name: 'Chinese Yuan' }
pub const inr = Currency{ code: 'INR', decimals: 2, symbol: '₹', name: 'Indian Rupee' }
pub const brl = Currency{ code: 'BRL', decimals: 2, symbol: 'R$', name: 'Brazilian Real' }
pub const sek = Currency{ code: 'SEK', decimals: 2, symbol: 'kr', name: 'Swedish Krona' }
pub const nok = Currency{ code: 'NOK', decimals: 2, symbol: 'kr', name: 'Norwegian Krone' }
pub const dkk = Currency{ code: 'DKK', decimals: 2, symbol: 'kr', name: 'Danish Krone' }
pub const pln = Currency{ code: 'PLN', decimals: 2, symbol: 'zł', name: 'Polish Zloty' }
pub const rub = Currency{ code: 'RUB', decimals: 2, symbol: '₽', name: 'Russian Ruble' }
pub const try_ = Currency{ code: 'TRY', decimals: 2, symbol: '₺', name: 'Turkish Lira' }
pub const zar = Currency{ code: 'ZAR', decimals: 2, symbol: 'R', name: 'South African Rand' }
pub const mxn = Currency{ code: 'MXN', decimals: 2, symbol: '$', name: 'Mexican Peso' }
pub const sar = Currency{ code: 'SAR', decimals: 2, symbol: '﷼', name: 'Saudi Riyal' }
pub const aed = Currency{ code: 'AED', decimals: 2, symbol: 'د.إ', name: 'UAE Dirham' }
pub const thb = Currency{ code: 'THB', decimals: 2, symbol: '฿', name: 'Thai Baht' }
pub const hkd = Currency{ code: 'HKD', decimals: 2, symbol: 'HK$', name: 'Hong Kong Dollar' }
pub const sgd = Currency{ code: 'SGD', decimals: 2, symbol: 'S$', name: 'Singapore Dollar' }
pub const krw = Currency{ code: 'KRW', decimals: 0, symbol: '₩', name: 'South Korean Won' }
pub const isk = Currency{ code: 'ISK', decimals: 0, symbol: 'kr', name: 'Icelandic Krona' }
pub const bhd = Currency{ code: 'BHD', decimals: 3, symbol: '.د.ب', name: 'Bahraini Dinar' }
pub const kwd = Currency{ code: 'KWD', decimals: 3, symbol: 'د.ك', name: 'Kuwaiti Dinar' }
pub const omr = Currency{ code: 'OMR', decimals: 3, symbol: '﷼', name: 'Omani Rial' }
pub const jod = Currency{ code: 'JOD', decimals: 3, symbol: 'د.ا', name: 'Jordanian Dinar' }

// from_code looks up a currency by its ISO 4217 code.
pub fn currency_from_code(code string) !Currency {
	match code.to_upper() {
		'USD' { return usd }
		'EUR' { return eur }
		'GBP' { return gbp }
		'JPY' { return jpy }
		'CHF' { return chf }
		'CAD' { return cad }
		'AUD' { return aud }
		'NZD' { return nzd }
		'CNY' { return cny }
		'INR' { return inr }
		'BRL' { return brl }
		'SEK' { return sek }
		'NOK' { return nok }
		'DKK' { return dkk }
		'PLN' { return pln }
		'RUB' { return rub }
		'TRY' { return try_ }
		'ZAR' { return zar }
		'MXN' { return mxn }
		'SAR' { return sar }
		'AED' { return aed }
		'THB' { return thb }
		'HKD' { return hkd }
		'SGD' { return sgd }
		'KRW' { return krw }
		'ISK' { return isk }
		'BHD' { return bhd }
		'KWD' { return kwd }
		'OMR' { return omr }
		'JOD' { return jod }
		else { return error('money: unknown currency code `${code}`') }
	}
}
