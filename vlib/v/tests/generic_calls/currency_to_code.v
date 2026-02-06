module main

pub fn (currency Currency) to_code() string {
	return match currency {
		.afghani { 'AFN' }
		.argentina_peso { 'ARS' }
		.aruba_guilder { 'AWG' }
	}
}
