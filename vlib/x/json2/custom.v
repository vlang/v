module json2

// implements encoding json, this is not validated so implementations must be correct
pub interface JsonEncoder {
	// to_json returns a string containing an objects json representation
	to_json() string
}

// Encodable is an interface, that allows custom implementations for encoding structs to their string based JSON representations.

@[deprecated: 'use `to_json` to implement `JsonEncoder` instead']
@[deprecated_after: '2025-10-30']
pub interface Encodable {
	json_str() string
}
