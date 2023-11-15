module pem

import encoding.base64
import arrays

// encode encodes the given block into a
// string using the EncodeConfig. It returns an error if `block_type` is undefined
// or if a value in `headers` contains an invalid character ':'
//
// default EncodeConfig values wrap lines at 64 bytes and use '\n' for newlines
pub fn (block Block) encode(config EncodeConfig) !string {
	if block.block_type == '' {
		return error('crypto.pem: `encode` called with undefined `block_type`')
	}
	if block.headers.keys().any(it.contains(':')) || block.headers.values().any(it.contains(':')) {
		return error('crypto.pem: invalid header character `:`')
	}

	// to avoid repeated struct access
	newline := config.line_ending
	length := config.line_length

	mut inner := ''
	if block.headers.len > 0 {
		// Proc-Type must be written first if it is present
		if block.headers['Proc-Type'].len > 0 {
			inner += 'Proc-Type: 4,${block.headers['Proc-Type'][0].trim_string_left('4,')}' +
				newline
		}

		for key, value in block.headers {
			if key == 'Proc-Type' {
				continue
			}

			for _, subvalue in value {
				inner += '${key}: '
				if key.len + subvalue.len < length {
					inner += subvalue
				} else {
					inner += newline + wrap_lines(subvalue, newline, length)
				}
				inner += newline
			}
		}

		inner += newline
	}

	inner += wrap_lines(base64.encode(block.data), newline, length)

	return '${pem_begin}${block.block_type}${pem_eol}${newline}' + '${inner}' +
		'${pem_end}${block.block_type}${pem_eol}'
}

@[inline]
fn wrap_lines(str string, newline string, length int) string {
	return arrays.chunk(str.bytes(), length).map(it.bytestr()).join(newline)
}
