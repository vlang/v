// Standard CRC64-ECMA reference implementation in C
// Compiles with: gcc -std=c99 crc64_ref.c -o crc64_ref

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define CRC64_ECMA 0x42F0E1EBA9EA3693ULL

static uint64_t crc64_table[256];

void crc64_init_table(void) {
	for (int i = 0; i < 256; i++) {
		uint64_t crc = (uint64_t)i << 56;
		for (int j = 0; j < 8; j++) {
			if (crc & 0x8000000000000000ULL) {
				crc = (crc << 1) ^ CRC64_ECMA;
			} else {
				crc <<= 1;
			}
		}
		crc64_table[i] = crc;
	}
}

uint64_t crc64_checksum(const uint8_t *data, size_t len) {
	uint64_t crc = 0ULL;
	for (size_t i = 0; i < len; i++) {
		uint8_t byte = data[i];
		crc = crc64_table[(uint8_t)((crc >> 56) ^ byte)] ^ (crc << 8);
	}
	return crc;
}

int main(int argc, char *argv[]) {
	crc64_init_table();

	if (argc < 2) {
		fprintf(stderr, "Usage: %s <action> [data...]\n", argv[0]);
		fprintf(stderr, "  checksum <hexstring>  - compute CRC64 of hex data\n");
		fprintf(stderr, "  table                 - print first 16 table entries\n");
		return 1;
	}

	const char *action = argv[1];

	if (strcmp(action, "table") == 0) {
		printf("CRC64_ECMA table (first 16):\n");
		for (int i = 0; i < 16; i++) {
			printf("  [%d] = 0x%016llx\n", i, (unsigned long long)crc64_table[i]);
		}
		return 0;
	}

	if (strcmp(action, "checksum") == 0) {
		const char *hexstr = (argc > 2) ? argv[2] : "";
		size_t hexlen = strlen(hexstr);

		if (hexlen == 0) {
			// Empty input
			uint64_t crc = crc64_checksum(NULL, 0);
			printf("%016llx\n", (unsigned long long)crc);
			return 0;
		}

		if (hexlen % 2 != 0) {
			fprintf(stderr, "Error: hex string must have even length\n");
			return 1;
		}

		size_t datalen = hexlen / 2;
		uint8_t *data = malloc(datalen);
		if (!data) {
			fprintf(stderr, "Error: memory allocation failed\n");
			return 1;
		}

		for (size_t i = 0; i < datalen; i++) {
			unsigned int byte;
			if (sscanf(&hexstr[i * 2], "%2x", &byte) != 1) {
				fprintf(stderr, "Error: invalid hex character\n");
				free(data);
				return 1;
			}
			data[i] = (uint8_t)byte;
		}

		uint64_t crc = crc64_checksum(data, datalen);
		printf("%016llx\n", (unsigned long long)crc);
		free(data);
		return 0;
	}

	fprintf(stderr, "Error: unknown action '%s'\n", action);
	return 1;
}

