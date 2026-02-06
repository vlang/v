#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <openssl/ec.h>
#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/sha.h>
#include <openssl/rand.h>
#include <sys/time.h>

#define ITERATIONS 1000
#define MESSAGE "Benchmark message"

// Function to calculate the difference in microseconds between two timevals
long time_diff_microseconds(struct timeval start, struct timeval end) {
    long seconds = end.tv_sec - start.tv_sec;
    long useconds = end.tv_usec - start.tv_usec;
    return seconds * 1000000 + useconds;
}

int main() {
    int iterations = ITERATIONS;
    struct timeval start, end;
    long total_gen_time = 0;
    long total_sign_time = 0;
    long total_verify_time = 0;
    long avg_gen_time, avg_sign_time, avg_verify_time;

    // Benchmarking key generation
    printf("Benchmarking key generation...\n");
    for(int i = 0; i < iterations; i++) {
        gettimeofday(&start, NULL);

        // Create a new EC_KEY object with the P-256 curve
        EC_KEY *key = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
        if (key == NULL) {
            fprintf(stderr, "Error creating EC_KEY object.\n");
            exit(EXIT_FAILURE);
        }

        // Generate the key
        if (EC_KEY_generate_key(key) != 1) {
            fprintf(stderr, "Error generating EC key.\n");
            EC_KEY_free(key);
            exit(EXIT_FAILURE);
        }

        gettimeofday(&end, NULL);
        total_gen_time += time_diff_microseconds(start, end);

        // Free the key
        EC_KEY_free(key);
    }
    avg_gen_time = total_gen_time / iterations;
    printf("Average key generation time: %ld µs\n", avg_gen_time);

    // Generate a single key for signing and verification
    EC_KEY *priv_key = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
    if (priv_key == NULL) {
        fprintf(stderr, "Error creating EC_KEY object.\n");
        exit(EXIT_FAILURE);
    }
    if (EC_KEY_generate_key(priv_key) != 1) {
        fprintf(stderr, "Error generating EC key.\n");
        EC_KEY_free(priv_key);
        exit(EXIT_FAILURE);
    }

    // Prepare the message hash
    unsigned char hash[SHA256_DIGEST_LENGTH];
    SHA256((unsigned char*)MESSAGE, strlen(MESSAGE), hash);

    // Benchmarking signing
    printf("Benchmarking signing...\n");
    for(int i = 0; i < iterations; i++) {
        gettimeofday(&start, NULL);

        ECDSA_SIG *signature = ECDSA_do_sign(hash, SHA256_DIGEST_LENGTH, priv_key);
        if (signature == NULL) {
            fprintf(stderr, "Error signing message.\n");
            EC_KEY_free(priv_key);
            exit(EXIT_FAILURE);
        }

        gettimeofday(&end, NULL);
        total_sign_time += time_diff_microseconds(start, end);

        // Free the signature
        ECDSA_SIG_free(signature);
    }
    avg_sign_time = total_sign_time / iterations;
    printf("Average sign time: %ld µs\n", avg_sign_time);

    // Create a signature for verification benchmarking
    ECDSA_SIG *signature = ECDSA_do_sign(hash, SHA256_DIGEST_LENGTH, priv_key);
    if (signature == NULL) {
        fprintf(stderr, "Error signing message for verification.\n");
        EC_KEY_free(priv_key);
        exit(EXIT_FAILURE);
    }

    // Benchmarking verification
    printf("Benchmarking verification...\n");
    for(int i = 0; i < iterations; i++) {
        gettimeofday(&start, NULL);

        int verify_status = ECDSA_do_verify(hash, SHA256_DIGEST_LENGTH, signature, priv_key);

        gettimeofday(&end, NULL);
        total_verify_time += time_diff_microseconds(start, end);

        if (verify_status != 1) {
            fprintf(stderr, "Signature verification failed.\n");
            ECDSA_SIG_free(signature);
            EC_KEY_free(priv_key);
            exit(EXIT_FAILURE);
        }
    }
    avg_verify_time = total_verify_time / iterations;
    printf("Average verify time: %ld µs\n", avg_verify_time);

    // Cleanup
    ECDSA_SIG_free(signature);
    EC_KEY_free(priv_key);

    return 0;
}

