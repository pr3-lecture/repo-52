#include <stdio.h>
#include <string.h>
#include "crypto.h"

#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; \
                                if (message) return message; } while (0)

int tests_run = 0;

static char* encryptTest() {
	char* input = "HALLO";
    KEY key;
    key.type = 1;
    key.chars = "ASDFG";
    char output[strlen(input)];
    encrypt(key, input, output);
    mu_assert("Test: Verschlüsselung", strcmp("IRHJH", output) == 0);
    return 0;
}

static char* encryptTestFailInput() {
	char* input = "HALLO1";
    KEY key;
    key.type = 1;
    key.chars = "ASDFG";
    char output[strlen(input)];
    int result = encrypt(key, input, output);
    mu_assert("Test: Verschlüsselung - ungültiges Zeichen", result == E_MESSAGE_ILLEGAL_CHAR);
    return 0;
}

static char* decryptTest() {
	char* cypherText = "IRHJH";
    KEY key;
    key.type = 1;
    key.chars = "ASDFG";

    char output[strlen(cypherText)];
    decrypt(key, cypherText, output);
    mu_assert("Test: Entschlüsselung", strcmp("HALLO", output) == 0);
    return 0;
}

static char* decryptTestFailInput() {
	char* cypherText = "IRHJH1";
    KEY key;
    key.type = 1;
    key.chars = "ASDFG";
    char output[strlen(cypherText)];
    int result = decrypt(key, cypherText, output);
    mu_assert("Test: Entschlüsselung - ungültiges Zeichen", result == E_CYPHER_ILLEGAL_CHAR);
    return 0;
}

static char* allTests() {
	mu_run_test(encryptTest);
    mu_run_test(encryptTestFailInput);

    mu_run_test(decryptTest);
    mu_run_test(decryptTestFailInput);

    return 0;
}

int main() {
	char *result = allTests();

	if (result != 0) printf("%s\n", result);
	else             printf("ALL TESTS PASSED\n");

	printf("Tests run: %d\n", tests_run);

	return result != 0;
}