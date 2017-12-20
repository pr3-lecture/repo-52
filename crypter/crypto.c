#include <stdio.h>
#include <string.h>
#include "crypto.h"

int checkIllegalChars(const char* input, int encrypt) {
    char* chars;
    if(encrypt == 1) {
        chars = MESSAGE_CHARACTERS;
    } else {
        chars = CYPHER_CHARACTERS;
    }

    for(int i = 0; i < strlen(input); i++) {
        int isCorrectChar = 0;
        for(int j = 0; j < strlen(chars); j++) {
            if(input[i] == chars[j]) {
                isCorrectChar = 1;
                break;
            }
        }
        if (isCorrectChar == 0) {
            return 1;
        }
    }
    return 0;
}

int getIndex(char c, const char* strg) {
    for (int i = 0; i < *strg; i++) {
        if (strg[i] == c) {
            return i;
        }
    }
    return -1;
}

int encrypt(KEY key, const char* input, char* output) {
    if(checkIllegalChars(input, 1) != 0) {
        fprintf(stderr, "Ungültige Nachricht\n");
        return E_MESSAGE_ILLEGAL_CHAR;
    }

    for(int i = 0; i < strlen(input); i++) {
        int indexInput = getIndex(input[i], MESSAGE_CHARACTERS) + 1;
        int indexKey = getIndex(key.chars[i % strlen(key.chars)], KEY_CHARACTERS) + 1;
        output[i] = CYPHER_CHARACTERS[indexInput ^ indexKey];
    }
    output[strlen(input)] = '\0';
    return 0;
}


int decrypt(KEY key, const char* cypherText, char* output) {
    if(checkIllegalChars(cypherText, 0) != 0) {
        fprintf(stderr, "Ungültige Nachricht\n");
        return E_CYPHER_ILLEGAL_CHAR;
    }

    for (int i = 0; i < strlen(cypherText); i++) {
        int indexCypher = getIndex(cypherText[i], CYPHER_CHARACTERS);
        int indexKey = getIndex(key.chars[i % strlen(key.chars)], KEY_CHARACTERS) + 1;

        output[i] = MESSAGE_CHARACTERS[indexCypher ^ indexKey] - 1;
    }
    output[strlen(cypherText)] = '\0';
    return 0;
}