#include <stdio.h>
#include <string.h>
#include "crypto.h"

int encrypt(KEY key, const char* input, char* output) {
    printf("INPUT: %s",input);
    printf("KEY: %s",key.chars);
    
    while(*input != '\0') {
        // dirty ASCII
        if(*input < 'A' || *input > 'Z') {
            fprintf(stderr, "Ungültige Nachricht\n");
            return E_MESSAGE_ILLEGAL_CHAR;
        }
        input++;
    }


    strcpy(output, "Encrypting output");
    return 0;
}

int decrypt(KEY key, const char* cypherText, char* output) {
    
    
    
    
    strcpy(output, "Decrypting output");
    return 0;
}