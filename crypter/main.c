#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "crypto.h"

int main(int argc, char* argv[]) {
    KEY* myKey = malloc(sizeof(myKey));
    char* cypher = argv[1];

    // check number of args
    if (argc < 2) {
        fprintf(stderr, "Zu wenig Argumente\n");
        free(myKey);
        return E_KEY_TOO_SHORT;

    } else {
        // check the key
        while(*argv[1] != '\0') {
            // dirty ASCII
            if(*argv[1] < 'A' || *argv[1] > 'Z') {
                fprintf(stderr, "Ungültiger Schlüssel\n");
                free(myKey);
                return E_KEY_ILLEGAL_CHAR;
            }
            argv[1]++;
        }

        // cypherText
        printf("Der Schlüssel lautet: %s\n",cypher);

        // check for input file
        if(argc == 3) {
            char c;
            FILE *fp = fopen(argv[2], "r");
            if(fp != NULL) {
                while((c = fgetc(fp)) != EOF) {
                    // TODO - fread
                }
            } else {
                fprintf(stderr, "Datei nicht lesbar\n");
                return E_MESSAGE_ILLEGAL_CHAR;
            }

        }

        // read from stdin
        char* input = malloc(sizeof(char)*255);
        
        if(argc == 2) {    
            printf("Bitte geben Sie ihr Klartextpasswort bzw. Cypher ein:\n");
            fgets(input, 255, stdin);
        }

        // resize cypher if needed
        char* finalCypher;

        if(strlen(cypher) < strlen(input)) {
            int diff = strlen(input) - strlen(cypher);
            char strTemp[diff-1];
            finalCypher = cypher;
            strncpy(strTemp, cypher, diff-1);
            strcat(finalCypher, strTemp);
        } else {
            finalCypher = cypher;
        }

        // setup output memory
        char* output = malloc(sizeof(char)*255);

        // setup the Key
        myKey->chars = finalCypher;
        myKey->type = 1;

        if(strstr(argv[0], "encrypt") != NULL) {
            encrypt(*myKey, input, output);
        }

        if(strstr(argv[0], "decrypt") != NULL) {
            decrypt(*myKey, input, output);
        }

        // print result
        printf("Das Ergebnis lautet: %s\n", output);

        free(myKey);
        free(input);
        free(output);
        return 0;
    }
}