#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include "crypto.h"

int main(int argc, char* argv[]) {
    KEY* myKey = malloc(sizeof(myKey));
    char* key = argv[1];

    // check number of args
    if (argc < 2) {
        fprintf(stderr, "Zu wenig Argumente\n");
        free(myKey);
        return E_KEY_TOO_SHORT;

    } else {
        // check the key
        while(*argv[1] != '\0') {
            if(*argv[1] < 'A' || *argv[1] > 'Z') {
                fprintf(stderr, "Ungültiger Schlüssel\n");
                free(myKey);
                return E_KEY_ILLEGAL_CHAR;
            }
            argv[1]++;
        }

        // input buffer
        char input[255];

        // check for input file
        if(argc == 3) {
            FILE* fp = fopen(argv[2], "r");
            if(fp != NULL) {
                fgets(input, sizeof(input), fp);
                fclose(fp);
            } else {
                fprintf(stderr, "Datei nicht lesbar\n");
                fclose(fp);
                return E_MESSAGE_ILLEGAL_CHAR;
            }
        }
        
        if(argc == 2) {    
            printf("Bitte geben Sie ihr Klartextpasswort bzw. Cipher ein:\n");
            scanf("%s", input);
        }

        // trim the input string
        input[strcspn(input, "\r\n")] = 0;
        printf("Eingabe: %s\n", input);
        
        // setup output memory
        char output[sizeof(char) * strlen(input)];


        // setup the Key
        myKey->chars = key;
        myKey->type = 1;

        if(strstr(argv[0], "encrypt") != NULL) {
            if(encrypt(*myKey, input, output) == 0) {
                printf("Das Ergebnis lautet: %s\n", output);
            }
        }

        if(strstr(argv[0], "decrypt") != NULL) {
            if(decrypt(*myKey, input, output) == 0) {
                printf("Das Ergebnis lautet: %s\n", output);
            }
        }
        
        free(myKey);
        return 0;
    }
}