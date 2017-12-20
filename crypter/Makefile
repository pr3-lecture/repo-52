.PHONY: all clean
all: build
clean:
	rm -f crypter decrypt encrypt crypto.o main.o
runtest:

crypto.o: crypto.c
	gcc -Wall -c crypto.c
main.o: main.c
	gcc -Wall -c main.c
build: main.o crypto.o
	gcc -o crypter main.o crypto.o