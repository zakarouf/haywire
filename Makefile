all: haywire

haywire:
	gcc -O0 -Wall -Wextra -pedantic -std=c99 ./src/*.c -o hw
