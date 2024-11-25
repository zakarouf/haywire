all: haywire

haywire:
	gcc -Wall -Wextra -pedantic -std=c99 ./src/*.c -o hw
