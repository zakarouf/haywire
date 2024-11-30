all: haywire

haywire:
	gcc -fsanitize=address -DHW_DEBUG_CODE_ENABLE -O0 -Wall -Wextra -pedantic -std=c99 ./src/*.c -o hw
