all: haywire

haywire:
	gcc -ansi -Wno-unused-parameter -Wfatal-errors -g -Wall -Wextra -pedantic -std=c99\
		-DHW_DEBUG_CODE_ENABLE -O0\
		./src/*.c -o hw
