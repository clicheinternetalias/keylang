#

CC=gcc -g

CFLAGS=-Wall -std=c11 -Wextra -Wformat -Wshadow -Wconversion \
	-Wredundant-decls -Wpointer-arith -Wcast-align -Werror \
	-pedantic -pedantic-errors -Wformat=2 -Og \
	-Wstrict-overflow=5 -fstrict-aliasing \
	-Wtrampolines -Wlarger-than=65535 -Wstack-usage=1024 \
	-Wunsafe-loop-optimizations -Wcast-qual -Wcast-align -Wconversion \
	-Wlogical-op -Wmissing-prototypes \
	-Wredundant-decls -Wdisabled-optimization -Wstack-protector

all: key

key: key.c key.h
	$(CC) $(CFLAGS) -DTEST -o key key.c

clean:
	rm -f key
