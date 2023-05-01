CC := gcc
CFLAGS := -D_POSIX_C_SOURCE=199309L -Wall -Wshadow -pedantic -ansi -std=c99 -O3
SRCS := $(wildcard *.c)
OBJS := $(SRCS:.c=.o)

TARGET := 6502

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CLIBS) $(CFLAGS) -o $@ $^
	rm -f $(OBJS)

%.o: %.c
	$(CC) $(CLIBS) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS)
