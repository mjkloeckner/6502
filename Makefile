CC := gcc
CLIBS :=
CFLAGS := -Wall -Wshadow -pedantic -ansi -std=c99 -O3
# SRCS := $(wildcard *.c)
SRCS := $(shell find ./ ! -name "shift.c" -name "*.c")
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
