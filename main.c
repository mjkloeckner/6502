#include "6502.h"

#include <stdio.h>
#include <stdbool.h>

#define INPUT_FILE_PATH	"example.bin"

int main (void) {
	INS ins;

	MEM_init();
	MEM_load_from_file(INPUT_FILE_PATH);

	CPU_init();
	CPU_reset();

	do {
		CPU_dump();
		CPU_fetch(&ins);
		CPU_exec(ins);

		printf("ins:  %s (%s)(%d)\n", ins.name, mode_to_str(ins.mode), ins.cycles);

		CPU_clock();

		MEM_dump();
	} while(getchar() != 'q');

	return 0;
}
