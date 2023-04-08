#include "6502.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define INPUT_FILE_PATH	"6502_functional_test.bin"

/* TODO: Add support for command line arguments */
/* TODO: Add proper system monitor (memory dump, cpu registers) */

static bool brk = false;

void CPU_brk(uint16_t pc) {
	if(CPU_get_pc() == pc) {
		printf("BRK: %2X reached\n", pc);
		brk = true;
	}
}

int main (void) {
	INS ins;

	/* Initialize memory to 0 */
	MEM_init();

	/* Load program to memory */
	if(MEM_load_from_file(INPUT_FILE_PATH))
		return 1;

	/* set the first address that the pc should be set to */
	MEM_set_pc_start(0x0400);

	/* Initialize cpu registers to 0 */
	CPU_init();

	/* initialize registers from memmory */
	CPU_reset();

	int c = 0;
	do {
		if(c == 'c') brk = false;
		CPU_brk(0x336d);

		/* Fetch an instruction and increment pc */
		CPU_fetch(&ins);

		CPU_dump();
		/* MEM_dump_last_six(); */
		/* MEM_dump_page(0x0000); */
		/* MEM_dump_page(0x0100); */

		/* Execute instruction */
		if(CPU_exec(ins) != 0) brk = true;
	} while(brk ? ((c = getchar()) != 'q') : true);

	return 0;
}
