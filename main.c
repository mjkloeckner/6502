#include "6502.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

/* #define INPUT_FILE_PATH	"10times3.bin" */
#define INPUT_FILE_PATH	"6502_functional_test.bin"

/* TODO: Finish with CPU instructions implementation */
/* TODO: add support for command line arguments */
/* TODO: add proper system monitor (memory dump, cpu registers) */

static bool brk = false;

void CPU_brk(uint16_t pc) {
	if(CPU_get_pc() == pc) {
		brk = true;
	}
}


int main (void) {
	INS ins;

	/* Initialize memory to 0 */
	MEM_init();

	/* set the first address that the pc should be set to */
	MEM_set_pc_start(0x0400);

	/* Load program to memory */
	MEM_load_from_file(INPUT_FILE_PATH);

	/* Initialize cpu registers to 0 */
	CPU_init();

	CPU_reset();

	do {
		/* Fetch an instruction and increment pc */
		CPU_fetch(&ins);

		CPU_dump();
		MEM_dump_last_six();
		/* MEM_dump_page(0x0000); */
		/* MEM_dump_page(0x0100); */

		/* Execute instruction */
		if(CPU_exec(ins) != 0) return 1;

		CPU_brk(0x22D8);
		printf("--------------------------------------------\n");
	} while(brk ? (getchar() != 'q') : true);

	return 0;
}
