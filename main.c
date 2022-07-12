#include "6502.h"

#include <stdio.h>
#include <stdbool.h>

#define INPUT_FILE_PATH	"10times3.bin"

/* TODO: Finish with CPU instructions implementation */
/* TODO: add support for command line arguments */
/* TODO: add proper system monitor (memory dump, cpu registers) */

int main (void) {
	INS ins;

	/* Initialize memory to 0 */
	MEM_init();

	/* Load program to memory */
	MEM_load_from_file(INPUT_FILE_PATH);

	/* Initialize registers to 0 */
	CPU_init();
	CPU_reset();

	do {
		/* Fetch an instruction and increment pc */
		CPU_fetch(&ins);

		CPU_dump();

		putchar('\n');

		MEM_dump_page(0x0000);
		MEM_dump_page(0x8000);

		/* Execute instruction */
		CPU_exec(ins);

	} while(getchar() != 'q');

	return 0;
}
