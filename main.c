#include "6502.h"
#include "tui.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <signal.h>

#include <unistd.h>

#include <time.h>
#define nsleep(t) nanosleep((const struct timespec[]){{0, t * 1000L}}, NULL)

#define INPUT_FILE_PATH	"6502_functional_test.bin"

/* TODO: Add support for command line arguments */
/* TODO: Add proper system monitor (memory dump, cpu registers) */

static bool brk = false;

void CPU_brk(uint16_t pc) {
	if (CPU_get_pc () == pc) {
		printf("BRK: %2X reached\n", pc);
		brk = true;
	}
}

void sig_handler(int signo) {
	if (signo == SIGINT) brk = true;
	if (signo == SIGQUIT) brk = true;
}

int main(void) {
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

	set_input_mode();
	signal(SIGINT, sig_handler);
	signal(SIGQUIT, sig_handler);
	atexit(reset_input_mode);

	do {
		/* set break point at pc 0x336d */
		CPU_brk(0x336d);

		CPU_fetch(&ins);
		CPU_dump();

		if(CPU_exec(ins) != 0) {
			/* exit if invalid instruction */
			char c;
			read(STDIN_FILENO, &c, 1);
			brk = true;
		}

		nsleep(1000);
	} while(!brk);

	reset_input_mode();
	return 0;
}
