#include "tui.h"

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>


/* variable to remember original terminal attributes. */
static struct termios default_attributes;

void reset_input_mode (void) {
    tcsetattr(STDIN_FILENO, TCSANOW, &default_attributes);

    /* restore cursor */
    printf("\033[?25h");
}

void set_input_mode (void) {
    struct termios tattr;

    /* Make sure stdin is a terminal. */
    if (!isatty(STDIN_FILENO)) {
        fprintf (stderr, "Not a terminal.\n");
        exit (EXIT_FAILURE);
    }

    /* Save the terminal attributes so we can restore them later. */
    tcgetattr(STDIN_FILENO, &default_attributes);
    atexit(reset_input_mode);

    /* hide cursor */
    printf("\033[?25l");

    /* Set the terminal modes. */
    tcgetattr(STDIN_FILENO, &tattr);
    tattr.c_lflag &= ~(ICANON|ECHO); /* Clear ICANON and ECHO. */
    tattr.c_cc[VMIN] = 1;
    tattr.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &tattr);
}
