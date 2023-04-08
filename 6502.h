#ifndef _6502_H
#define _6502_H

#include <stdint.h>

/* Instruction */
typedef struct {
	char *name;
	uint8_t (*op)(void);
	uint8_t (*mode)(void);
	uint8_t cycles;
} INS;

typedef struct {
	uint8_t ac, x, y, sp, st;
	uint16_t pc;
} CPU;

/* zero-page: fast shorter instructions
 * first-page: stack;
 * last-six bytes: used to store special addresses */
typedef struct {
	uint8_t ram[0x10000]; /* 64kb memory */
} MEM;

/* 6502 status register flags */
typedef enum {
	C,    /* Carry */
	Z,    /* Zero  */
	I,    /* Disable interrupts  */
	D,    /* Decimal mode  */
	B,    /* Break */
	U,    /* Unused */
	V,    /* Overflow */
	N     /* Negative */
} ST_FLAG;

char *CPU_mode_name(uint8_t (*mode)(void));
uint16_t CPU_get_pc(void);
uint8_t CPU_exec(INS ins);
uint8_t CPU_get_flag(ST_FLAG flag);
void CPU_branch(void);
void CPU_dump(void);
void CPU_fetch(INS *ins);
void CPU_init(void);
void CPU_irq(void);
void CPU_nm_irq(void);
void CPU_reset(void);
void CPU_set_flag(ST_FLAG flag, uint8_t val);

void MEM_init(void);
void MEM_set_pc_start(uint16_t addr);
uint8_t MEM_read(uint16_t addr);
uint8_t MEM_write(uint16_t addr, uint8_t val); /* returns written value */
void MEM_dump(void);
void MEM_dump_page(uint16_t page);
void MEM_dump_last_six(void);
int MEM_load_from_file(char *fp);


void print_reg(uint8_t);


/*  Addresing modes  */
uint8_t ABS(void);
uint8_t ABX(void);
uint8_t ABY(void);
uint8_t IMM(void);
uint8_t IMP(void);
uint8_t IND(void);
uint8_t IZX(void);
uint8_t IZY(void);
uint8_t REL(void);
uint8_t ZP0(void);
uint8_t ZPX(void);
uint8_t ZPY(void);


/* Opcodes */
uint8_t ADC(void);
uint8_t AND(void);
uint8_t ASL(void);
uint8_t BCC(void);
uint8_t BCS(void);
uint8_t BEQ(void);
uint8_t BIT(void);
uint8_t BMI(void);
uint8_t BNE(void);
uint8_t BPL(void);
uint8_t BRK(void);
uint8_t BVC(void);
uint8_t BVS(void);
uint8_t CLC(void);
uint8_t CLD(void);
uint8_t CLI(void);
uint8_t CLV(void);
uint8_t CMP(void);
uint8_t CPX(void);
uint8_t CPY(void);
uint8_t DEC(void);
uint8_t DEX(void);
uint8_t DEY(void);
uint8_t EOR(void);
uint8_t INC(void);
uint8_t INX(void);
uint8_t INY(void);
uint8_t JMP(void);
uint8_t JSR(void);
uint8_t LDA(void);
uint8_t LDX(void);
uint8_t LDY(void);
uint8_t LSR(void);
uint8_t NOP(void);
uint8_t ORA(void);
uint8_t PHA(void);
uint8_t PHP(void);
uint8_t PLA(void);
uint8_t PLP(void);
uint8_t ROL(void);
uint8_t ROR(void);
uint8_t RTI(void);
uint8_t RTS(void);
uint8_t SBC(void);
uint8_t SEC(void);
uint8_t SED(void);
uint8_t SEI(void);
uint8_t STA(void);
uint8_t STX(void);
uint8_t STY(void);
uint8_t TAX(void);
uint8_t TAY(void);
uint8_t TSX(void);
uint8_t TXA(void);
uint8_t TXS(void);
uint8_t TYA(void);

uint8_t NUL(void);


#endif
