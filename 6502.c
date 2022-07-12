#include "6502.h"

#include <stdio.h>
#include <string.h>

static CPU cpu;
static MEM mem; /* 8 bit word */

static uint8_t cyc;
static uint8_t opc;
static uint8_t fch;

static uint16_t  addr_abs;
static uint16_t  addr_rel;

INS lookup[0x100];

static uint8_t ST_FLAG_MASK[] = { 
	(1 << 0),    /* Carry */
	(1 << 1),    /* Zero  */
	(1 << 2),    /* Disable interrupts  */
	(1 << 3),    /* Decimal mode  */
	(1 << 4),    /* Break */
	(1 << 5),    /* Unused */
	(1 << 6),    /* Overflow */
	(1 << 7)     /* Negative */
};


/* Cpu methods */
void CPU_init(void) {
	cpu.ac = cpu.x = cpu.y = cpu.sp = cpu.st = 0;
}

void CPU_reset(void) {
	cpu.sp = 0xFF;	 /* first page */
	cpu.st = 0x00 | ST_FLAG_MASK[U];

	/* little endian */
	uint16_t lo = MEM_fetch(0xFFFC);
	uint16_t hi = MEM_fetch(0xFFFD);

	cpu.pc = (hi << 8) | lo;

	addr_abs = addr_rel = opc = 0x0000;

	cyc = 8;
}

void CPU_clock(void) {
}

void CPU_fetch(INS *ins) {
	*ins = lookup[MEM_fetch(cpu.pc++)];
}

void CPU_exec(const INS ins) {
	addr_abs = 0x0000;
	ins.mode();
	ins.op();
}

void CPU_irq(void) {
}

void CPU_nm_irq(void) {
}

void CPU_set_flag(ST_FLAG flag, uint8_t val) {
	cpu.st = val ? cpu.st | ST_FLAG_MASK[flag] : cpu.st & ~(ST_FLAG_MASK[flag]);
}

uint8_t CPU_get_flag(ST_FLAG flag) {
	return ((cpu.st & ST_FLAG_MASK[flag]) >> flag);
}

void print_reg(uint8_t reg) {
	for(unsigned long i = ((sizeof(reg) * 8)); i > 0; i--) {
		putchar((reg & (0x01 << (i - 1))) ? '1' : '0');
		putchar(' ');
	}

	putchar('\n');
}

void CPU_dump(void) {
	printf("N O - B D I Z C\n");
	print_reg(cpu.st);
	printf("        a:   %02X\n", cpu.ac);
	printf("        x:   %02X\n", cpu.x);
	printf("        y:   %02X\n", cpu.y);
	printf("       sp:   %02X\n", cpu.sp);
	printf("       pc: %04X\n", cpu.pc);
	printf("ram[%04X]:   %02X\n", cpu.pc, mem.ram[cpu.pc]);
}

/* Memory methods */
void MEM_init(void) {
	for(uint16_t i = 0; i < 0xFFFF; i++)
		mem.ram[i] = 0;

	mem.ram[0xFFFC] = 0x00;
	mem.ram[0xFFFD] = 0x80;
}

void MEM_load_from_file(char *path) {
	FILE *fp;

	if(path == NULL) return;

	fp = fopen(path, "rb");
	if(fp == NULL) return;

	fread(mem.ram + 0x8000, 1, 0x7FFF - 0x6, fp);

	fclose(fp);
}

uint8_t MEM_fetch(uint16_t addr) {
	return mem.ram[addr];
}

uint8_t MEM_write(uint16_t addr, uint8_t val) {
	mem.ram[addr] = val;
	return 0;
}

void MEM_dump(void) {
	for(uint16_t i = 0; i < 0xF; i++)
		printf("%02X ", mem.ram[i]);

	putchar('\n');
}

char *mode_to_str(uint8_t (*mode)(void)) {
	if(mode == &IMM) return "IMM";
	else if(mode == &ABS) return "ABS";
	else if(mode == &ABX) return "ABX";
	else if(mode == &ABY) return "ABY";
	else if(mode == &IMP) return "IMP";
	else if(mode == &IND) return "IND";
	else if(mode == &IZX) return "IZX";
	else if(mode == &IZY) return "IZY";
	else if(mode == &REL) return "REL";
	else if(mode == &ZP0) return "ZP0";
	else if(mode == &ZPX) return "ZPX";
	else if(mode == &ZPY) return "ZPY";

	return "NUL";
}

/* Lookup table */
INS lookup[0x100] = {
    {"BRK", &BRK, &IMM, 7}, {"ORA", &ORA, &IZX, 6}, {"???", &NUL, &IMP, 2},
    {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 3}, {"ORA", &ORA, &ZP0, 3},
    {"ASL", &ASL, &ZP0, 5}, {"???", &NUL, &IMP, 5}, {"PHP", &PHP, &IMP, 3},
    {"ORA", &ORA, &IMM, 2}, {"ASL", &ASL, &IMP, 2}, {"???", &NUL, &IMP, 2},
    {"???", &NOP, &IMP, 4}, {"ORA", &ORA, &ABS, 4}, {"ASL", &ASL, &ABS, 6},
    {"???", &NUL, &IMP, 6}, {"BPL", &BPL, &REL, 2}, {"ORA", &ORA, &IZY, 5},
    {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"ORA", &ORA, &ZPX, 4}, {"ASL", &ASL, &ZPX, 6}, {"???", &NUL, &IMP, 6},
    {"CLC", &CLC, &IMP, 2}, {"ORA", &ORA, &ABY, 4}, {"???", &NOP, &IMP, 2},
    {"???", &NUL, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"ORA", &ORA, &ABX, 4},
    {"ASL", &ASL, &ABX, 7}, {"???", &NUL, &IMP, 7}, {"JSR", &JSR, &ABS, 6},
    {"AND", &AND, &IZX, 6}, {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8},
    {"BIT", &BIT, &ZP0, 3}, {"AND", &AND, &ZP0, 3}, {"ROL", &ROL, &ZP0, 5},
    {"???", &NUL, &IMP, 5}, {"PLP", &PLP, &IMP, 4}, {"AND", &AND, &IMM, 2},
    {"ROL", &ROL, &IMP, 2}, {"???", &NUL, &IMP, 2}, {"BIT", &BIT, &ABS, 4},
    {"AND", &AND, &ABS, 4}, {"ROL", &ROL, &ABS, 6}, {"???", &NUL, &IMP, 6},
    {"BMI", &BMI, &REL, 2}, {"AND", &AND, &IZY, 5}, {"???", &NUL, &IMP, 2},
    {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 4}, {"AND", &AND, &ZPX, 4},
    {"ROL", &ROL, &ZPX, 6}, {"???", &NUL, &IMP, 6}, {"SEC", &SEC, &IMP, 2},
    {"AND", &AND, &ABY, 4}, {"???", &NOP, &IMP, 2}, {"???", &NUL, &IMP, 7},
    {"???", &NOP, &IMP, 4}, {"AND", &AND, &ABX, 4}, {"ROL", &ROL, &ABX, 7},
    {"???", &NUL, &IMP, 7}, {"RTI", &RTI, &IMP, 6}, {"EOR", &EOR, &IZX, 6},
    {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 3},
    {"EOR", &EOR, &ZP0, 3}, {"LSR", &LSR, &ZP0, 5}, {"???", &NUL, &IMP, 5},
    {"PHA", &PHA, &IMP, 3}, {"EOR", &EOR, &IMM, 2}, {"LSR", &LSR, &IMP, 2},
    {"???", &NUL, &IMP, 2}, {"JMP", &JMP, &ABS, 3}, {"EOR", &EOR, &ABS, 4},
    {"LSR", &LSR, &ABS, 6}, {"???", &NUL, &IMP, 6}, {"BVC", &BVC, &REL, 2},
    {"EOR", &EOR, &IZY, 5}, {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8},
    {"???", &NOP, &IMP, 4}, {"EOR", &EOR, &ZPX, 4}, {"LSR", &LSR, &ZPX, 6},
    {"???", &NUL, &IMP, 6}, {"CLI", &CLI, &IMP, 2}, {"EOR", &EOR, &ABY, 4},
    {"???", &NOP, &IMP, 2}, {"???", &NUL, &IMP, 7}, {"???", &NOP, &IMP, 4},
    {"EOR", &EOR, &ABX, 4}, {"LSR", &LSR, &ABX, 7}, {"???", &NUL, &IMP, 7},
    {"RTS", &RTS, &IMP, 6}, {"ADC", &ADC, &IZX, 6}, {"???", &NUL, &IMP, 2},
    {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 3}, {"ADC", &ADC, &ZP0, 3},
    {"ROR", &ROR, &ZP0, 5}, {"???", &NUL, &IMP, 5}, {"PLA", &PLA, &IMP, 4},
    {"ADC", &ADC, &IMM, 2}, {"ROR", &ROR, &IMP, 2}, {"???", &NUL, &IMP, 2},
    {"JMP", &JMP, &IND, 5}, {"ADC", &ADC, &ABS, 4}, {"ROR", &ROR, &ABS, 6},
    {"???", &NUL, &IMP, 6}, {"BVS", &BVS, &REL, 2}, {"ADC", &ADC, &IZY, 5},
    {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"ADC", &ADC, &ZPX, 4}, {"ROR", &ROR, &ZPX, 6}, {"???", &NUL, &IMP, 6},
    {"SEI", &SEI, &IMP, 2}, {"ADC", &ADC, &ABY, 4}, {"???", &NOP, &IMP, 2},
    {"???", &NUL, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"ADC", &ADC, &ABX, 4},
    {"ROR", &ROR, &ABX, 7}, {"???", &NUL, &IMP, 7}, {"???", &NOP, &IMP, 2},
    {"STA", &STA, &IZX, 6}, {"???", &NOP, &IMP, 2}, {"???", &NUL, &IMP, 6},
    {"STY", &STY, &ZP0, 3}, {"STA", &STA, &ZP0, 3}, {"STX", &STX, &ZP0, 3},
    {"???", &NUL, &IMP, 3}, {"DEY", &DEY, &IMP, 2}, {"???", &NOP, &IMP, 2},
    {"TXA", &TXA, &IMP, 2}, {"???", &NUL, &IMP, 2}, {"STY", &STY, &ABS, 4},
    {"STA", &STA, &ABS, 4}, {"STX", &STX, &ABS, 4}, {"???", &NUL, &IMP, 4},
    {"BCC", &BCC, &REL, 2}, {"STA", &STA, &IZY, 6}, {"???", &NUL, &IMP, 2},
    {"???", &NUL, &IMP, 6}, {"STY", &STY, &ZPX, 4}, {"STA", &STA, &ZPX, 4},
    {"STX", &STX, &ZPY, 4}, {"???", &NUL, &IMP, 4}, {"TYA", &TYA, &IMP, 2},
    {"STA", &STA, &ABY, 5}, {"TXS", &TXS, &IMP, 2}, {"???", &NUL, &IMP, 5},
    {"???", &NOP, &IMP, 5}, {"STA", &STA, &ABX, 5}, {"???", &NUL, &IMP, 5},
    {"???", &NUL, &IMP, 5}, {"LDY", &LDY, &IMM, 2}, {"LDA", &LDA, &IZX, 6},
    {"LDX", &LDX, &IMM, 2}, {"???", &NUL, &IMP, 6}, {"LDY", &LDY, &ZP0, 3},
    {"LDA", &LDA, &ZP0, 3}, {"LDX", &LDX, &ZP0, 3}, {"???", &NUL, &IMP, 3},
    {"TAY", &TAY, &IMP, 2}, {"LDA", &LDA, &IMM, 2}, {"TAX", &TAX, &IMP, 2},
    {"???", &NUL, &IMP, 2}, {"LDY", &LDY, &ABS, 4}, {"LDA", &LDA, &ABS, 4},
    {"LDX", &LDX, &ABS, 4}, {"???", &NUL, &IMP, 4}, {"BCS", &BCS, &REL, 2},
    {"LDA", &LDA, &IZY, 5}, {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 5},
    {"LDY", &LDY, &ZPX, 4}, {"LDA", &LDA, &ZPX, 4}, {"LDX", &LDX, &ZPY, 4},
    {"???", &NUL, &IMP, 4}, {"CLV", &CLV, &IMP, 2}, {"LDA", &LDA, &ABY, 4},
    {"TSX", &TSX, &IMP, 2}, {"???", &NUL, &IMP, 4}, {"LDY", &LDY, &ABX, 4},
    {"LDA", &LDA, &ABX, 4}, {"LDX", &LDX, &ABY, 4}, {"???", &NUL, &IMP, 4},
    {"CPY", &CPY, &IMM, 2}, {"CMP", &CMP, &IZX, 6}, {"???", &NOP, &IMP, 2},
    {"???", &NUL, &IMP, 8}, {"CPY", &CPY, &ZP0, 3}, {"CMP", &CMP, &ZP0, 3},
    {"DEC", &DEC, &ZP0, 5}, {"???", &NUL, &IMP, 5}, {"INY", &INY, &IMP, 2},
    {"CMP", &CMP, &IMM, 2}, {"DEX", &DEX, &IMP, 2}, {"???", &NUL, &IMP, 2},
    {"CPY", &CPY, &ABS, 4}, {"CMP", &CMP, &ABS, 4}, {"DEC", &DEC, &ABS, 6},
    {"???", &NUL, &IMP, 6}, {"BNE", &BNE, &REL, 2}, {"CMP", &CMP, &IZY, 5},
    {"???", &NUL, &IMP, 2}, {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 4},
    {"CMP", &CMP, &ZPX, 4}, {"DEC", &DEC, &ZPX, 6}, {"???", &NUL, &IMP, 6},
    {"CLD", &CLD, &IMP, 2}, {"CMP", &CMP, &ABY, 4}, {"NOP", &NOP, &IMP, 2},
    {"???", &NUL, &IMP, 7}, {"???", &NOP, &IMP, 4}, {"CMP", &CMP, &ABX, 4},
    {"DEC", &DEC, &ABX, 7}, {"???", &NUL, &IMP, 7}, {"CPX", &CPX, &IMM, 2},
    {"SBC", &SBC, &IZX, 6}, {"???", &NOP, &IMP, 2}, {"???", &NUL, &IMP, 8},
    {"CPX", &CPX, &ZP0, 3}, {"SBC", &SBC, &ZP0, 3}, {"INC", &INC, &ZP0, 5},
    {"???", &NUL, &IMP, 5}, {"INX", &INX, &IMP, 2}, {"SBC", &SBC, &IMM, 2},
    {"NOP", &NOP, &IMP, 2}, {"???", &SBC, &IMP, 2}, {"CPX", &CPX, &ABS, 4},
    {"SBC", &SBC, &ABS, 4}, {"INC", &INC, &ABS, 6}, {"???", &NUL, &IMP, 6},
    {"BEQ", &BEQ, &REL, 2}, {"SBC", &SBC, &IZY, 5}, {"???", &NUL, &IMP, 2},
    {"???", &NUL, &IMP, 8}, {"???", &NOP, &IMP, 4}, {"SBC", &SBC, &ZPX, 4},
    {"INC", &INC, &ZPX, 6}, {"???", &NUL, &IMP, 6}, {"SED", &SED, &IMP, 2},
    {"SBC", &SBC, &ABY, 4}, {"NOP", &NOP, &IMP, 2}, {"???", &NUL, &IMP, 7},
    {"???", &NOP, &IMP, 4}, {"SBC", &SBC, &ABX, 4}, {"INC", &INC, &ABX, 7},
    {"???", &NUL, &IMP, 7}
};


/* Addressing modes */
uint8_t ABS(void) {
	uint8_t lo = MEM_fetch(cpu.pc++);
	uint8_t hi = MEM_fetch(cpu.pc++);

	addr_abs = (hi << 8) | lo;
	return 0;
}

uint8_t ABX(void) {
	return 0;
}

uint8_t ABY(void) {
	return 0;
}

uint8_t IMM(void) {
	addr_abs = cpu.pc++;
	return 0;
}

uint8_t IMP(void) {
	fch = cpu.ac;
	return 0;
}

uint8_t IND(void) {
	return 0;
}

uint8_t IZX(void) {
	return 0;
}

uint8_t IZY(void) {
	return 0;
}

uint8_t REL(void) {
	addr_rel = MEM_fetch(cpu.pc++);

    if(addr_rel & (1 << 7))
		addr_rel |= 0xFF00;

	return 0;
}

uint8_t ZP0(void) {
	return 0;
}

uint8_t ZPX(void) {
	return 0;
}

uint8_t ZPY(void) {
	return 0;
}


/* Instructions */
uint8_t ADC(void) {
	uint8_t data = MEM_fetch(addr_abs);

	cpu.ac += (data + CPU_get_flag(C));

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));
	CPU_set_flag(O, ((cpu.ac^data) & ~(cpu.ac^data)) & (1 << 7));

	return 1;
}

uint8_t AND(void) {
	return 0;
}

uint8_t ASL(void) {
	
	return 0;
}

uint8_t BCC(void) {
	return 0;
}

uint8_t BCS(void) {
	return 0;
}

uint8_t BEQ(void) {
	return 0;
}

uint8_t BIT(void) {
	return 0;
}

uint8_t BMI(void) {
	return 0;
}

uint8_t BNE(void) {
	if(!CPU_get_flag(Z))
		cpu.pc += addr_rel;

	return 0;
}

uint8_t BPL(void) {
	return 0;
}

uint8_t BRK(void) {
	return 0;
}

uint8_t BVC(void) {
	return 0;
}

uint8_t BVS(void) {
	return 0;
}

uint8_t CLC(void) {
	CPU_set_flag(C, 0);
	return 0;
}

uint8_t CLD(void) {
	return 0;
}

uint8_t CLI(void) {
	return 0;
}

uint8_t CLV(void) {
	return 0;
}

uint8_t CMP(void) {
	return 0;
}

uint8_t CPX(void) {
	return 0;
}

uint8_t CPY(void) {
	return 0;
}

uint8_t DEC(void) {
	return 0;
}

uint8_t DEX(void) {
	cpu.x -= 1;

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

/* Subtracts one from the Y register */
uint8_t DEY(void) {
	cpu.y -= 1;

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 0;
}

uint8_t EOR(void) {
	return 0;
}

uint8_t INC(void) {
	return 0;
}

uint8_t INX(void) {
	return 0;
}

uint8_t INY(void) {
	return 0;
}

uint8_t JMP(void) {
	return 0;
}

uint8_t JSR(void) {
	return 0;
}

uint8_t LDA(void) {
	cpu.ac = MEM_fetch(addr_abs);

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 0;
}

uint8_t LDX(void) {
	cpu.x = MEM_fetch(addr_abs);

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

uint8_t LDY(void) {
	cpu.y = MEM_fetch(addr_abs);

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 0;
}

uint8_t LSR(void) {
	return 0;
}

uint8_t NOP(void) {
	return 0;
}

uint8_t ORA(void) {
	return 0;
}

uint8_t PHA(void) {
	return 0;
}

uint8_t PHP(void) {
	return 0;
}

uint8_t PLA(void) {
	return 0;
}

uint8_t PLP(void) {
	return 0;
}

uint8_t ROL(void) {
	return 0;
}

uint8_t ROR(void) {
	return 0;
}

uint8_t RTI(void) {
	return 0;
}

uint8_t RTS(void) {
	return 0;
}

uint8_t SBC(void) {
	return 0;
}

uint8_t SEC(void) {
	return 0;
}

uint8_t SED(void) {
	return 0;
}

uint8_t SEI(void) {
	return 0;
}

uint8_t STA(void) {
	MEM_write(addr_abs, cpu.ac);
	return 0;
}

uint8_t STX(void) {
	MEM_write(addr_abs, cpu.x);
	return 0;
}

uint8_t STY(void) {
	return 0;
}

uint8_t TAX(void) {
	return 0;
}

uint8_t TAY(void) {
	return 0;
}

uint8_t TSX(void) {
	return 0;
}

uint8_t TXA(void) {
	return 0;
}

uint8_t TXS(void) {
	return 0;
}

uint8_t TYA(void) {
	return 0;
}


uint8_t NUL(void) {
	return 0;
}
