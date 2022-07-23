#include "6502.h"

#include <stdio.h>

/* included for debugging purposes */
#include <string.h>

static CPU cpu;
static MEM mem; /* 8 bit word */

static uint8_t cyc;
static uint8_t opc;

static uint16_t  tmp;
static uint16_t  addr_abs;
static uint16_t  addr_rel;

INS decode[0x100];
static INS prev_ins;

/* Cpu methods */
void CPU_init(void) {
	cpu.ac = cpu.x = cpu.y = cpu.sp = cpu.st = 0;
	cyc = opc = tmp = 0;
	prev_ins = (INS) {"???", &NUL, &IMP, 7};
}

void CPU_reset(void) {
	cpu.sp = 0xFF;
	cpu.st = 0x00;

	/* little endian */
	uint16_t lo = MEM_read(0xFFFC);
	uint16_t hi = MEM_read(0xFFFD);

	cpu.pc = (hi << 8) | lo;

	addr_abs = addr_rel = opc = 0x0000;

	/* simulate 8 cycles needed to reset the cpu */
	cyc = 8;
}

void CPU_fetch(INS *ins) {
	*ins = decode[opc = MEM_read(cpu.pc)];
}

uint8_t CPU_exec(INS ins) {
	/* simulate instructions cycles */
    do {
        if (cyc == 0) {
			if (!strcmp(ins.name, "???")) {
				printf("EXEC: invalid instruction reached\n");
				return 1;
			} else if (!strcmp(ins.name, "BNE") && !(strcmp(prev_ins.name, "BNE"))) {
				printf("EXEC: consecutive BNE reached\n");
				return 1;
			}

			cpu.pc++;
			cyc = ins.cycles;

			uint8_t add_cyc_0 = ins.mode();
			uint8_t add_cyc_1 = ins.op();

			prev_ins = ins;
			cyc += add_cyc_0 & add_cyc_1;
        }
    } while(--cyc);

	return 0;
}

/* interrupt request */
void CPU_irq(void) {
	if(!CPU_get_flag(I)) {
		/* save pc */
		MEM_write(0x100 + cpu.sp--, (cpu.pc >> 8) & 0x00FF);
		MEM_write(0x100 + cpu.sp--, cpu.pc & 0x00FF);

		/* save st */
		MEM_write(0x100 + cpu.sp--, cpu.st | (1 << 4) | (1 << 5));

		CPU_set_flag(I, 1);

		uint16_t lo = MEM_read(0xFFFE);
		uint16_t hi = MEM_read(0xFFFF);

		cpu.pc = ((hi << 8) & 0xFF00) | (lo & 0x00FF);

		cyc = 8;
	}
}

/* non maskeable interrupt request */
void CPU_nm_irq(void) {
	/* save program counter to stack */
	MEM_write(0x100 + cpu.sp--, (cpu.pc >> 8) & 0x00FF);
	MEM_write(0x100 + cpu.sp--, cpu.pc & 0x00FF);

	/* save cpu status to stack */
	MEM_write(0x100 + cpu.sp--, cpu.st | (1 << 4) | (1 << 5));
	CPU_set_flag(I, 1);

	addr_abs = 0xFFFA;
	uint16_t lo = MEM_read(addr_abs + 0);
	uint16_t hi = MEM_read(addr_abs + 1);

	cpu.pc = (hi << 8) | lo;

	cyc = 8;
}

void CPU_branch(void) {
	cyc++;
	addr_abs = cpu.pc + addr_rel;

	/* if address changes page */
	if((addr_abs & 0xFF00) != (cpu.pc & 0xFF00))
		cyc++;

	cpu.pc = addr_abs;
}

void CPU_set_flag(ST_FLAG flag, uint8_t val) {
	cpu.st = val ? cpu.st | (1 << flag) : cpu.st & ~(1 << flag);
}

uint8_t CPU_get_flag(ST_FLAG flag) {
	return (cpu.st & (1 << flag)) >> flag;
}

void print_reg(uint8_t reg) {
	for(unsigned long i = ((sizeof(reg) * 8)); i > 0; i--) {
		putchar((reg & (0x01 << (i - 1))) ? '1' : '0');
		putchar(' ');
	}
	putchar('\n');
}

void CPU_dump(void) {
	INS aux = decode[MEM_read(cpu.pc)];

	printf("    a:   %02X (%3d)            N V - B D I Z C\n", cpu.ac, cpu.ac);
	printf("    x:   %02X (%3d)            ", cpu.x, cpu.x);
	print_reg(cpu.st);
	printf("    y:   %02X (%3d)\n", cpu.y, cpu.y);
	printf("   sp:   %02X\n", cpu.sp);
	printf("   pc: %04X -> %02X (%s)(%s)\n", cpu.pc, mem.ram[cpu.pc], aux.name, CPU_mode_name(aux.mode));
	printf("--------------------------------------------\n");
}

uint16_t CPU_get_pc(void) {
	return cpu.pc;
}

/* Memory methods */
void MEM_init(void) {
	for(uint16_t i = 0; i < 0xFFFF; i++)
		mem.ram[i] = 0;
}

void MEM_set_pc_start(uint16_t addr) {
	mem.ram[0xFFFC] = (addr & 0x00FF);
	mem.ram[0xFFFD] = ((addr & 0xFF00) >> 8);
}

void MEM_load_from_file(char *path) {
	FILE *fp;

	if(path == NULL) return;

	fp = fopen(path, "rb");
	if(fp == NULL) return;

	fread(mem.ram, 1, sizeof(mem.ram), fp);
	/* fread(mem.ram + 0x8000, 1, 0x7FFF - 0x6, fp); */

	fclose(fp);
}

uint8_t MEM_read(uint16_t addr) {
	return mem.ram[addr];
}

uint8_t MEM_write(uint16_t addr, uint8_t val) {
	mem.ram[addr] = val;
	return val;
}

void MEM_dump() {
	for(uint16_t i = 0; i < 0xF; i++)
		printf("%02X ", mem.ram[i]);

	putchar('\n');
}

void MEM_dump_page(uint16_t page) {
	uint16_t j = 1;

	printf("Page %04X:\n", page);
	for(uint16_t i = page; i <= (page + 0xFF); i++, j++)
		printf("%02X %s", mem.ram[i], (j % 0x1E) ? "" : "\n");

	putchar('\n');
}

void MEM_dump_last_six(void) {
	printf("Last six:\n");
	for(size_t i = 0xFFFA; i <= 0xFFFF; i++)
		printf("%02X ", mem.ram[i]);

	putchar('\n');
}

char *CPU_mode_name(uint8_t (*mode)(void)) {
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

/* Addressing modes */
uint8_t ABS(void) {
	uint16_t lo = MEM_read(cpu.pc++);
	uint16_t hi = MEM_read(cpu.pc++);

	addr_abs = (hi << 8) | lo;

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}

uint8_t ABX(void) {
	uint16_t lo = MEM_read(cpu.pc++);
	uint16_t hi = MEM_read(cpu.pc++);

	addr_abs = (hi << 8) | lo;

	printf("address fetched: %04X\nx register: %02X\nend address: %04X (%02X)\n", addr_abs, cpu.x, addr_abs + cpu.x, MEM_read(addr_abs + cpu.x));

	addr_abs += cpu.x;
	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));

	return ((addr_abs & 0x00FF) != (hi << 8)) ? 1 : 0;
}

uint8_t ABY(void) {
	uint16_t lo = MEM_read(cpu.pc++);
	uint16_t hi = MEM_read(cpu.pc++);

	addr_abs = (hi << 8) | lo;

	printf("address fetched: %04X\ny register: %02X\nend address: %04X (%02X)\n", addr_abs, cpu.y, addr_abs + cpu.y, MEM_read(addr_abs + cpu.y));

	addr_abs += cpu.y;
	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return ((addr_abs & 0x00FF) != (hi << 8)) ? 1 : 0;
}

uint8_t IMM(void) {
	addr_abs = cpu.pc++;
	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}

uint8_t IMP(void) {
	tmp = cpu.ac;
	return 0;
}

uint8_t IND(void) {
	/* get a direction from memory */
	uint16_t lo = MEM_read(cpu.pc++);
	uint16_t hi = MEM_read(cpu.pc++);

	/* assign the direction to ptr */
	uint16_t p = (hi << 8) | lo;

	/* get the address pointed by p
	 * and simulate page boundary hardware bug */
	addr_abs = (MEM_read((lo == 0xFF) ? (p & 0xFF00) : p + 1) << 8) | MEM_read(p);

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}

uint8_t IZX(void) {
	uint8_t addr = MEM_read(cpu.pc++);

	/* get a direction from memory */
	uint16_t lo = MEM_read((addr + cpu.x) & 0xFF);
	uint16_t hi = MEM_read((addr + cpu.x + 1) & 0xFF);

	addr_abs = ((hi << 8) & 0xFF00) | lo;

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));

	return 0;
}

uint8_t IZY(void) {
	uint8_t addr = MEM_read(cpu.pc++);

	/* get a direction from memory */
	uint16_t lo = MEM_read(addr);
	uint16_t hi = MEM_read(addr + 1);

	addr_abs = ((hi << 8) & 0xFF00) | lo;
	addr_abs += cpu.y;

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));

	return ((addr_abs & 0xFF00) != (hi << 8)) ? 1 : 0;
}

uint8_t REL(void) {
	addr_rel = (MEM_read(cpu.pc++) & 0x00FF);
	printf("REL: offset: %02X (%d)\n", addr_rel, (int8_t)addr_rel);

	/* signed byte */
    if (addr_rel & (1 << 7))
		addr_rel |= 0xFF00;

	return 0;
}

/* read only the offset of the zero page */
uint8_t ZP0(void) {
	addr_abs = (MEM_read(cpu.pc++) & 0x00FF);

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}

uint8_t ZPX(void) {
	addr_abs = (MEM_read(cpu.pc++) + cpu.x) & 0x00FF;

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}

uint8_t ZPY(void) {
	addr_abs = (MEM_read(cpu.pc++) + cpu.y) & 0x00FF;

	printf("Address: %04X -> (%02X)\n", addr_abs, MEM_read(addr_abs));
	return 0;
}


/* Instructions */
uint8_t ADC(void) {
	uint16_t data = MEM_read(addr_abs);
	uint16_t res = (cpu.ac + data + (uint16_t)CPU_get_flag(C));

	CPU_set_flag(N, res & (1 << 7));
	CPU_set_flag(Z, (res & 0x00FF) == 0);
	CPU_set_flag(C, ((res & 0xFF00) >> 7));
	CPU_set_flag(V, ((cpu.ac^res) & ~(cpu.ac^data)) & (1 << 7));

	cpu.ac = (uint8_t)(res & 0x00FF);

	return 1;
}

uint8_t AND(void) {
	cpu.ac &= MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 1;
}

uint8_t ASL(void) {
	if(decode[opc].mode == &IMP) {
		tmp = cpu.ac << 1;
		CPU_set_flag(C, (cpu.ac & (1 << 7)));
		cpu.ac = (tmp & 0x00FF);
	}
	else {
		tmp = MEM_read(addr_abs) << 1;
		CPU_set_flag(C, MEM_read(addr_abs) & (1 << 7));
		MEM_write(addr_abs, tmp & 0x00FF);
	}

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t BCC(void) {
	if(!CPU_get_flag(C))
		CPU_branch();

	return 0;
}

uint8_t BCS(void) {
	if(CPU_get_flag(C))
		CPU_branch();

	return 0;
}

uint8_t BEQ(void) {
	if(CPU_get_flag(Z))
		CPU_branch();

	return 0;
}

uint8_t BIT(void) {
	tmp = (MEM_read(addr_abs) & 0x00FF);

	CPU_set_flag(Z, (cpu.ac & tmp) == 0);
	CPU_set_flag(V, tmp & (1 << 6));
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t BMI(void) {
	if(CPU_get_flag(N))
		CPU_branch();

	return 0;
}

uint8_t BNE(void) {
	if(!CPU_get_flag(Z))
		CPU_branch();

	return 0;
}

uint8_t BPL(void) {
	if(!CPU_get_flag(N))
		CPU_branch();

	return 0;
}

uint8_t BRK(void) {
	/* save program counter to stack */
	MEM_write(0x100 + cpu.sp--, (cpu.pc >> 8) & 0x00FF);
	MEM_write(0x100 + cpu.sp--, cpu.pc & 0x00FF);

	/* save cpu status to stack */
	MEM_write(0x100 + cpu.sp--, cpu.st | (1 << 4) | (1 << 5));
	CPU_set_flag(I, 1);

	uint16_t lo = MEM_read(0xFFFE);
	uint16_t hi = MEM_read(0xFFFF);

	cpu.pc = (hi << 8) | lo;

	return 0;
}

uint8_t BVC(void) {
	if(!CPU_get_flag(V))
		CPU_branch();

	return 0;
}

uint8_t BVS(void) {
	if(CPU_get_flag(V))
		CPU_branch();

	return 0;
}

uint8_t CLC(void) {
	CPU_set_flag(C, 0);
	return 0;
}

uint8_t CLD(void) {
	CPU_set_flag(D, 0);
	return 0;
}

uint8_t CLI(void) {
	CPU_set_flag(I, 0);
	return 0;
}

uint8_t CLV(void) {
	CPU_set_flag(V, 0);
	return 0;
}

uint8_t CMP(void) {
	tmp = (MEM_read(addr_abs) & 0x00FF);

	printf("cmp: %d, acc: %d\n", tmp, cpu.ac);

	CPU_set_flag(C, cpu.ac >= tmp);
	CPU_set_flag(Z, cpu.ac == tmp);
	CPU_set_flag(N, (cpu.ac - tmp) & (1<<7));

	return 0;
}

uint8_t CPX(void) {
	tmp = (MEM_read(addr_abs) & 0x00FF);

	CPU_set_flag(C, cpu.x >= tmp);
	CPU_set_flag(Z, cpu.x == tmp);
	CPU_set_flag(N, (cpu.x - tmp) & (1<<7));

	return 0;
}

uint8_t CPY(void) {
	tmp = (MEM_read(addr_abs) & 0x00FF);

	CPU_set_flag(C, cpu.y >= tmp);
	CPU_set_flag(Z, cpu.y == tmp);
	CPU_set_flag(N, (cpu.y - tmp) & (1<<7));

	return 0;
}

uint8_t DEC(void) {
	tmp = MEM_write(addr_abs, MEM_read(addr_abs) - 1) & 0x00FF;

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

/* Decrements X register */
uint8_t DEX(void) {
	cpu.x -= 1;

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

/* Decrements Y register */
uint8_t DEY(void) {
	cpu.y -= 1;

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 0;
}

uint8_t EOR(void) {
	cpu.ac ^= MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 1;
}

uint8_t INC(void) {
	tmp = MEM_write(addr_abs, MEM_read(addr_abs) + 1) & 0x00FF;

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t INX(void) {
	cpu.x += 1;

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

uint8_t INY(void) {
	cpu.y += 1;

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 0;
}

uint8_t JMP(void) {
	cpu.pc = addr_abs;
	return 0;
}

uint8_t JSR(void) {
	cpu.pc--;

	MEM_write(0x100 + cpu.sp--, (cpu.pc & 0xFF00) >> 8);
	MEM_write(0x100 + cpu.sp--, (cpu.pc & 0x00FF));

	cpu.pc = addr_abs;

	return 0;
}

uint8_t LDA(void) {
	cpu.ac = MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 1;
}

uint8_t LDX(void) {
	cpu.x = MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 1;
}

uint8_t LDY(void) {
	cpu.y = MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 1;
}

uint8_t LSR(void) {
	if(decode[opc].mode == &IMP) {
		tmp = (cpu.ac >> 1);
		CPU_set_flag(C, cpu.ac & 0x01);
		cpu.ac = (tmp & 0x00FF);
	}
	else {
		tmp = (MEM_read(addr_abs) >> 1);
		CPU_set_flag(C, MEM_read(addr_abs) & 0x01);
		MEM_write(addr_abs, tmp & 0x00FF);
	}

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t NOP(void) {
	return 0;
}

uint8_t ORA(void) {
	cpu.ac |= MEM_read(addr_abs);

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 1;
}

uint8_t PHA(void) {
	MEM_write(0x100 + cpu.sp--, cpu.ac);

	return 0;
}

uint8_t PHP(void) {
	MEM_write(0x100 + cpu.sp--, cpu.st | (1 << 4) | (1 << 5));

	return 0;
}

uint8_t PLA(void) {
	cpu.ac = MEM_read(0x100 + ++cpu.sp);

	CPU_set_flag(Z, (cpu.ac == 0));
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 0;
}

uint8_t PLP(void) {
	uint8_t B_is_set = CPU_get_flag(B);
	uint8_t U_is_set = CPU_get_flag(U);

	/* restore status */
	cpu.st = MEM_read(0x100 + ++cpu.sp);

	/* This two flags stay the same as before */
	CPU_set_flag(B, B_is_set);
	CPU_set_flag(U, U_is_set);

	return 0;
}

uint8_t ROL(void) {
	if(decode[opc].mode == &IMP) {
		tmp = (cpu.ac << 1) & 0xFE;
		tmp |= CPU_get_flag(C);

		CPU_set_flag(C, cpu.ac & (1 << 7));
		cpu.ac = (tmp & 0x00FF);
	}
	else {
		tmp = (MEM_read(addr_abs) << 1) & 0xFE;
		tmp |= CPU_get_flag(C);

		CPU_set_flag(C, MEM_read(addr_abs) & (1 << 7));
		MEM_write(addr_abs, tmp & 0x00FF);
	}

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t ROR(void) {
	if(decode[opc].mode == &IMP) {
		tmp = (cpu.ac >> 1) & 0x7F;
		tmp |= (CPU_get_flag(C) << 7);

		CPU_set_flag(C, cpu.ac & 0x01);

		cpu.ac = (tmp & 0x00FF);
	}
	else {
		tmp = (MEM_read(addr_abs) >> 1) & 0x7F;
		tmp |= (CPU_get_flag(C) << 7);

		CPU_set_flag(C, MEM_read(addr_abs) & 1);
		MEM_write(addr_abs, tmp & 0x00FF);
	}

	CPU_set_flag(Z, tmp == 0);
	CPU_set_flag(N, tmp & (1 << 7));

	return 0;
}

uint8_t RTI(void) {
	uint8_t B_is_set = CPU_get_flag(B);
	uint8_t U_is_set = CPU_get_flag(U);

	/* restore status */
	cpu.st = MEM_read(0x100 + ++cpu.sp);

	/* This two flags stay the same as before */
	CPU_set_flag(B, B_is_set);
	CPU_set_flag(U, U_is_set);

	/* restore pc */
	cpu.pc = MEM_read(0x100 + ++cpu.sp);
	cpu.pc |= (MEM_read(0x100 + ++cpu.sp) << 8);

	return 0;
}

uint8_t RTS(void) {
	cpu.pc = MEM_read(0x100 + ++cpu.sp);
	cpu.pc |= (MEM_read(0x100 + ++cpu.sp) << 8);

	CPU_set_flag(B, 0);

	cpu.pc++;

	return 0;
}

uint8_t SBC(void) {
	uint16_t data = MEM_read(addr_abs) ^ 0xFF;
	uint16_t res = (cpu.ac + data + (uint16_t)CPU_get_flag(C));

	CPU_set_flag(N, res & (1 << 7));
	CPU_set_flag(Z, (res & 0x00FF) == 0);
	CPU_set_flag(C, ((res & 0xFF00) >> 7));
	CPU_set_flag(V, ((cpu.ac^res) & ~(cpu.ac^data)) & (1 << 7));

	cpu.ac = (uint8_t)(res & 0x00FF);

	return 1;
}

uint8_t SEC(void) {
	CPU_set_flag(C, 1);
	return 0;
}

uint8_t SED(void) {
	CPU_set_flag(D, 1);
	return 0;
}

uint8_t SEI(void) {
	CPU_set_flag(I, 1);
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
	MEM_write(addr_abs, cpu.y);
	return 0;
}

uint8_t TAX(void) {
	cpu.x = cpu.ac;

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

uint8_t TAY(void) {
	cpu.y = cpu.ac;

	CPU_set_flag(Z, cpu.y == 0);
	CPU_set_flag(N, cpu.y & (1 << 7));

	return 0;
}

uint8_t TSX(void) {
	cpu.x = cpu.sp;

	CPU_set_flag(Z, cpu.x == 0);
	CPU_set_flag(N, cpu.x & (1 << 7));

	return 0;
}

uint8_t TXA(void) {
	cpu.ac = cpu.x;

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 0;
}

uint8_t TXS(void) {
	cpu.sp = cpu.x;

	return 0;
}

uint8_t TYA(void) {
	cpu.ac = cpu.y;

	CPU_set_flag(Z, cpu.ac == 0);
	CPU_set_flag(N, cpu.ac & (1 << 7));

	return 0;
}

uint8_t NUL(void) {
	return 0;
}

/* Lookup table */
INS decode[0x100] = {
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

