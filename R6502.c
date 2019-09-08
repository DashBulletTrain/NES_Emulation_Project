#include "R6502.h"

#define _C_ (1<<0) // CARRY BIT
#define _Z_ (1<<1) // ZERO BIT
#define _I_ (1<<2) // DISABLE INTERUPTS
#define _D_ (1<<3) // DECIMAL MODE (unused for this Implementation)
#define _B_ (1<<4) // BREAK
#define _U_ (1<<5) // UNUSED
#define _V_ (1<<6) // OVERFLOW
#define _N_ (1<<7) // NEGATIVE

struct INSTRUCTION
{
  uint8_t(*operate)();
  uint8_t(*addrmode)();
  uint8_t cycles;
};

// Forward Dec
const struct INSTRUCTION* R6502_GetInstructionFromOpCode(uint8_t opCode);

void R6502_Write(uint16_t addr, uint8_t data)
{

}

uint8_t R6502_Read(uint16_t addr)
{
  return 0x00;
}

uint8_t R6502_GetFlag(struct R6502* cpu, uint8_t flag)
{
  return (cpu->status & flag);
}

void R6502_SetFlag(struct R6502* cpu, uint8_t flag, bool value)
{
  if (value)
  {
    cpu->status |= flag;
  }
  else
  {
    cpu->status &= ~flag;
  }
}

/////////////////////////////////////
// ADDRESSING MODES
/////////////////////////////////////
#pragma region ADDRESSING MODES
uint8_t IMP(struct R6502* cpu)
{
  cpu->fetched = cpu->a;
  return 0;
}
uint8_t IMM(struct R6502* cpu)
{
  cpu->addr_abs = cpu->pc++;
  return 0;
}
uint8_t ZP0(struct R6502* cpu)
{
  cpu->addr_abs = R6502_Read(cpu->pc);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t ZPX(struct R6502* cpu)
{
  cpu->addr_abs = (R6502_Read(cpu->pc) + cpu->x);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t ZPY(struct R6502* cpu)
{
  cpu->addr_abs = (R6502_Read(cpu->pc) + cpu->y);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t REL(struct R6502* cpu)
{
  cpu->addr_rel = R6502_Read(cpu->pc);
  cpu->pc++;

  if (cpu->addr_rel & 0x80)
  {
    cpu->addr_rel |= 0xFF00;
  }

  return 0;
}
uint8_t ABS(struct R6502* cpu)
{
  uint16_t low = R6502_Read(cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu->pc);
  cpu->pc++;

  cpu->addr_abs = (high << 8) | low;

  return 0;
}
uint8_t ABX(struct R6502* cpu)
{
  uint16_t low = R6502_Read(cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu->pc);
  cpu->pc++;

  cpu->addr_abs = ((high << 8) | low) + cpu->x;

  if ((cpu->addr_abs & 0xFF00) != (high << 8))
    return 1;
  else
    return 0;
}
uint8_t ABY(struct R6502* cpu)
{
  uint16_t low = R6502_Read(cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu->pc);
  cpu->pc++;

  cpu->addr_abs = ((high << 8) | low) + cpu->y;

  if ((cpu->addr_abs & 0xFF00) != (high << 8))
    return 1;
  else
    return 0;
}
uint8_t IND(struct R6502* cpu)
{
  uint16_t ptr_low = R6502_Read(cpu->pc);
  cpu->pc++;
  uint16_t ptr_high = R6502_Read(cpu->pc);
  cpu->pc++;

  uint16_t ptr = ((ptr_high << 8) | ptr_low);

  // Original hardware had a bug we need to emulate here as programs worked around it.
  if (ptr_low == 0xFF00)
  {
    cpu->addr_abs = (R6502_Read(ptr & 0xFF00) << 8) | R6502_Read(ptr);
  }
  // Otherwise, behave normally
  else
  {
    cpu->addr_abs = (R6502_Read(ptr + 1) << 8) | R6502_Read(ptr);
  }

  return 0;
}
uint8_t IZX(struct R6502* cpu)
{
  uint16_t temp = R6502_Read(cpu->pc);
  cpu->pc++;

  uint16_t low  = R6502_Read((uint16_t)(temp + (uint16_t)cpu->x) & 0x00FF);
  uint16_t high = R6502_Read((uint16_t)(temp + (uint16_t)cpu->x + 1) & 0x00FF);

  cpu->addr_abs = (high << 8) | low;

  return 0;
}
uint8_t IZY(struct R6502* cpu)
{
  uint16_t temp = R6502_Read(cpu->pc);
  cpu->pc++;

  uint16_t low  = R6502_Read(temp & 0x00FF);
  uint16_t high = R6502_Read((temp + 1) & 0x00FF);

  cpu->addr_abs = ((high << 8) | low) + cpu->y;

  if ((cpu->addr_abs & 0xFF00) != (high << 8))
    return 1;
  else
    return 0;
}
#pragma endregion

/////////////////////////////////////
// OPCODES
/////////////////////////////////////
#pragma region OPCODES
// Forward Declaration of Fetch for use by Operations
uint8_t R6502_Fetch(struct R6502* cpu);

uint8_t ADC(struct R6502* cpu)
{
  R6502_Fetch(cpu);

  uint16_t temp = (uint16_t)cpu->a + (uint16_t)cpu->fetched + (uint16_t)R6502_GetFlag(cpu, _C_);
  R6502_SetFlag(cpu, _C_, temp > 255);
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  R6502_SetFlag(cpu, _V_, (~((uint16_t)cpu->a ^ (uint16_t)cpu->fetched) & ((uint16_t)cpu->a ^ (uint16_t)temp)) & 0x0080);
  cpu->a = temp & 0x00FF;

  return 1;
}
uint8_t AND(struct R6502* cpu)
{
  R6502_Fetch(cpu);
  cpu->a &= cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 1;
}
uint8_t ASL(struct R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint16_t temp = cpu->fetched << 1;
  R6502_SetFlag(cpu, _C_, (temp & 0xFF00) > 0);
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0);
  R6502_SetFlag(cpu, _N_, temp & 0x80);

  const struct INSTRUCTION* instructPtr = R6502_GetInstructionFromOpCode(cpu->opcode);
  if (instructPtr->addrmode == &IMP)
  {
    cpu->a = temp & 0x00FF;
  }
  else
  {
    R6502_Write(cpu->addr_abs, temp & 0x00FF);
  }

  return 0;
};
uint8_t BCC(struct R6502* cpu) 
{
  if (R6502_GetFlag(cpu, _C_) == 0)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0; 
};
uint8_t BCS(struct R6502* cpu)
{
  if (R6502_GetFlag(cpu, _C_) == 1)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0;
}
uint8_t BEQ(struct R6502* cpu) 
{ 
  if (R6502_GetFlag(cpu, _Z_) == 1)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0; 
};
uint8_t BIT(struct R6502* cpu) 
{
  R6502_Fetch(cpu);
  uint8_t temp = cpu->a & cpu->fetched;
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->fetched & (1 << 7));
  R6502_SetFlag(cpu, _V_, cpu->fetched & (1 << 6));
  return 0; 
};
uint8_t BMI(struct R6502* cpu) 
{ 
  if (R6502_GetFlag(cpu, _N_) == 1)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0;
  return 0; 
};
uint8_t BNE(struct R6502* cpu) 
{
  if (R6502_GetFlag(cpu, _Z_) == 0)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0; 
};
uint8_t BPL(struct R6502* cpu) 
{ 
  if (R6502_GetFlag(cpu, _N_) == 0)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0;
};
uint8_t BRK(struct R6502* cpu) 
{ 
  cpu->pc++;

  R6502_SetFlag(cpu, _I_, true);
  R6502_Write(0x0100 + cpu->pStk, (cpu->pc >> 8) & 0x00FF);
  cpu->pStk--;
  R6502_Write(0x0100 + cpu->pStk, cpu->pc & 0x00FF);
  cpu->pStk--;

  R6502_SetFlag(cpu, _B_, true);
  R6502_Write(0x0100 + cpu->pStk, cpu->status);
  cpu->pStk--;
  R6502_SetFlag(cpu, _B_, false);

  cpu->pc = (uint16_t)R6502_Read(0xFFFE) | ((uint16_t)R6502_Read(0xFFFF) << 8);
  return 0; 
};
uint8_t BVC(struct R6502* cpu)
{ 
  if (R6502_GetFlag(cpu, _V_) == 0)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0;
};
uint8_t BVS(struct R6502* cpu) 
{ 
  if (R6502_GetFlag(cpu, _V_) == 1)
  {
    cpu->cycles++;
    cpu->addr_abs = cpu->pc + cpu->addr_rel;

    if ((cpu->addr_abs & 0xFF00) != (cpu->pc & 0xFF00))
    {
      cpu->cycles++;
    }

    cpu->pc = cpu->addr_abs;
  }
  return 0;
};
uint8_t CLC(struct R6502* cpu)
{
  R6502_SetFlag(cpu, _C_, false);
  return 0;
}
uint8_t CLD(struct R6502* cpu)
{
  R6502_SetFlag(cpu, _D_, false);
  return 0;
}
uint8_t CLI(struct R6502* cpu)
{
  R6502_SetFlag(cpu, _I_, false);
  return 0;
}
uint8_t CLV(struct R6502* cpu)
{
  R6502_SetFlag(cpu, _V_, false);
  return 0;
}
uint8_t CMP(struct R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->a - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->a >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 1; 
};
uint8_t CPX(struct R6502* cpu)
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->x - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->x >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0;
};
uint8_t CPY(struct R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->y - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->y >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0; 
};
uint8_t DEC(struct R6502* cpu) { return 0; };
uint8_t DEX(struct R6502* cpu) { return 0; };
uint8_t DEY(struct R6502* cpu) { return 0; };
uint8_t EOR(struct R6502* cpu) { return 0; };
uint8_t INC(struct R6502* cpu) { return 0; };
uint8_t INX(struct R6502* cpu) { return 0; };
uint8_t INY(struct R6502* cpu) { return 0; };
uint8_t JMP(struct R6502* cpu) { return 0; };
uint8_t JSR(struct R6502* cpu) { return 0; };
uint8_t LDA(struct R6502* cpu) { return 0; };
uint8_t LDX(struct R6502* cpu) { return 0; };
uint8_t LDY(struct R6502* cpu) { return 0; };
uint8_t LSR(struct R6502* cpu) { return 0; };
uint8_t NOP(struct R6502* cpu) { return 0; };
uint8_t ORA(struct R6502* cpu) { return 0; };
uint8_t PHA(struct R6502* cpu) { return 0; };
uint8_t PHP(struct R6502* cpu) { return 0; };
uint8_t PLA(struct R6502* cpu) { return 0; };
uint8_t PLP(struct R6502* cpu) { return 0; };
uint8_t ROL(struct R6502* cpu) { return 0; };
uint8_t ROR(struct R6502* cpu) { return 0; };
uint8_t RTI(struct R6502* cpu) { return 0; };
uint8_t RTS(struct R6502* cpu) { return 0; };
uint8_t SBC(struct R6502* cpu)
{
  R6502_Fetch(cpu);

  uint16_t value = ((uint16_t)cpu->fetched) ^ 0x00FF;

  uint16_t temp = (uint16_t)cpu->a + value + (uint16_t)R6502_GetFlag(cpu, _C_);
  R6502_SetFlag(cpu, _C_, temp & 0xFF00);
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0);
  R6502_SetFlag(cpu, _N_, temp & 0x0080);
  R6502_SetFlag(cpu, _V_, (temp ^ (uint16_t)cpu->a) & (temp ^ value) & 0x0080);
  cpu->a = temp & 0x00FF;

  return 1;
}
uint8_t SEC(struct R6502* cpu) { return 0; };
uint8_t SED(struct R6502* cpu) { return 0; };
uint8_t SEI(struct R6502* cpu) { return 0; };
uint8_t STA(struct R6502* cpu) { return 0; };
uint8_t STX(struct R6502* cpu) { return 0; };
uint8_t STY(struct R6502* cpu) { return 0; };
uint8_t TAX(struct R6502* cpu) { return 0; };
uint8_t TAY(struct R6502* cpu) { return 0; };
uint8_t TSX(struct R6502* cpu) { return 0; };
uint8_t TXA(struct R6502* cpu) { return 0; };
uint8_t TXS(struct R6502* cpu) { return 0; };
uint8_t TYA(struct R6502* cpu) { return 0; };
uint8_t XXX(struct R6502* cpu) { return 0; };
#pragma endregion

static struct INSTRUCTION OpCodeMatrix[16 * 16] = { 
  {&BRK, &IMM, 7} , {&ORA, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &ZP0, 5} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&ASL, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BPL, &REL, 2} , {&ORA, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&ASL, &ABX, 7} , {&XXX, &IMM, 2} ,
  {&JSR, &ABS, 6} , {&AND, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&ROL, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BMI, &REL, 2} , {&AND, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&ROL, &ABX, 7} , {&XXX, &IMM, 2} ,
  {&RTI, &IMP, 6} , {&EOR, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&LSR, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BVC, &REL, 2} , {&EOR, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&LSR, &ABX, 7} , {&XXX, &IMM, 2} ,
  {&RTS, &IMP, 6} , {&ADC, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&ROR, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BVS, &REL, 2} , {&ADC, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&ROR, &ABX, 7} , {&XXX, &IMM, 2} ,
  {&NOP, &IMP, 2} , {&STA, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&STX, &ABS, 4} , {&XXX, &IMM, 2} ,
  {&BCC, &REL, 2} , {&STA, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} ,
  {&LDY, &IMM, 2} , {&LDA, &IZX, 6} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&LDX, &ABS, 4} , {&XXX, &IMM, 2} ,
  {&BCS, &REL, 2} , {&LDA, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&LDX, &ABY, 4} , {&XXX, &IMM, 2} ,
  {&CPY, &IMM, 2} , {&CMP, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&DEC, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BNE, &REL, 2} , {&CMP, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&DEC, &ABX, 7} , {&XXX, &IMM, 2} ,
  {&CPX, &IMM, 2} , {&SBC, &IZX, 6} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&INC, &ABS, 6} , {&XXX, &IMM, 2} ,
  {&BEQ, &REL, 2} , {&SBC, &IZX, 5} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&BRK, &IMM, 7} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&XXX, &IMM, 2} , {&BRK, &IMM, 7} , {&INC, &ABX, 7} , {&XXX, &IMM, 2}
};

uint8_t R6502_Fetch(struct R6502* cpu)
{
  struct INSTRUCTION* instructPtr = OpCodeMatrix + (sizeof(struct INSTRUCTION) * cpu->opcode);

  if (instructPtr->addrmode != &IMP)
  {
    cpu->fetched = R6502_Read(cpu->addr_abs);
  }

  return 0;
}

const struct INSTRUCTION* R6502_GetInstructionFromOpCode(uint8_t opCode)
{
  return OpCodeMatrix + (sizeof(struct INSTRUCTION) * opCode);
}

void R6502_Clock(struct R6502* cpu)
{
  if (cpu->cycles == 0)
  {
    cpu->opcode = R6502_Read(cpu->pc);
    cpu->pc++;

    struct INSTRUCTION* instructPtr = R6502_GetInstructionFromOpCode(cpu->opcode);
    uint8_t additional_addrmode = instructPtr->addrmode();
    uint8_t additional_operate  = instructPtr->operate();

    cpu->cycles = instructPtr->cycles + (additional_addrmode & additional_operate);
  }

  cpu->cycles--;
}