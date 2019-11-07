#include "R6502.h"
#include "Bus.h"

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
  uint8_t(*operate)(R6502*);
  uint8_t(*addrmode)(R6502*);
  uint8_t cycles;
};

// Forward Dec
const struct INSTRUCTION* R6502_GetInstructionFromOpCode(uint8_t opCode);

void R6502_Write(R6502* cpu, uint16_t addr, uint8_t data)
{
  Bus_Write(cpu->bus, addr, data);
}

uint8_t R6502_Read(R6502* cpu, uint16_t addr)
{
  return Bus_Read(cpu->bus, addr);
}

uint8_t R6502_GetFlag(R6502* cpu, uint8_t flag)
{
  return (cpu->status & flag);
}

void R6502_SetFlag(R6502* cpu, uint8_t flag, bool value)
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
uint8_t IMP(R6502* cpu)
{
  cpu->fetched = cpu->a;
  return 0;
}
uint8_t IMM(R6502* cpu)
{
  cpu->addr_abs = cpu->pc++;
  return 0;
}
uint8_t ZP0(R6502* cpu)
{
  cpu->addr_abs = R6502_Read(cpu, cpu->pc);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t ZPX(R6502* cpu)
{
  cpu->addr_abs = (R6502_Read(cpu, cpu->pc) + cpu->x);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t ZPY(R6502* cpu)
{
  cpu->addr_abs = (R6502_Read(cpu, cpu->pc) + cpu->y);
  cpu->pc++;
  cpu->addr_abs &= 0x00FF;
  return 0;
}
uint8_t REL(R6502* cpu)
{
  cpu->addr_rel = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  if (cpu->addr_rel & 0x80)
  {
    cpu->addr_rel |= 0xFF00;
  }

  return 0;
}
uint8_t ABS(R6502* cpu)
{
  uint16_t low = R6502_Read(cpu, cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  cpu->addr_abs = (high << 8) | low;

  return 0;
}
uint8_t ABX(R6502* cpu)
{
  uint16_t low = R6502_Read(cpu, cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  cpu->addr_abs = ((high << 8) | low) + cpu->x;

  if ((cpu->addr_abs & 0xFF00) != (high << 8))
    return 1;
  else
    return 0;
}
uint8_t ABY(R6502* cpu)
{
  uint16_t low = R6502_Read(cpu, cpu->pc);
  cpu->pc++;
  uint16_t high = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  cpu->addr_abs = ((high << 8) | low) + cpu->y;

  if ((cpu->addr_abs & 0xFF00) != (high << 8))
    return 1;
  else
    return 0;
}
uint8_t IND(R6502* cpu)
{
  uint16_t ptr_low = R6502_Read(cpu, cpu->pc);
  cpu->pc++;
  uint16_t ptr_high = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  uint16_t ptr = ((ptr_high << 8) | ptr_low);

  // Original hardware had a bug we need to emulate here as programs worked around it.
  if (ptr_low == 0xFF00)
  {
    cpu->addr_abs = (R6502_Read(cpu, ptr & 0xFF00) << 8) | R6502_Read(cpu, ptr);
  }
  // Otherwise, behave normally
  else
  {
    cpu->addr_abs = (R6502_Read(cpu, ptr + 1) << 8) | R6502_Read(cpu, ptr);
  }

  return 0;
}
uint8_t IZX(R6502* cpu)
{
  uint16_t temp = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  uint16_t low  = R6502_Read(cpu, (uint16_t)(temp + (uint16_t)cpu->x) & 0x00FF);
  uint16_t high = R6502_Read(cpu, (uint16_t)(temp + (uint16_t)cpu->x + 1) & 0x00FF);

  cpu->addr_abs = (high << 8) | low;

  return 0;
}
uint8_t IZY(R6502* cpu)
{
  uint16_t temp = R6502_Read(cpu, cpu->pc);
  cpu->pc++;

  uint16_t low  = R6502_Read(cpu, temp & 0x00FF);
  uint16_t high = R6502_Read(cpu, (temp + 1) & 0x00FF);

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
uint8_t R6502_Fetch(R6502* cpu);

uint8_t ADC(R6502* cpu)
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
uint8_t AND(R6502* cpu)
{
  R6502_Fetch(cpu);
  cpu->a &= cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 1;
}
uint8_t ASL(R6502* cpu) 
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
    R6502_Write(cpu, cpu->addr_abs, temp & 0x00FF);
  }

  return 0;
};
uint8_t BCC(R6502* cpu) 
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
uint8_t BCS(R6502* cpu)
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
uint8_t BEQ(R6502* cpu) 
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
uint8_t BIT(R6502* cpu) 
{
  R6502_Fetch(cpu);
  uint8_t temp = cpu->a & cpu->fetched;
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->fetched & (1 << 7));
  R6502_SetFlag(cpu, _V_, cpu->fetched & (1 << 6));
  return 0; 
};
uint8_t BMI(R6502* cpu) 
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
uint8_t BNE(R6502* cpu) 
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
uint8_t BPL(R6502* cpu) 
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
uint8_t BRK(R6502* cpu) 
{ 
  cpu->pc++;

  R6502_SetFlag(cpu, _I_, true);
  R6502_Write(cpu, 0x0100 + cpu->pStk, (cpu->pc >> 8) & 0x00FF);
  cpu->pStk--;
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->pc & 0x00FF);
  cpu->pStk--;

  R6502_SetFlag(cpu, _B_, true);
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->status);
  cpu->pStk--;
  R6502_SetFlag(cpu, _B_, false);

  cpu->pc = (uint16_t)R6502_Read(cpu, 0xFFFE) | ((uint16_t)R6502_Read(cpu, 0xFFFF) << 8);
  return 0; 
};
uint8_t BVC(R6502* cpu)
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
uint8_t BVS(R6502* cpu) 
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
uint8_t CLC(R6502* cpu)
{
  R6502_SetFlag(cpu, _C_, false);
  return 0;
}
uint8_t CLD(R6502* cpu)
{
  R6502_SetFlag(cpu, _D_, false);
  return 0;
}
uint8_t CLI(R6502* cpu)
{
  R6502_SetFlag(cpu, _I_, false);
  return 0;
}
uint8_t CLV(R6502* cpu)
{
  R6502_SetFlag(cpu, _V_, false);
  return 0;
}
uint8_t CMP(R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->a - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->a >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 1; 
};
uint8_t CPX(R6502* cpu)
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->x - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->x >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0;
};
uint8_t CPY(R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->y - cpu->fetched;
  R6502_SetFlag(cpu, _C_, cpu->y >= cpu->fetched);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0; 
};
uint8_t DEC(R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->fetched - 1;
  R6502_Write(cpu, cpu->addr_abs, temp);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0; 
};
uint8_t DEX(R6502* cpu) 
{ 
  cpu->x--;
  R6502_SetFlag(cpu, _Z_, cpu->x == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->x & 0x80);
  return 0; 
};
uint8_t DEY(R6502* cpu) 
{
  cpu->y--;
  R6502_SetFlag(cpu, _Z_, cpu->y == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->y & 0x80);
  return 0;
};
uint8_t EOR(R6502* cpu) 
{
  R6502_Fetch(cpu);
  cpu->a ^= cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 1; 
};
uint8_t INC(R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  uint8_t temp = cpu->fetched + 1;
  R6502_Write(cpu, cpu->addr_abs, temp);
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);
  return 0; 
};
uint8_t INX(R6502* cpu) 
{ 
  cpu->x++;
  R6502_SetFlag(cpu, _Z_, cpu->x == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->x & 0x80);
  return 0; 
};
uint8_t INY(R6502* cpu) 
{ 
  cpu->y++;
  R6502_SetFlag(cpu, _Z_, cpu->y == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->y & 0x80);
  return 0;
};
uint8_t JMP(R6502* cpu) 
{ 
  cpu->pc = cpu->addr_abs;
  return 0; 
};
uint8_t JSR(R6502* cpu) 
{
  cpu->pc--;

  R6502_Write(cpu, 0x0100 + cpu->pStk, (cpu->pc >> 8) & 0x00FF);
  cpu->pStk--;
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->pc & 0x00FF);
  cpu->pStk--;

  cpu->pc = cpu->addr_abs;
  return 0; 
};
uint8_t LDA(R6502* cpu) 
{ 
  R6502_Fetch(cpu);

  cpu->a = cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);

  return 1; 
};
uint8_t LDX(R6502* cpu) 
{ 
  R6502_Fetch(cpu);

  cpu->x = cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->x == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->x & 0x80);

  return 1;
};
uint8_t LDY(R6502* cpu) 
{ 
  R6502_Fetch(cpu);

  cpu->y = cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->y == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->y & 0x80);

  return 1;
};
uint8_t LSR(R6502* cpu) 
{
  R6502_Fetch(cpu);
  R6502_SetFlag(cpu, _C_, cpu->fetched & 0x0001);
  uint8_t temp = cpu->fetched >> 1;
  R6502_SetFlag(cpu, _Z_, temp == 0x00);
  R6502_SetFlag(cpu, _N_, temp & 0x80);

  if (R6502_GetInstructionFromOpCode(cpu->opcode)->addrmode == &IMP)
  {
    cpu->a = temp;
  }
  else
  {
    R6502_Write(cpu, cpu->addr_abs, temp);
  }

  return 0; 
};
uint8_t NOP(R6502* cpu) 
{
  switch (cpu->opcode)
  {
    case 0x1C:
    case 0x3C:
    case 0x5C:
    case 0x7C:
    case 0xDC:
    case 0xFC:
      return 1;
  }
  return 0; 
};
uint8_t ORA(R6502* cpu) 
{ 
  R6502_Fetch(cpu);
  cpu->a |= cpu->fetched;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 1; 
};
uint8_t PHA(R6502* cpu) 
{
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->a);
  cpu->pStk--;
  return 1; 
};
uint8_t PHP(R6502* cpu) 
{
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->status | R6502_GetFlag(cpu, _B_) | R6502_GetFlag(cpu, _U_));
  R6502_SetFlag(cpu, _B_, false);
  R6502_SetFlag(cpu, _U_, false);
  cpu->pStk--;
  return 0; 
};
uint8_t PLA(R6502* cpu) 
{ 
  cpu->pStk++;
  cpu->a = R6502_Read(cpu, 0x0100 + cpu->pStk);
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 0; 
};
uint8_t PLP(R6502* cpu) 
{
  cpu->pStk--;
  cpu->status = R6502_Read(cpu, 0x100 + cpu->pStk);
  R6502_SetFlag(cpu, _U_, true);
  return 0; 
};
uint8_t ROL(R6502* cpu) 
{
  R6502_Fetch(cpu);
  uint16_t temp = (uint16_t)(cpu->fetched << 1) | R6502_GetFlag(cpu, _C_);
  R6502_SetFlag(cpu, _C_, temp & 0xFF00);
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0x0000);
  R6502_SetFlag(cpu, _N_, temp & 0x0080);

  if (R6502_GetInstructionFromOpCode(cpu->opcode)->addrmode == &IMP)
  {
    cpu->a = temp & 0x00FF;
  }
  else
  {
    R6502_Write(cpu, cpu->addr_abs, temp & 0x00FF);
  }

  return 0;
};
uint8_t ROR(R6502* cpu) 
{
  R6502_Fetch(cpu);
  uint16_t temp = (uint16_t)(R6502_GetFlag(cpu, _C_) << 7) | (cpu->fetched >> 1);
  R6502_SetFlag(cpu, _C_, cpu->fetched & 0x01);
  R6502_SetFlag(cpu, _Z_, (temp & 0x00FF) == 0x0000);
  R6502_SetFlag(cpu, _N_, temp & 0x0080);

  if (R6502_GetInstructionFromOpCode(cpu->opcode)->addrmode == &IMP)
  {
    cpu->a = temp & 0x00FF;
  }
  else
  {
    R6502_Write(cpu, cpu->addr_abs, temp & 0x00FF);
  }

  return 0;
};
uint8_t RTI(R6502* cpu) 
{ 
  cpu->pStk++;
  cpu->status = R6502_Read(cpu, 0x0100 + cpu->pStk);
  R6502_SetFlag(cpu, _B_, false);
  R6502_SetFlag(cpu, _U_, false);

  cpu->pStk++;
  cpu->pc = (uint16_t)R6502_Read(cpu, 0x0100 + cpu->pStk);
  cpu->pStk++;
  cpu->pc |= (uint16_t)R6502_Read(cpu, 0x0100 + cpu->pStk) << 8;
  return 0; 
};
uint8_t RTS(R6502* cpu) 
{
  cpu->pStk++;
  cpu->pc = (uint16_t)R6502_Read(cpu, 0x0100 + cpu->pStk);
  cpu->pStk++;
  cpu->pc |= (uint16_t)R6502_Read(cpu, 0x0100 + cpu->pStk) << 8;

  cpu->pc++;
  return 0; 
};
uint8_t SBC(R6502* cpu)
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
uint8_t SEC(R6502* cpu)
{ 
  R6502_SetFlag(cpu, _C_, true);
  return 0; 
};
uint8_t SED(R6502* cpu) 
{ 
  R6502_SetFlag(cpu, _D_, true);
  return 0; 
};
uint8_t SEI(R6502* cpu)
{ 
  R6502_SetFlag(cpu, _I_, true);
  return 0; 
};
uint8_t STA(R6502* cpu) 
{ 
  R6502_Write(cpu, cpu->addr_abs, cpu->a);
  return 0; 
};
uint8_t STX(R6502* cpu)
{ 
  R6502_Write(cpu, cpu->addr_abs, cpu->x);
  return 0; 
};
uint8_t STY(R6502* cpu)
{ 
  R6502_Write(cpu, cpu->addr_abs, cpu->y);
  return 0; 
};
uint8_t TAX(R6502* cpu)
{
  cpu->x = cpu->a;
  R6502_SetFlag(cpu, _Z_, cpu->x == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->x & 0x80);
  return 0; 
};
uint8_t TAY(R6502* cpu)
{
  cpu->y = cpu->a;
  R6502_SetFlag(cpu, _Z_, cpu->y == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->y & 0x80);
  return 0; 
};
uint8_t TSX(R6502* cpu) 
{
  cpu->x = cpu->pStk;
  R6502_SetFlag(cpu, _Z_, cpu->x == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->x & 0x80);
  return 0; 
};
uint8_t TXA(R6502* cpu) 
{
  cpu->a = cpu->x;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 0; 
};
uint8_t TXS(R6502* cpu)
{
  cpu->pStk = cpu->x;
  return 0; 
};
uint8_t TYA(R6502* cpu)
{ 
  cpu->a = cpu->y;
  R6502_SetFlag(cpu, _Z_, cpu->a == 0x00);
  R6502_SetFlag(cpu, _N_, cpu->a & 0x80);
  return 0; 
};
uint8_t XXX(R6502* cpu)
{ 
  return 0; 
};
#pragma endregion

static struct INSTRUCTION OpCodeMatrix[16 * 16] = { 
  {&BRK, &IMM, 7} , {&ORA, &IZX, 6} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 3} , {&ORA, &ZP0, 3} , {&ASL, &ZP0, 5} , {&XXX, &IMP, 5} , {&PHP, &IMP, 3} , {&ORA, &IMM, 2} , {&ASL, &IMP, 2} , {&XXX, &IMP, 2} , {&NOP, &IMP, 4} , {&ORA, &ABS, 4} , {&ASL, &ABS, 6} , {&XXX, &IMP, 6} , // Check
  {&BPL, &REL, 2} , {&ORA, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&ORA, &ZPX, 4} , {&ASL, &ZPX, 6} , {&XXX, &IMP, 6} , {&CLC, &IMP, 2} , {&ORA, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&ORA, &ABX, 4} , {&ASL, &ABX, 7} , {&XXX, &IMP, 7} ,
  {&JSR, &ABS, 6} , {&AND, &IZX, 6} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&BIT, &ZP0, 3} , {&AND, &ZP0, 3} , {&ROL, &ZP0, 5} , {&XXX, &IMP, 5} , {&PLP, &IMP, 4} , {&AND, &IMM, 2} , {&ROL, &IMP, 2} , {&XXX, &IMP, 2} , {&BIT, &ABS, 4} , {&AND, &ABS, 4} , {&ROL, &ABS, 6} , {&XXX, &IMP, 6} ,
  {&BMI, &REL, 2} , {&AND, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&AND, &ZPX, 4} , {&ROL, &ZPX, 6} , {&XXX, &IMP, 6} , {&SEC, &IMP, 2} , {&AND, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&AND, &ABX, 4} , {&ROL, &ABX, 7} , {&XXX, &IMP, 7} ,
  {&RTI, &IMP, 6} , {&EOR, &IZX, 6} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 3} , {&EOR, &ZP0, 3} , {&LSR, &ZP0, 5} , {&XXX, &IMP, 5} , {&PHA, &IMP, 3} , {&EOR, &IMM, 2} , {&LSR, &IMP, 2} , {&XXX, &IMP, 2} , {&JMP, &ABS, 3} , {&EOR, &ABS, 4} , {&LSR, &ABS, 6} , {&XXX, &IMP, 6} ,
  {&BVC, &REL, 2} , {&EOR, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&EOR, &ZPX, 4} , {&LSR, &ZPX, 6} , {&XXX, &IMP, 6} , {&CLI, &IMP, 2} , {&EOR, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&EOR, &ABX, 4} , {&LSR, &ABX, 7} , {&XXX, &IMP, 7} ,
  {&RTS, &IMP, 6} , {&ADC, &IZX, 6} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 3} , {&ADC, &ZP0, 3} , {&ROR, &ZP0, 5} , {&XXX, &IMP, 5} , {&PLA, &IMP, 4} , {&ADC, &IMM, 2} , {&ROR, &IMP, 2} , {&XXX, &IMP, 2} , {&JMP, &IND, 5} , {&ADC, &ABS, 4} , {&ROR, &ABS, 6} , {&XXX, &IMP, 6} ,
  {&BVS, &REL, 2} , {&ADC, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&ADC, &ZPX, 4} , {&ROR, &ZPX, 6} , {&XXX, &IMP, 6} , {&SEI, &IMP, 2} , {&ADC, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&ADC, &ABX, 4} , {&ROR, &ABX, 7} , {&XXX, &IMP, 7} ,
  {&NOP, &IMP, 2} , {&STA, &IZX, 6} , {&NOP, &IMP, 2} , {&XXX, &IMP, 6} , {&STY, &ZP0, 3} , {&STA, &ZP0, 3} , {&STX, &ZP0, 3} , {&XXX, &IMP, 3} , {&DEY, &IMP, 2} , {&NOP, &IMP, 2} , {&TXA, &IMP, 2} , {&XXX, &IMP, 2} , {&STY, &ABS, 4} , {&STA, &ABS, 4} , {&STX, &ABS, 4} , {&XXX, &IMP, 4} ,
  {&BCC, &REL, 2} , {&STA, &IZY, 6} , {&XXX, &IMP, 2} , {&XXX, &IMP, 6} , {&STY, &ZPX, 4} , {&STA, &ZPX, 4} , {&STX, &ZPY, 4} , {&XXX, &IMP, 4} , {&TYA, &IMP, 2} , {&STA, &ABY, 5} , {&TXS, &IMP, 2} , {&XXX, &IMP, 5} , {&NOP, &IMP, 5} , {&STA, &ABX, 5} , {&XXX, &IMP, 5} , {&XXX, &IMP, 5} ,
  {&LDY, &IMM, 2} , {&LDA, &IZX, 6} , {&LDX, &IMM, 2} , {&XXX, &IMP, 6} , {&LDY, &ZP0, 3} , {&LDA, &ZP0, 3} , {&LDX, &ZP0, 3} , {&XXX, &IMP, 3} , {&TAY, &IMP, 2} , {&LDA, &IMM, 2} , {&TAX, &IMP, 2} , {&XXX, &IMP, 2} , {&LDY, &ABS, 4} , {&LDA, &ABS, 4} , {&LDX, &ABS, 4} , {&XXX, &IMP, 4} ,
  {&BCS, &REL, 2} , {&LDA, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 5} , {&LDY, &ZPX, 4} , {&LDA, &ZPX, 4} , {&LDX, &ZPY, 4} , {&XXX, &IMP, 4} , {&CLV, &IMP, 2} , {&LDA, &ABY, 4} , {&TSX, &IMP, 2} , {&XXX, &IMP, 4} , {&LDY, &ABX, 4} , {&LDA, &ABX, 4} , {&LDX, &ABY, 4} , {&XXX, &IMP, 4} ,
  {&CPY, &IMM, 2} , {&CMP, &IZX, 6} , {&NOP, &IMP, 2} , {&XXX, &IMP, 8} , {&CPY, &ZP0, 3} , {&CMP, &ZP0, 3} , {&DEC, &ZP0, 5} , {&XXX, &IMP, 5} , {&INY, &IMP, 2} , {&CMP, &IMM, 2} , {&DEX, &IMP, 2} , {&XXX, &IMP, 2} , {&CPY, &ABS, 4} , {&CMP, &ABS, 4} , {&DEC, &ABS, 6} , {&XXX, &IMP, 6} ,
  {&BNE, &REL, 2} , {&CMP, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&CMP, &ZPX, 4} , {&DEC, &ZPX, 6} , {&XXX, &IMP, 6} , {&CLD, &IMP, 2} , {&CMP, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&CMP, &ABX, 4} , {&DEC, &ABX, 7} , {&XXX, &IMP, 7} ,
  {&CPX, &IMM, 2} , {&SBC, &IZX, 6} , {&NOP, &IMP, 2} , {&XXX, &IMP, 8} , {&CPX, &ZP0, 3} , {&SBC, &ZP0, 3} , {&INC, &ZP0, 5} , {&XXX, &IMP, 5} , {&INX, &IMP, 2} , {&SBC, &IMM, 2} , {&NOP, &IMP, 2} , {&SBC, &IMP, 2} , {&CPX, &ABS, 4} , {&SBC, &ABS, 4} , {&INC, &ABS, 6} , {&XXX, &IMP, 6} ,
  {&BEQ, &REL, 2} , {&SBC, &IZY, 5} , {&XXX, &IMP, 2} , {&XXX, &IMP, 8} , {&NOP, &IMP, 4} , {&SBC, &ZPX, 4} , {&INC, &ZPX, 6} , {&XXX, &IMP, 6} , {&SED, &IMP, 2} , {&SBC, &ABY, 4} , {&NOP, &IMP, 2} , {&XXX, &IMP, 7} , {&NOP, &IMP, 4} , {&SBC, &ABX, 4} , {&INC, &ABX, 7} , {&XXX, &IMP, 7}
};

uint8_t R6502_Fetch(R6502* cpu)
{
  if (OpCodeMatrix[cpu->opcode].addrmode != &IMP)
  {
    cpu->fetched = R6502_Read(cpu, cpu->addr_abs);
  }

  return 0;
}

const struct INSTRUCTION* R6502_GetInstructionFromOpCode(uint8_t opCode)
{
  return &OpCodeMatrix[opCode];
}

void R6502_Clock(R6502* cpu)
{
  if (cpu->cycles == 0)
  {
    cpu->opcode = R6502_Read(cpu, cpu->pc);
    cpu->pc++;

    const struct INSTRUCTION* instructPtr = R6502_GetInstructionFromOpCode(cpu->opcode);
    uint8_t additional_addrmode = instructPtr->addrmode(cpu);
    uint8_t additional_operate  = instructPtr->operate(cpu);

    cpu->cycles = instructPtr->cycles + (additional_addrmode & additional_operate);
  }

  cpu->cycles--;
}

void R6502_Reset(R6502* cpu)
{
  cpu->addr_abs = 0xFFFC;
  uint16_t low  = R6502_Read(cpu, cpu->addr_abs);
  uint16_t high = R6502_Read(cpu, cpu->addr_abs + 1);

  cpu->pc = (high << 8) | low;

  cpu->a = cpu->x = cpu->y = 0;
  cpu->pStk = 0xFD;
  cpu->status = 0x00 | R6502_GetFlag(cpu, _U_);

  cpu->addr_abs = cpu->addr_rel = 0x0000;
  cpu->fetched = 0x00;

  cpu->cycles = 8;
}

void R6502_IRQ(R6502* cpu)
{
  if (R6502_GetFlag(cpu, _I_) != 1)
  {
    R6502_Write(cpu, 0x0100 + cpu->pStk, (cpu->pc >> 8) & 0x00FF);
    cpu->pStk--;
    R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->pc & 0x00FF);
    cpu->pStk--;

    R6502_SetFlag(cpu, _B_, false);
    R6502_SetFlag(cpu, _U_, true);
    R6502_SetFlag(cpu, _I_, true);
    R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->status);
    cpu->pStk;

    cpu->addr_abs = 0xFFFE;
    uint16_t low = R6502_Read(cpu, cpu->addr_abs);
    uint16_t high = R6502_Read(cpu, cpu->addr_abs + 1);
    cpu->pc = (high << 8) | low;

    cpu->cycles = 7;
  }
}

void R6502_NMI(R6502* cpu)
{
  R6502_Write(cpu, 0x0100 + cpu->pStk, (cpu->pc >> 8) & 0x00FF);
  cpu->pStk--;
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->pc & 0x00FF);
  cpu->pStk--;

  R6502_SetFlag(cpu, _B_, false);
  R6502_SetFlag(cpu, _U_, true);
  R6502_SetFlag(cpu, _I_, true);
  R6502_Write(cpu, 0x0100 + cpu->pStk, cpu->status);
  cpu->pStk;

  cpu->addr_abs = 0xFFFE;
  uint16_t low = R6502_Read(cpu, cpu->addr_abs);
  uint16_t high = R6502_Read(cpu, cpu->addr_abs + 1);
  cpu->pc = (high << 8) | low;

  cpu->cycles = 8;
}
