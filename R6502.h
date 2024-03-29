#pragma once
#include <stdint.h>
#include <stdbool.h>

struct SystemBus;

typedef struct
{
  uint8_t a;       // Accumulator
  uint8_t x;       // X Register
  uint8_t y;       // Y Register
  uint8_t pStk;    // Stack Pointer
  uint8_t status;  // Status Flags
  uint16_t pc;     // Program Counter

  uint8_t fetched;
  uint16_t addr_abs;
  uint16_t addr_rel;
  uint8_t opcode;
  uint8_t cycles;

  struct SystemBus* bus;
} R6502;

void R6502_Clock(R6502* cpu);
void R6502_Reset(R6502* cpu);
void R6502_IRQ(R6502* cpu);
void R6502_NMI(R6502* cpu);