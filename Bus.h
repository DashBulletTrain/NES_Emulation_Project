#pragma once
#include <stdint.h>
#include "R6502.h"
#include "PPU.h"
#include "Ram.h"
#include "Cart.h"

typedef struct
{
  SystemRam cpuRam;
  R6502 cpu;
  NES_PPU ppu;
  Cartridge cart;

  uint32_t systemClockCounter;
} SystemBus;

void Bus_AttachComponents(SystemBus* bus);

void Bus_Write(SystemBus* bus, uint16_t addr, uint8_t data);
uint8_t Bus_Read(SystemBus* bus, uint16_t addr);

void Bus_AttachCart(SystemBus* bus, Cartridge* cart);
void Bus_Reset(SystemBus* bus);
void Bus_Clock(SystemBus* bus);
