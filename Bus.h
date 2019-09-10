#pragma once
#include <stdint.h>
#include "R6502.h"
#include "Ram.h"

struct SystemBus
{
  struct SystemRam cpuRam;
  struct R6502 cpu;
};

void Bus_AttachComponents(struct SystemBus* bus);

void Bus_Write(struct SystemBus* bus, uint16_t addr, uint8_t data);
uint8_t Bus_Read(struct SystemBus* bus, uint16_t addr);