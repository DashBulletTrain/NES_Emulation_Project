#include "Bus.h"

void Bus_AttachComponents(struct SystemBus* bus)
{
  bus->cpu.bus = bus;
}

void Bus_Write(struct SystemBus* bus, uint16_t addr, uint8_t data)
{
  if (addr <= 0x1FFF)
  {
    bus->cpuRam.ram[addr] = data; // &0x07FF] = data;
  }
}

uint8_t Bus_Read(struct SystemBus* bus, uint16_t addr)
{
  uint8_t data = 0x00;

  if (addr >= 0x0000 && addr <= 0xFFFF)
  {
    data = bus->cpuRam.ram[addr]; //& 0x07FF]; // Account for mirroring.
  }

  return data;
}
