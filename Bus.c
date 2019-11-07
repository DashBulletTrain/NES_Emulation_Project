#include "Bus.h"

void Bus_AttachComponents(SystemBus* bus)
{
  bus->cpu.bus = bus;
  bus->cart.bus = bus;
  bus->ppu.bus = bus;
}

void Bus_Write(SystemBus* bus, uint16_t addr, uint8_t data)
{
  if (addr <= 0x1FFF)
  {
    bus->cpuRam.ram[addr & 0x07FF] = data;
  }
  else if (addr <= 0x3FFF)
  {
    // PPU Addressing
    // TODO: ppu write (addr & 0x0007, data);
  }


}

uint8_t Bus_Read(SystemBus* bus, uint16_t addr)
{
  uint8_t data = 0x00;

  if (addr >= 0x0000 && addr <= 0x1FFF)
  {
    data = bus->cpuRam.ram[addr & 0x07FF]; // Account for mirroring.
  }
  else if (addr <= 0x3FFF)
  {
    // PPU Addressing
    // TODO: data = ppu read (addr & 0x0007);
  }
  return data;
}

void Bus_AttachCart(SystemBus* bus, Cartridge* cart)
{
  bus->cart = *cart;
  PPU_InsertCart(&bus->ppu, cart);
}

void Bus_Reset(SystemBus* bus)
{
  R6502_Reset(&bus->cpu);
  bus->systemClockCounter = 0;
}

void Bus_Clock(SystemBus* bus)
{
  R6502_Clock(&bus->cpu);
}
