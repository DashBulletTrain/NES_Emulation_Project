#include "Bus.h"

void main()
{
  struct SystemBus cpuBus;
  Bus_AttachComponents(&cpuBus);

  // Set Resets
  cpuBus.cpuRam.ram[0] = 0;
  cpuBus.cpuRam.ram[1] = 0x00;
  cpuBus.cpuRam.ram[0x02] = 2;
  cpuBus.cpuRam.ram[0x03] = 0x02;



  cpuBus.cpuRam.ram[0xFFFC] = 0x00;
  cpuBus.cpuRam.ram[0xFFFD] = 0x80;

  // Set Data
  uint16_t offset = 0x8000;
  cpuBus.cpuRam.ram[offset++] = 0xA2;
  cpuBus.cpuRam.ram[offset++] = 0x0A;
  cpuBus.cpuRam.ram[offset++] = 0x8E;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  
  cpuBus.cpuRam.ram[offset++] = 0xA2;
  cpuBus.cpuRam.ram[offset++] = 0x03;
  cpuBus.cpuRam.ram[offset++] = 0x8E;
  cpuBus.cpuRam.ram[offset++] = 0x01;
  cpuBus.cpuRam.ram[offset++] = 0x00;

  cpuBus.cpuRam.ram[offset++] = 0xAC;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  cpuBus.cpuRam.ram[offset++] = 0xA9;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  
  cpuBus.cpuRam.ram[offset++] = 0x18;
  cpuBus.cpuRam.ram[offset++] = 0x6D;
  cpuBus.cpuRam.ram[offset++] = 0x01;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  cpuBus.cpuRam.ram[offset++] = 0x88;
  
  cpuBus.cpuRam.ram[offset++] = 0xD0;
  cpuBus.cpuRam.ram[offset++] = 0xFA;
  cpuBus.cpuRam.ram[offset++] = 0x8D;
  cpuBus.cpuRam.ram[offset++] = 0x02;
  cpuBus.cpuRam.ram[offset++] = 0x00;
  
  cpuBus.cpuRam.ram[offset++] = 0xEA;
  cpuBus.cpuRam.ram[offset++] = 0xEA;
  cpuBus.cpuRam.ram[offset++] = 0xEA;

  R6502_Reset(&cpuBus.cpu);

  while (true)
  {
    R6502_Clock(&cpuBus.cpu);
  }

  return;
}