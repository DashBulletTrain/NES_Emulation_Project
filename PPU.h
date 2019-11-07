#pragma once
#include <stdint.h>

struct SystemBus;
struct Cartridge;

typedef struct
{
  uint8_t nameTable[2][1024];
  uint8_t palette[32];

  struct SystemBus* bus;
  struct Cartridge* cart;
} NES_PPU;

void PPU_InsertCart(NES_PPU* ppu, struct Cartridge* _cart);
void PPU_Clock(NES_PPU* ppu);