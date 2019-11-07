#pragma once
#include <stdint.h>

struct Mapper;

typedef struct
{
  char name[4];
  uint8_t prg_rom_chunks;
  uint8_t chr_rom_chunks;
  uint8_t mapper1;
  uint8_t mapper2;
  uint8_t prg_ram_size;
  uint8_t tv_system1;
  uint8_t tv_system2;
  char unused[5];
} RomHeader;

typedef struct
{
  RomHeader header;
  uint8_t mapperID;
  uint8_t prgBanks;
  uint8_t chrBanks;

  uint8_t* prgMemory;
  uint8_t* chrMemory;

  struct Mapper* mapper;

  struct SystemBus* bus;
} Cartridge;

void Cart_LoadCart(char* filename, Cartridge* cart);