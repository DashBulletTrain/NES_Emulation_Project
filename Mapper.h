#pragma once
#include <stdint.h>

struct Cartridge;
struct Mapper;

typedef struct
{
  uint8_t(*MapRead)(struct Mapper* mapper, uint16_t addr, uint32_t& mapped_addr);
  uint8_t(*MapWrite)(struct Mapper* mapper, uint16_t addr, uint32_t& mapped_addr);

  Cartridge* cart;

  uint8_t prgBanks;
  uint8_t chrBanks;
} Mapper;
