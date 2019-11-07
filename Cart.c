#include "Cart.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void Cart_LoadHeaderData(const char* fileName, RomHeader* header, Cartridge* cart)
{
  FILE* pFP;
  if (fopen_s(&pFP, fileName, "rb") == 0)
  {
    fread(&header, sizeof(RomHeader), 1, pFP);

    if (header->mapper1 & 0x04)
    {
      fseek(pFP, 512, SEEK_CUR);
    }

    // Find the Mapper ID
    cart->mapperID = ((header->mapper2 >> 4) << 4) | (header->mapper1 >> 4);

    // Determine File Format
    uint8_t nFileType = 1;

    if (nFileType == 0)
    {

    }

    if (nFileType == 1)
    {
      cart->prgBanks = header->prg_rom_chunks;
      cart->prgMemory = malloc(cart->prgBanks * 16384);
      fread(cart->prgMemory, cart->prgBanks * 16384, 1, pFP);

      cart->chrBanks = header->chr_rom_chunks;
      cart->chrMemory = malloc(cart->chrBanks * 8192);
      fread(cart->chrMemory, cart->chrBanks * 8192, 1, pFP);
    }

    if (nFileType == 2)
    {

    }

    fclose(pFP);
  }
}

void Cart_LoadCart(char* filename, Cartridge* cart)
{
  Cart_LoadHeaderData(filename, &cart->header, cart);
}

