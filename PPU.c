#include "PPU.h"
#include "Cart.h"

void PPU_InsertCart(NES_PPU* ppu, struct Cartridge* _cart)
{
  ppu->cart = _cart;
}

void PPU_Clock(NES_PPU* ppu)
{

}