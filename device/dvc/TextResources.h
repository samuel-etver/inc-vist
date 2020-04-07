#ifndef TEXTRESOURCES_H_INCLUDED
#define TEXTRESIURCES_H_INCLUDED

/*---------------------------------------------------------
  TextResources.h
---------------------------------------------------------*/

#include "xmain.h"

#define TEXTRESOURCE_EOF            0xFFFF

BYTE const* GetTextResource(WORD id);
BYTE const* GetLangResource(WORD id);
BYTE LoadTextResource(BYTE* buff, WORD id);
BYTE LoadLangResource(BYTE* buff, WORD id);

#endif
