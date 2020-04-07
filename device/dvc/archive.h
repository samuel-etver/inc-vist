#ifndef ARCHIVE_H_INCLUDED
#define ARCHIVE_H_INCLUDED

/*---------------------------------------------------------
  Archive.h
---------------------------------------------------------*/

#include "xmain.h"

void ShowArchiveIni(void);
void ShowArchive(void);

BOOL ArchiveCheck(void);
void ArchiveNoDataIni(void);
void ArchiveNoData(void);
void ArchiveDataIni(void);
BOOL ArchiveData(void);

void ArchiveDrawCalendar(void);
void ArchiveDrawNumber(void);

extern NOINIT BOOL ArchiveRedraw;

#endif
