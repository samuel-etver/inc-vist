#ifndef AUTOOFF_H_INCLUDED
#define AUTOODD_H_INCLUDED

/*---------------------------------------------------------
  autooff.h
---------------------------------------------------------*/

void InitAutoOff(void);
void ShowAutoOffIni(void);
void ShowAutoOff(void);

void LoadAutoOff(void);
void SaveAutoOff(void);

void LoadFirstProducedAutoOff(void);

void DoAutoOff(void);
void ClearAutoOff(void);

#endif
