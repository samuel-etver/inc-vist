/*---------------------------------------------------------
  TextResources.c
---------------------------------------------------------*/

#include "TextResources.h"
#include "language.h"

static const BYTE Empty[] = "";

#define Tr0000 Empty
#define Tr0001 Tr0000
static const BYTE Tr0002[] = " Русский ";
static const BYTE Tr0003[] = " Russian ";
static const BYTE Tr0004[] = " Английский ";
static const BYTE Tr0005[] = " English ";
static const BYTE Tr0006[] = "Авт.усиление";
static const BYTE Tr0007[] = "Auto amp.";
#define Tr0008 Empty
static const BYTE Tr0009[] = "Количество";
static const BYTE Tr0010[] = "Number of";
static const BYTE Tr0011[] = "замеров:";
static const BYTE Tr0012[] = "measurements:";
#define Tr0013 Empty
#define Tr0014 Empty
static const BYTE Tr0015[] = "Аккумулятор";
static const BYTE Tr0016[] = "Accumulator";
static const BYTE Tr0017[] = "Ошибка!";
static const BYTE Tr0018[] = "Error!";
#define Tr0019 Empty
#define Tr0020 Empty
static const BYTE Tr0021[] = "МПа";
static const BYTE Tr0022[] = "MPa";
static const BYTE Tr0023[] = "кгс/см\026";
static const BYTE Tr0024[] = "kgf/cm\026";
#define Tr0025 Empty
#define Tr0026 Empty
static const BYTE Tr0027[] = "Измерит.тракт";
static const BYTE Tr0028[] = "Measuring tract";
static const BYTE Tr0029[] = "Автоматический";
static const BYTE Tr0030[] = "Auto";
#define Tr0031 Empty
#define Tr0032 Empty
#define Tr0033 Empty
#define Tr0034 Empty
#define Tr0035 Empty
#define Tr0036 Empty
#define Tr0037 Empty
#define Tr0038 Empty
#define Tr0039 Empty
#define Tr0040 Empty
#define Tr0041 Empty
#define Tr0042 Empty
#define Tr0043 Empty
#define Tr0044 Empty
static const BYTE Tr0045[] = "W = ";
static const BYTE Tr0046[] = "W = ";
static const BYTE Tr0047[] = "H = ";
static const BYTE Tr0048[] = "H = ";
static const BYTE Tr0049[] = "Тест ЦАП,ФНЧ";
static const BYTE Tr0050[] = "DAC,LPF test ";
static const BYTE Tr0051[] = "Объект контроля";
static const BYTE Tr0052[] = "Object";
static const BYTE Tr0053[] = "Uср =";
static const BYTE Tr0054[] = "Uth =";
static const BYTE Tr0055[] = "Установки";
static const BYTE Tr0056[] = "Parameters";
static const BYTE Tr0057[] = "Сервис";
static const BYTE Tr0058[] = "Service";
static const BYTE Tr0059[] = "Инженер.меню";
static const BYTE Tr0060[] = "Engineer.menu";
#define Tr0061 Empty
#define Tr0062 Empty
static const BYTE Tr0063[] = "Память";
static const BYTE Tr0064[] = "Memory";
static const BYTE Tr0065[] = "Архив";
static const BYTE Tr0066[] = "Archive";
static const BYTE Tr0067[] = "Ресурсы";
static const BYTE Tr0068[] = "Resources";
static const BYTE Tr0069[] = "Очистка памяти";
static const BYTE Tr0070[] = "Memory clear";
#define Tr0071 Tr0055
#define Tr0072 Tr0056
#define Tr0073 Empty
#define Tr0074 Empty
#define Tr0075 Empty
#define Tr0076 Empty
#define Tr0077 Empty
#define Tr0078 Empty
static const BYTE Tr0079[] = "Размерность:";
static const BYTE Tr0080[] = "Units:";
#define Tr0081 Tr0057
#define Tr0082 Tr0058
static const BYTE Tr0083[] = "Калибровка";
static const BYTE Tr0084[] = "Calibration";
#define Tr0085 Empty
#define Tr0086 Empty
static const BYTE Tr0087[] = "Ист.питания";
static const BYTE Tr0088[] = "Supply source";
static const BYTE Tr0089[] = "Энергосбереж.";
static const BYTE Tr0090[] = "Power savings";
static const BYTE Tr0091[] = "Дата,время";
static const BYTE Tr0092[] = "Date,time";
static const BYTE Tr0093[] = "Язык";
static const BYTE Tr0094[] = "Language";
#define Tr0095 Tr0059
#define Tr0096 Tr0060
#define Tr0097 Empty
#define Tr0098 Empty
static const BYTE Tr0099[] = "Наладка";
static const BYTE Tr0100[] = "Debug";
static const BYTE Tr0101[] = "Архив";
static const BYTE Tr0102[] = "Archive";
static const BYTE Tr0103[] = "Коэф.датчика";
static const BYTE Tr0104[] = "Sensor factors";
#define Tr0105 Empty
#define Tr0106 Empty
static const BYTE Tr0107[] = "Зав.установки";
static const BYTE Tr0108[] = "Factory set";
static const BYTE Tr0109[] = "Режим прибора";
static const BYTE Tr0110[] = "Device type";
static const BYTE Tr0111[] = "О приборе";
static const BYTE Tr0112[] = "About device";
static const BYTE Tr0113[] = "Длина крепежа";
static const BYTE Tr0114[] = "Fastener length";
#define Tr0115 Empty
#define Tr0116 Empty
static const BYTE Tr0117[] = "<ПРОИЗВОДИТЕЛЬ>";
static const BYTE Tr0118[] = "<MANUFACTURER>";
static const BYTE Tr0119[] = "Пароль";
static const BYTE Tr0120[] = "Password";
static const BYTE Tr0121[] = "AC";
static const BYTE Tr0122[] = "Зарядить АКБ!";
static const BYTE Tr0123[] = "Charge ACC, please.";
#define Tr0124 Empty
#define Tr0125 Empty
#define Tr0126 Empty
#define Tr0127 Empty
static const BYTE Tr0128[] = "S/V/A";
#define Tr0129 Tr0128
static const BYTE Tr0130[] = "Калибровка";
static const BYTE Tr0131[] = "Calibration";
static const BYTE Tr0132[] = "завершена!";
static const BYTE Tr0133[] = "completed!";
static const BYTE Tr0134[] = "\010 = ";
#define Tr0135 Tr0134
static const BYTE Tr0136[] = ">50 МПа";
static const BYTE Tr0137[] = ">50 MPa";
#define Tr0138 Empty
#define Tr0139 Empty
static const BYTE Tr0140[] = "\00250 мм";
static const BYTE Tr0141[] = "\00250 mm";
static const BYTE Tr0142[] = ">50 мм";
static const BYTE Tr0143[] = ">50 mm";
static const BYTE Tr0144[] = "Проектное";
static const BYTE Tr0145[] = "Estimated";
static const BYTE Tr0146[] = "напряжение:";
static const BYTE Tr0147[] = "tension:";
static const BYTE Tr0148[] = "ПОМЕХИ!";
static const BYTE Tr0149[] = "NOISE!";
static const BYTE Tr0150[] = "Повторите";
static const BYTE Tr0151[] = "Repeat";
static const BYTE Tr0152[] = "измерение!";
static const BYTE Tr0153[] = "the measurement!";
#define Tr0154 Empty
#define Tr0155 Empty
static const BYTE Tr0156[] = "Калибров.коэф.";
static const BYTE Tr0157[] = "Calib.factors";
static const BYTE Tr0158[] = "Датчик";
static const BYTE Tr0159[] = "Sensor";
static const BYTE Tr0160[] = "m\014";
static const BYTE Tr0161[] = "m\014";
static const BYTE Tr0162[] = "m\015";
static const BYTE Tr0163[] = "m\015";
static const BYTE Tr0164[] = "(U =     В)";
static const BYTE Tr0165[] = "(U =     V)";
static const BYTE Tr0166[] = "Да";
static const BYTE Tr0167[] = "Yes";
static const BYTE Tr0168[] = "Нет";
static const BYTE Tr0169[] = "No";
static const BYTE Tr0170[] = "Очистить память?";
static const BYTE Tr0171[] = "Clear memory?";
#define Tr0172 Empty
#define Tr0173 Empty
static const BYTE Tr0174[] = "Этап %i:";
static const BYTE Tr0175[] = "Step %i:";
static const BYTE Tr0176[] = "мкм";
static const BYTE Tr0177[] = "\036m";
#define Tr0178 Empty
#define Tr0179 Empty
static const BYTE Tr0180[] = "Установить?";
static const BYTE Tr0181[] = "Set?";
static const BYTE Tr0182[] = "Время:";
static const BYTE Tr0183[] = "Time:";
static const BYTE Tr0184[] = "Дата:";
static const BYTE Tr0185[] = "Date:";
static const BYTE Tr0186[] = "Янв";
static const BYTE Tr0187[] = "Jan";
static const BYTE Tr0188[] = "Фев";
static const BYTE Tr0189[] = "Feb";
static const BYTE Tr0190[] = "Мар";
static const BYTE Tr0191[] = "Mar";
static const BYTE Tr0192[] = "Апр";
static const BYTE Tr0193[] = "Apr";
static const BYTE Tr0194[] = "Май";
static const BYTE Tr0195[] = "May";
static const BYTE Tr0196[] = "Июн";
static const BYTE Tr0197[] = "Jun";
static const BYTE Tr0198[] = "Июл";
static const BYTE Tr0199[] = "Jul";
static const BYTE Tr0200[] = "Авг";
static const BYTE Tr0201[] = "Aug";
static const BYTE Tr0202[] = "Сен";
static const BYTE Tr0203[] = "Sep";
static const BYTE Tr0204[] = "Окт";
static const BYTE Tr0205[] = "Oct";
static const BYTE Tr0206[] = "Ноя";
static const BYTE Tr0207[] = "Nov";
static const BYTE Tr0208[] = "Дек";
static const BYTE Tr0209[] = "Dec";
static const BYTE Tr0210[] = "???";
static const BYTE Tr0211[] = "Ресурсы";
static const BYTE Tr0212[] = "Resources";
static const BYTE Tr0213[] = "Режим измер.";
static const BYTE Tr0214[] = "Measure mode";
static const BYTE Tr0215[] = "Измерение";
static const BYTE Tr0216[] = "Measure";
static const BYTE Tr0217[] = "Автоматическое";
static const BYTE Tr0218[] = "Auto";
static const BYTE Tr0219[] = "усиление:";
static const BYTE Tr0220[] = "amplifying:";
#define Tr0221 Empty
#define Tr0222 Empty
static const BYTE Tr0223[] = "L = ";
#define Tr0224 Tr0223
#define Tr0225 Empty
#define Tr0226 Empty
static const BYTE Tr0227[] = "U =      В";
static const BYTE Tr0228[] = "U =      V";
static const BYTE Tr0229[] = "Заряд %i%%";
static const BYTE Tr0230[] = "Battery %i%%";
#define Tr0231 Empty
#define Tr0232 Empty
static const BYTE Tr0233[] = "F = ";
#define Tr0234 Tr0233
#define Tr0235 Tr0225
#define Tr0236 Tr0226
static BYTE Tr0237[] = "Измеренные значения";
static BYTE Tr0238[] = "The measured values";
static BYTE Tr0239[] = "должны различаться!";
static BYTE Tr0240[] = "must differ!";
static BYTE Tr0241[] = "Установка \"0\"";
static BYTE Tr0242[] = "Zero calibration";
#define Tr0243 Empty
#define Tr0244 Empty
static BYTE Tr0245[] = "Установить";
static BYTE Tr0246[] = "Set";
static BYTE Tr0247[] = "Сейчас на входе:";
static BYTE Tr0248[] = "Input signal:";
#define Tr0249 Empty
static const BYTE Tr0250[] = "Всего";
static const BYTE Tr0251[] = "Total";
static const BYTE Tr0252[] = "Занято";
static const BYTE Tr0253[] = "Used";
static const BYTE Tr0254[] = "Свободно";
static const BYTE Tr0255[] = "Free";
#define Tr0256 Empty
#define Tr0257 Empty
#define Tr0258 Empty
#define Tr0259 Empty
static const BYTE Tr0260[] = "Автоматич.отключение";
static const BYTE Tr0261[] = "Autooff";
static const BYTE Tr0262[] = "прибора через:";
static const BYTE Tr0263[] = "device in:";
static const BYTE Tr0264[] = "подсветки через:";
static const BYTE Tr0265[] = "light in:";
static const BYTE Tr0266[] = "м";
static const BYTE Tr0267[] = "m";
static const BYTE Tr0268[] = "5";
static const BYTE Tr0269[] = "10";
static const BYTE Tr0270[] = "15";
static const BYTE Tr0271[] = "20";
static const BYTE Tr0272[] = "25";
static const BYTE Tr0273[] = "30";
static const BYTE Tr0274[] = "с";
static const BYTE Tr0275[] = "s";
#define Tr0276 Empty
#define Tr0277 Empty
static const BYTE Tr0278[] = "m\015 = ";
static const BYTE Tr0279[] = "Уровень подсветки:";
static const BYTE Tr0280[] = "Light level:";
#define Tr0281 Empty
#define Tr0282 Empty
#define Tr0283 Empty
#define Tr0284 Empty
#define Tr0285 Empty
#define Tr0286 Empty
#define Tr0287 Empty
#define Tr0288 Empty
#define Tr0289 Empty
#define Tr0290 Empty
#define Tr0291 Empty
#define Tr0292 Empty
static const BYTE Tr0293[] = "ФНЧ:";
static const BYTE Tr0294[] = "LPF:";
static const BYTE Tr0295[] = "Kус:";
static const BYTE Tr0296[] = "Kamp:";
static const BYTE Tr0297[] = "Парам:";
static const BYTE Tr0298[] = "Param:";
#define Tr0299 Empty
#define Tr0300 Empty
static const BYTE Tr0301[] = "Уровень сигнала";
static const BYTE Tr0302[] = "Signal level";
static const BYTE Tr0303[] = "S";
static const BYTE Tr0304[] = "V";
static const BYTE Tr0305[] = "A";
#define Tr0306 Empty
static const BYTE Tr0307[] = "Нет данных";
static const BYTE Tr0308[] = "No data";
static const BYTE Tr0309[] = "A0 = ";
static const BYTE Tr0310[] = "A1 = ";
static const BYTE Tr0311[] = "A2 = ";
static const BYTE Tr0312[] = "Амплитудное значение";
static const BYTE Tr0313[] = "Amplitude";
static const BYTE Tr0314[] = "Размах";
static const BYTE Tr0315[] = "Peak-to-peak";
static const BYTE Tr0316[] = "Среднеквадратичное значение";
static const BYTE Tr0317[] = "RMS";
static const BYTE Tr0318[] = "Версия ПО: ";
static const BYTE Tr0319[] = "Soft version: ";
#define Tr0320 Empty
#define Tr0321 Empty
#define Tr0322 Empty
#define Tr0323 Empty
#define Tr0324 Empty
#define Tr0325 Empty
#define Tr0326 Empty
#define Tr0327 Empty
#define Tr0328 Empty
#define Tr0329 Empty
#define Tr0330 Empty
#define Tr0331 Empty
static const BYTE Tr0332[] = "мм";
static const BYTE Tr0333[] = "mm";
static const BYTE Tr0334[] = "кН";
static const BYTE Tr0335[] = "kN";
static const BYTE Tr0336[] = "кгс";
static const BYTE Tr0337[] = "kgf";
static const BYTE Tr0338[] = "Ua =";
#define Tr0339 Tr0338
static const BYTE Tr0340[] = "Uд =";
static const BYTE Tr0341[] = "Us =";
#define Tr0342 Empty
#define Tr0343 Empty
#define Tr0344 Empty
#define Tr0345 Empty
static const BYTE Tr0346[] = ",";
#define Tr0347 Empty
#define Tr0348 Empty
#define Tr0349 Empty
#define Tr0350 Empty
#define Tr0351 Empty
#define Tr0352 Empty
#define Tr0353 Empty
#define Tr0354 Empty
#define Tr0355 Empty
#define Tr0356 Empty
static const BYTE Tr0357[] = "F=?";
static const BYTE Tr0358[] = "F=";
static const BYTE Tr0359[] = "S=?";
static const BYTE Tr0360[] = "S=";
#define Tr0361 Empty
#define Tr0362 Empty
#define Tr0363 Empty
#define Tr0364 Empty
static const BYTE Tr0365[] = "Параметры";
static const BYTE Tr0366[] = "Parameters";
static const BYTE Tr0367[] = "изменены!";
static const BYTE Tr0368[] = "was changed!";
static const BYTE Tr0369[] = "Продолжить?";
static const BYTE Tr0370[] = "Continue?";
#define Tr0371 Empty
#define Tr0372 Empty
static const BYTE Tr0373[] = " МПа";
static const BYTE Tr0374[] = " MPa";
static const BYTE Tr0375[] = "V=?";
static const BYTE Tr0376[] = "Коэф.датчика S";
static const BYTE Tr0377[] = "Sensor factors S";
#define Tr0378 Empty
#define Tr0379 Empty
#define Tr0380 Empty
#define Tr0381 Empty
#define Tr0382 Empty
#define Tr0383 Empty
#define Tr0384 Empty
#define Tr0385 Empty
#define Tr0386 Empty
#define Tr0387 Empty
#define Tr0388 Empty
#define Tr0389 Empty
#define Tr0390 Empty
#define Tr0391 Empty
#define Tr0392 Empty
#define Tr0393 Empty
#define Tr0394 Empty
#define Tr0395 Empty
#define Tr0396 Empty
#define Tr0397 Empty
#define Tr0398 Empty
#define Tr0399 Empty
static const BYTE Tr0400[] = "кН";
static const BYTE Tr0401[] = "kN";
static const BYTE Tr0402[] = "кгс";
static const BYTE Tr0403[] = "kgf";
static const BYTE Tr0404[] = "Н";
static const BYTE Tr0405[] = "N";
static const BYTE Tr0406[] = "N =  ";
static const BYTE Tr0407[] = "N =  ";
#define Tr0408 Empty
#define Tr0409 Empty
#define Tr0410 Empty
#define Tr0411 Empty
#define Tr0412 Empty
#define Tr0413 Empty
#define Tr0414 Empty
#define Tr0415 Empty
#define Tr0416 Empty
#define Tr0417 Empty
#define Tr0418 Empty
#define Tr0419 Empty
#define Tr0420 Empty
#define Tr0421 Empty
#define Tr0422 Empty
#define Tr0423 Empty
static const BYTE Tr0424[] = "Параметр";
static const BYTE Tr0425[] = "Parameter";
static const BYTE Tr0426[] = "Удалить?";
static const BYTE Tr0427[] = "Clear?";
#define Tr0428 Empty
#define Tr0429 Empty
static const BYTE Tr0430[] = "d = ";
static const BYTE Tr0431[] = "d = ";
static const BYTE Tr0432[] = "Объект";
static const BYTE Tr0433[] = "Object";
static const BYTE Tr0434[] = "О производителе";
static const BYTE Tr0435[] = "Manufacturer";
static const BYTE Tr0436[] = "Разработчику";
static const BYTE Tr0437[] = "To developer";
static const BYTE Tr0438[] = "Разработчику";
static const BYTE Tr0439[] = "To developer";
static const BYTE Tr0440[] = "<ПРОИЗВОДИТЕЛЬ>";
static const BYTE Tr0441[] = "<MANUFACTURER>";
static const BYTE Tr0442[] = "www.<manufacturer>.com";
static const BYTE Tr0443[] = "Сайт:";
static const BYTE Tr0444[] = "Website:";
#define Tr0445 Empty
#define Tr0446 Empty
#define Tr0447 Empty
#define Tr0448 Empty
#define Tr0449 Empty
#define Tr0450 Empty
#define Tr0451 Empty
#define Tr0452 Empty
#define Tr0453 Empty
#define Tr0454 Empty
#define Tr0455 Empty
#define Tr0456 Empty
#define Tr0457 Empty
#define Tr0458 Empty
#define Tr0459 Empty
#define Tr0460 Empty
static const BYTE Tr0461[] = "Мод: ";
static const BYTE Tr0462[] = "Mod: ";
static const BYTE Tr0463[] = "Разработано:";
static const BYTE Tr0464[] = "Developed:";
#define Tr0465 Empty
#define Tr0466 Empty
static const BYTE Tr0467[] = "Дамп флэш";
static const BYTE Tr0468[] = "Flash dump";
static const BYTE Tr0469[] = "Адрес";
static const BYTE Tr0470[] = "Address";
static const BYTE Tr0471[] = "Адрес";
static const BYTE Tr0472[] = "Address";
static const BYTE Tr0473[] = "Проверка АЦП";
static const BYTE Tr0474[] = "ADC test";
#define Tr0475 Empty
#define Tr0476 Empty
static const BYTE Tr0477[] = "КОД = %5i";
static const BYTE Tr0478[] = "CODE = %5i";
static const BYTE Tr0479[] = "U = %7.3f мВ";
static const BYTE Tr0480[] = "U = %7.3f mV";
static const BYTE Tr0481[] = "m\216:";
static const BYTE Tr0482[] = "Калибров. коэф.:";
static const BYTE Tr0483[] = "Calib. factors:";
static const BYTE Tr0484[] = "Сохранение...";
static const BYTE Tr0485[] = "Saving...";
static const BYTE Tr0486[] = "Начальные уст.";
static const BYTE Tr0487[] = "Init.settings";
static const BYTE Tr0488[] = "Установить?";
static const BYTE Tr0489[] = "Set?";
static const BYTE Tr0490[] = "Калибровка АЦП";
static const BYTE Tr0491[] = "ADC calibrat.";
#define Tr0492 Empty
#define Tr0493 Empty
static const BYTE Tr0494[] = "мВ";
static const BYTE Tr0495[] = "mV";
static const BYTE Tr0496[] = "Uf = ";
static const BYTE Tr0497[] = "Us = ";
static const BYTE Tr0498[] = "%7.3f мВ";
static const BYTE Tr0499[] = "%7.3f mV";
static const BYTE Tr0500[] = "Контрастность";
static const BYTE Tr0501[] = "Contrast";
#define Tr0502 Empty
#define Tr0503 Empty
#define Tr0504 Empty
#define Tr0505 Empty
#define Tr0506 Empty
#define Tr0507 Empty
#define Tr0508 Empty
#define Tr0509 Empty
#define Tr0510 Empty
#define Tr0511 Empty
static const BYTE Tr0512[] = "Наладка F";
static const BYTE Tr0513[] = "Tunes F";
static const BYTE Tr0514[] = "Параметры";
static const BYTE Tr0515[] = "Parameters";
static const BYTE Tr0516[] = "Параметры ";
static const BYTE Tr0517[] = "Paramet. ";
static const BYTE Tr0518[] = "\00220мм";
static const BYTE Tr0519[] = "\00220mm";
static const BYTE Tr0520[] = ">20мм \00230мм";
static const BYTE Tr0521[] = ">20mm \00230mm";
static const BYTE Tr0522[] = ">30мм \00240мм";
static const BYTE Tr0523[] = ">30mm \00240mm";
#define Tr0524 Tr0055
#define Tr0525 Tr0056
#define Tr0526 Tr0055
#define Tr0527 Tr0056
static const BYTE Tr0528[] = "Общий";
static const BYTE Tr0529[] = "General";
static const BYTE Tr0530[] = "Виброплощадка";
static const BYTE Tr0531[] = "Shaker table";
static const BYTE Tr0532[] = "Sскз";
static const BYTE Tr0533[] = "Srms";
static const BYTE Tr0534[] = "Vскз";
static const BYTE Tr0535[] = "Vrms";
static const BYTE Tr0536[] = "Aскз";
static const BYTE Tr0537[] = "Arms";
static const BYTE Tr0538[] = "m  =";
static const BYTE Tr0539[] = "кН/с";
static const BYTE Tr0540[] = "kN/s";
static const BYTE Tr0541[] = "кгс/с";
static const BYTE Tr0542[] = "kgf/s";
static const BYTE Tr0543[] = "Наладка S";
static const BYTE Tr0544[] = "Tunes S";
static const BYTE Tr0545[] = "Sамп";
static const BYTE Tr0546[] = "Samp";
static const BYTE Tr0547[] = "Vамп";
static const BYTE Tr0548[] = "Vamp";
static const BYTE Tr0549[] = "Aамп";
static const BYTE Tr0550[] = "Aamp";
static const BYTE Tr0551[] = "амп";
static const BYTE Tr0552[] = "amp";
static const BYTE Tr0553[] = "скз";
static const BYTE Tr0554[] = "rms";
static const BYTE Tr0555[] = "Sразмах";
static const BYTE Tr0556[] = "Spp";
#define Tr0557 Empty
#define Tr0558 Empty
#define Tr0559 Empty
#define Tr0560 Empty
#define Tr0561 Empty
#define Tr0562 Empty
#define Tr0563 Empty
#define Tr0564 Empty
#define Tr0565 Empty
#define Tr0566 Empty
#define Tr0567 Empty
#define Tr0568 Empty
#define Tr0569 Empty
#define Tr0570 Empty
#define Tr0571 Empty
#define Tr0572 Empty
#define Tr0573 Empty
#define Tr0574 Empty
#define Tr0575 Empty
#define Tr0576 Empty
#define Tr0577 Empty
#define Tr0578 Empty
static const BYTE Tr0579[] = "\245 =    мм";
static const BYTE Tr0580[] = "\245 =    mm";
#define Tr0581 Empty
#define Tr0582 Empty
#define Tr0583 Empty
#define Tr0584 Empty
#define Tr0585 Tr0055
#define Tr0586 Tr0056
#define Tr0587 Tr0055
#define Tr0588 Tr0056
#define Tr0589 Tr0057
#define Tr0590 Tr0058
#define Tr0591 Empty
#define Tr0592 Empty
#define Tr0593 Tr0059
#define Tr0594 Tr0060
#define Tr0595 Tr0059
#define Tr0596 Tr0060
#define Tr0597 Empty
#define Tr0598 Empty
#define Tr0599 Empty
#define Tr0600 Empty
#define Tr0601 Empty
#define Tr0602 Empty
#define Tr0603 Empty
#define Tr0604 Empty
#define Tr0605 Empty
#define Tr0606 Empty
static const BYTE Tr0607[] = "напряжения \010";
static const BYTE Tr0608[] = "tension \010";
static const BYTE Tr0609[] = "вибрации";
static const BYTE Tr0610[] = "vibration";
static const BYTE Tr0611[] = "Uf =";
#define Tr0612 Tr0611
static const BYTE Tr0613[] = "F =";
#define Tr0614 Tr0613
static const BYTE Tr0615[] = "S =";
#define Tr0616 Tr0615
static const BYTE Tr0617[] = "T =";
#define Tr0618 Tr0617
static const BYTE Tr0619[] = "КОДmin = %5i";
static const BYTE Tr0620[] = "CODEmin = %5i";
static const BYTE Tr0621[] = "КОДmax = %5i";
static const BYTE Tr0622[] = "CODEmax = %5i";
#define Tr0623 Empty
#define Tr0624 Empty
#define Tr0625 Empty
#define Tr0626 Empty
#define Tr0627 Empty
#define Tr0628 Empty
#define Tr0629 Empty
#define Tr0630 Empty
#define Tr0631 Empty
#define Tr0632 Empty
#define Tr0633 Empty
#define Tr0634 Empty
#define Tr0635 Empty
#define Tr0636 Empty
#define Tr0637 Empty
#define Tr0638 Empty
#define Tr0639 Empty
#define Tr0640 Empty
#define Tr0641 Empty
#define Tr0642 Empty
#define Tr0643 Empty
#define Tr0644 Empty
#define Tr0645 Empty
#define Tr0646 Empty
static const BYTE Tr0647[] = "Поверка";
static const BYTE Tr0648[] = "Verification";
static const BYTE Tr0649[] = "Длина,м:";
static const BYTE Tr0650[] = "Length,m:";
static const BYTE Tr0651[] = "Диаметр,мм:";
static const BYTE Tr0652[] = "Diameter,mm:";
static const BYTE Tr0653[] = "Объект:";
static const BYTE Tr0654[] = "Object:";
static const BYTE Tr0655[] = "F =";
static const BYTE Tr0656[] = "T =";
static const BYTE Tr0657[] = "Гц";
static const BYTE Tr0658[] = "Hz";
static const BYTE Tr0659[] = "млс";
static const BYTE Tr0660[] = "mls";
static const BYTE Tr0661[] = "S = ";
static const BYTE Tr0662[] = "V = ";
static const BYTE Tr0663[] = "A = ";
static const BYTE Tr0664[] = "";
static const BYTE Tr0665[] = "S =";
static const BYTE Tr0666[] = "V =";
static const BYTE Tr0667[] = "A =";
#define Tr0668 Empty
#define Tr0669 Empty
#define Tr0670 Empty
#define Tr0671 Empty
#define Tr0672 Empty
static const BYTE Tr0673[] = "мм";
static const BYTE Tr0674[] = "mm";
static const BYTE Tr0675[] = "П = ";
static const BYTE Tr0676[] = "N = ";
static const BYTE Tr0677[] = "П =";
static const BYTE Tr0678[] = "N =";
static const BYTE Tr0679[] = "U =";
#define Tr0680 Tr0679
#define Tr0681 Empty
#define Tr0682 Empty
#define Tr0683 Empty
#define Tr0684 Empty
static const BYTE Tr0685[] = "Сброс";
static const BYTE Tr0686[] = "Reset";
static const BYTE Tr0687[] = "Мин =";
static const BYTE Tr0688[] = "Min =";
static const BYTE Tr0689[] = "Mакс =";
static const BYTE Tr0690[] = "Max =";
static const BYTE Tr0691[] = "\011L = ";
#define Tr0692 Tr0691
static const BYTE Tr0693[] = "\006 = ";
#define Tr0694 Tr0693
#define Tr0695 Empty
#define Tr0696 Empty
#define Tr0697 Empty
#define Tr0698 Empty
#define Tr0699 Empty
#define Tr0700 Empty
#define Tr0701 Empty
#define Tr0702 Empty
#define Tr0703 Empty
#define Tr0704 Empty
#define Tr0705 Empty
#define Tr0706 Empty
#define Tr0707 Empty
#define Tr0708 Empty
#define Tr0709 Empty
#define Tr0710 Empty
static const BYTE Tr0711[] = "мм";
static const BYTE Tr0712[] = "mm";
static const BYTE Tr0713[] = "мм/с";
static const BYTE Tr0714[] = "mm/s";
static const BYTE Tr0715[] = "Архив";
static const BYTE Tr0716[] = "Archive";
static const BYTE Tr0717[] = "м/с\026";
static const BYTE Tr0718[] = "m/s\026";
#define Tr0719 Empty
#define Tr0720 Empty
#define Tr0721 Empty
#define Tr0722 Empty
#define Tr0723 Empty
#define Tr0724 Empty
#define Tr0725 Empty
#define Tr0726 Empty
#define Tr0727 Empty
#define Tr0728 Empty
static const BYTE Tr0729[] = "ИЗМЕРЕНО";
static const BYTE Tr0730[] = "DONE";
#define Tr0731 Empty
#define Tr0732 Empty
#define Tr0733 Empty
#define Tr0734 Empty
#define Tr0735 Empty
#define Tr0736 Empty
#define Tr0737 Empty
#define Tr0738 Empty
#define Tr0739 Empty
#define Tr0740 Empty
#define Tr0741 Empty
#define Tr0742 Empty
#define Tr0743 Empty
#define Tr0744 Empty
static const BYTE Tr0745[] = "кгс/с";
static const BYTE Tr0746[] = "kgf/s";
#define Tr0747 Empty
#define Tr0748 Empty
#define Tr0749 Empty
#define Tr0750 Empty
#define Tr0751 Empty
#define Tr0752 Empty
#define Tr0753 Empty
#define Tr0754 Empty
#define Tr0755 Empty
#define Tr0756 Empty
#define Tr0757 Empty
#define Tr0758 Empty
static const BYTE Tr0759[] = "МПа/c";
static const BYTE Tr0760[] = "MPa/s";
static const BYTE Tr0761[] = "(кгс/м\026)/с";
static const BYTE Tr0762[] = "(kgf/m\026)/s";
#define Tr0763 Empty
#define Tr0764 Empty
#define Tr0765 Empty
#define Tr0766 Empty
static const BYTE Tr0767[] = "Калибровка";
static const BYTE Tr0768[] = "Calibration of";
static const BYTE Tr0769[] = "перемещения S:";
static const BYTE Tr0770[] = "movement S:";
static const BYTE Tr0771[] = "'С' - Сброс \"0\"";
static const BYTE Tr0772[] = "'C' - Reset \"0\"";
static const BYTE Tr0773[] = "Удаление";
static const BYTE Tr0774[] = "Delete";
static const BYTE Tr0775[] = "ИНК";
static const BYTE Tr0776[] = "INC";
static const BYTE Tr0777[] = "ВИСТ";
static const BYTE Tr0778[] = "VIST";
#define Tr0779 Empty
#define Tr0780 Empty
#define Tr0781 Empty
#define Tr0782 Empty
static const BYTE Tr0783[] = "Н";
static const BYTE Tr0784[] = "N";
#define Tr0785 Empty
#define Tr0786 Empty
static const BYTE Tr0787[] = "Тип прибора";
static const BYTE Tr0788[] = "Device type";
static const BYTE Tr0789[] = "Предел.перемещение";
static const BYTE Tr0790[] = "Limiting movement";
static const BYTE Tr0791[] = "ИНК-ВИСТ-3.0";
static const BYTE Tr0792[] = "INC-VIST-3.0";
static const BYTE Tr0793[] = "ИНК-3.0";
static const BYTE Tr0794[] = "INC-3.0";
static const BYTE Tr0795[] = "ВИСТ-3.0";
static const BYTE Tr0796[] = "VIST-3.0";
#define Tr0797 Empty
#define Tr0798 Empty
static const BYTE Tr0799[] = "ИОН";
static const BYTE Tr0800[] = "RSS";
static const BYTE Tr0801[] = "Uион =";
static const BYTE Tr0802[] = "Urss =";
static const BYTE Tr0803[] = "Проверка";
static const BYTE Tr0804[] = "Test";
#define Tr0805 Empty
#define Tr0806 Empty
#define Tr0807 Empty
#define Tr0808 Empty
#define Tr0809 Empty
#define Tr0810 Empty
#define Tr0811 Empty
#define Tr0812 Empty
static const BYTE Tr0813[] = "Датчик 1:";
static const BYTE Tr0814[] = "Sensor 1:";
static const BYTE Tr0815[] = "Датчик 2:";
static const BYTE Tr0816[] = "Sensor 2:";
static const BYTE Tr0817[] = "ДА";
static const BYTE Tr0818[] = "YES";
static const BYTE Tr0819[] = "НЕТ";
static const BYTE Tr0820[] = "NO";
static const BYTE Tr0821[] = "'C'-Отмена";
static const BYTE Tr0822[] = "'C'-Cancel";
#define Tr0823 Empty
#define Tr0824 Empty
#define Tr0825 Empty
#define Tr0826 Empty
static const BYTE Tr0827[] = "F\214 =   .   кН";
static const BYTE Tr0828[] = "F\214 =   .   kN";
static const BYTE Tr0829[] = "V\012 = ";
#define Tr0830 Empty
static const BYTE Tr0831[] = "Tр = ";
static const BYTE Tr0832[] = "Tr = ";
static const BYTE Tr0833[] = "Kр = ";
static const BYTE Tr0834[] = "Kr = ";
static const BYTE Tr0835[] = "Tс =  .  с";
static const BYTE Tr0836[] = "Ts =  .  s";
static const BYTE Tr0837[] = "Нет памяти";
static const BYTE Tr0838[] = "No memory";
static const BYTE Tr0839[] = "Нет календаря";
static const BYTE Tr0840[] = "No calendar";
static const BYTE Tr0841[] = "Диагностика";
static const BYTE Tr0842[] = "Checks";
#define Tr0843 Empty
#define Tr0844 Empty
#define Tr0845 Empty
#define Tr0846 Empty
#define Tr0847 Empty
#define Tr0848 Empty
#define Tr0849 Empty
#define Tr0850 Empty
#define Tr0851 Empty
#define Tr0852 Empty
#define Tr0853 Empty
#define Tr0854 Empty
static const BYTE Tr0855[] = "%";
#define Tr0856 Tr0855
static const BYTE Tr0857[] = "K =";
static const BYTE Tr0858[] = "Виброперемещ.";
static const BYTE Tr0859[] = "Vibromotion";
static const BYTE Tr0860[] = "Виброскорость";
static const BYTE Tr0861[] = "Vibrovelocity";
static const BYTE Tr0862[] = "Виброускорение";
static const BYTE Tr0863[] = "Vibroacceleration";
#define Tr0864 Empty
#define Tr0865 Empty
#define Tr0866 Empty
#define Tr0867 Empty
#define Tr0868 Empty
static const BYTE Tr0869[] = "Источник";
static const BYTE Tr0870[] = "Reference";
static const BYTE Tr0871[] = "опорного";
static const BYTE Tr0872[] = "supply source:";
static const BYTE Tr0873[] = "напряжения:";
#define Tr0874  Empty
static const BYTE Tr0875[] = "Далее";
static const BYTE Tr0876[] = "Next";
static const BYTE Tr0877[] = "Назад";
static const BYTE Tr0878[] = "Back";
#define Tr0879 Empty
#define Tr0880 Empty
#define Tr0881 Empty
#define Tr0882 Empty
#define Tr0883 Empty
static const BYTE Tr0884[] = "K =";
#define Tr0885 Tr0884
static const BYTE Tr0886[] = "Ф =";
static const BYTE Tr0887[] = "F =";
static const BYTE Tr0888[] = "Uпп =";
static const BYTE Tr0889[] = "Upp =";
static const BYTE Tr0890[] = " В";
static const BYTE Tr0891[] = " V";
static const BYTE Tr0892[] = "A0s =";
#define Tr0893 Tr0892
static const BYTE Tr0894[] = "A0v =";
#define Tr0895 Tr0894
static const BYTE Tr0896[] = "A0a =";
#define Tr0897 Tr0896
static const BYTE Tr0898[] = "s";
static const BYTE Tr0899[] = "Есть";
static const BYTE Tr0900[] = "Yes";
static const BYTE Tr0901[] = "Нет";
static const BYTE Tr0902[] = "No";
static const BYTE Tr0903[] = "Вкл";
static const BYTE Tr0904[] = "On";
static const BYTE Tr0905[] = "Выкл";
static const BYTE Tr0906[] = "Off";
static const BYTE Tr0907[] = "Звук:";
static const BYTE Tr0908[] = "Sound:";
static const BYTE Tr0909[] = "Подождите...";
static const BYTE Tr0910[] = "Please wait...";
#define Tr0911 Empty
#define Tr0912 Empty
static const BYTE Tr0913[] = "Пуск";
static const BYTE Tr0914[] = "Start";
#define Tr0915 Empty
#define Tr0916 Empty
#define Tr0917 Empty
#define Tr0918 Empty
static const BYTE Tr0919[] = "Звук клавиш";
static const BYTE Tr0920[] = "Keyboard sound";
#define Tr0921 Empty
#define Tr0922 Empty
static const BYTE Tr0923[] = "Fs =";
#define Tr0924 Tr0923
static const BYTE Tr0925[] = "В";
static const BYTE Tr0926[] = "V";
#define Tr0927 Empty
#define Tr0928 Empty
#define Tr0929 Empty
#define Tr0930 Empty
static const BYTE Tr0931[] = "A1s =";
#define Tr0932 Tr0931
static const BYTE Tr0933[] = "A1v =";
#define Tr0934 Tr0933
static const BYTE Tr0935[] = "A1a =";
#define Tr0936 Tr0935
static const BYTE Tr0937[] = "Виброперемещение";
static const BYTE Tr0938[] = "Vibromotion";
#define Tr0939 Empty
#define Tr0940 Empty
#define Tr0941 Empty
#define Tr0942 Empty
#define Tr0943 Empty
#define Tr0944 Empty
static const BYTE Tr0945[] = "Откл";
static const BYTE Tr0946[] = "Off";
static const BYTE Tr0947[] = "Изм.";
static const BYTE Tr0948[] = "Meas.";
static const BYTE Tr0949[] = "Дни";
static const BYTE Tr0950[] = "Days";
#define Tr0951 Empty
#define Tr0952 Empty
static const BYTE Tr0953[] = "Изменить";
static const BYTE Tr0954[] = "Edit";
static const BYTE Tr0955[] = "Отмена";
static const BYTE Tr0956[] = "Cancel";
static const BYTE Tr0957[] = "'C'-Стоп/Продолжить";
static const BYTE Tr0958[] = "'C'-Stop/Resume";
static const BYTE Tr0959[] = "Возврат";
static const BYTE Tr0960[] = "Return";
static const BYTE Tr0961[] = "кН/с";
static const BYTE Tr0962[] = "kN/s";
static const BYTE Tr0963[] = "Н/с";
static const BYTE Tr0964[] = "N/s";
static const BYTE Tr0965[] = "МПа/с";
static const BYTE Tr0966[] = "MPa/s";
static const BYTE Tr0967[] = "Kп = ";
static const BYTE Tr0968[] = "Напряжение питания:";
static const BYTE Tr0969[] = "Supply source:";
static const BYTE Tr0970[] = "Перемещение S";
static const BYTE Tr0971[] = "Movement S";
#define Tr0972 Empty
#define Tr0973 Empty
static const BYTE Tr0974[] = "Тема";
static const BYTE Tr0975[] = "Theme";
static const BYTE Tr0976[] = "Светлая";
static const BYTE Tr0977[] = "Light";
static const BYTE Tr0978[] = "Темная";
static const BYTE Tr0979[] = "Dark";
static const BYTE Tr0980[] = "Простая";
static const BYTE Tr0981[] = "Simple";
#define Tr0982 Empty
#define Tr0983 Empty
#define Tr0984 Empty
#define Tr0985 Empty
#define Tr0986 Empty
#define Tr0987 Empty
#define Tr0988 Empty
#define Tr0989 Empty
#define Tr0990 Empty
#define Tr0991 Empty
#define Tr0992 Empty
#define Tr0993 Empty
static const BYTE Tr0994[] = "Калибровка";
static const BYTE Tr0995[] = "Calibration";
#define Tr0996 Empty
#define Tr0997 Empty
#define Tr0998 Empty
#define Tr0999 Empty
#define Tr1000 Empty
#define Tr1001 Empty
#define Tr1002 Empty
#define Tr1003 Empty
#define Tr1004 Empty
#define Tr1005 Empty
#define Tr1006 Empty
#define Tr1007 Empty
static const BYTE Tr1008[] = "Нет акселерометра";
static const BYTE Tr1009[] = "No accelerometer";
#define Tr1010 Empty
#define Tr1011 Empty
static const BYTE Tr1012[] = "Канатная";
static const BYTE Tr1013[] = "Wire rope";
static const BYTE Tr1014[] = "Арматура-1";
static const BYTE Tr1015[] = "Reinforcement-1";
static const BYTE Tr1016[] = "Арматура-2";
static const BYTE Tr1017[] = "Reinforcement-2";
static const BYTE Tr1018[] = "Тип";
static const BYTE Tr1019[] = "Type";
static const BYTE Tr1020[] = "\010";
#define Tr1021 Tr1020

static const BYTE* TextResourcesRef[] = {
#include "TextResources.inc"
};

#define TEXTRESOURCES_COUNT    ARRAY_LENGTH(TextResourcesRef)

//---------------------------------------------------------
BYTE const* GetTextResource(WORD id)
{
  return id < TEXTRESOURCES_COUNT ? TextResourcesRef[id] : Empty;
}

//---------------------------------------------------------
BYTE const* GetLangResource(WORD id)
{
  return GetTextResource(id + LangId);
}

//---------------------------------------------------------
BYTE LoadTextResource(BYTE* buff, WORD id)
{
  BYTE const* src = GetTextResource(id);
  BYTE* dst = buff;
  BYTE b;
  while((b = *src++)) {
    *dst++ = b;
  }
  return (BYTE)(dst - buff);
}

//---------------------------------------------------------
BYTE LoadLangResource(BYTE* buff, WORD id)
{
  return LoadTextResource(buff, id + LangId);
}
