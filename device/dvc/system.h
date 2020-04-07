#ifndef SYSTEM_H_INCLUDED
#define SYSTEM_H_INCLUDED

/*---------------------------------------------------------
  system.h
---------------------------------------------------------*/

#include "xmain.h"

void PrepareSystem(void);
void InitSystem(void);

void EnableInterrupts(void);
void DisableInterrupts(void);
BOOL IsInterruptsEnabled(void);
DWORD GetTicks(void);
void DoTicks(void);
void Halt(void);
void SetFastFrequency(void);
void SetNormalFrequency(void);


#ifndef WIN32

#define SYS_K_T                       (84.0f/84.0f)
#define SYS_CONVERT_MCS(x)            ((DWORD)(x*SYS_K_T))

#define GPIO_MASK_PIN_0        (3U << (0U*2U))
#define GPIO_MASK_PIN_1        (3U << (1U*2U))
#define GPIO_MASK_PIN_2        (3U << (2U*2U))
#define GPIO_MASK_PIN_3        (3U << (3U*2U))
#define GPIO_MASK_PIN_4        (3U << (4U*2U))
#define GPIO_MASK_PIN_5        (3U << (5U*2U))
#define GPIO_MASK_PIN_6        (3U << (6U*2U))
#define GPIO_MASK_PIN_7        (3U << (7U*2U))
#define GPIO_MASK_PIN_8        (3U << (8U*2U))
#define GPIO_MASK_PIN_9        (3U << (9U*2U))
#define GPIO_MASK_PIN_10       (3U << (10U*2U))
#define GPIO_MASK_PIN_11       (3U << (11U*2U))
#define GPIO_MASK_PIN_12       (3U << (12U*2U))
#define GPIO_MASK_PIN_13       (3U << (13U*2U))
#define GPIO_MASK_PIN_14       (3U << (14U*2U))
#define GPIO_MASK_PIN_15       (3U << (15U*2U))

#define GPIO_IN_PIN_0          (0U << (0U*2U))
#define GPIO_IN_PIN_1          (0U << (1U*2U))
#define GPIO_IN_PIN_2          (0U << (2U*2U))  
#define GPIO_IN_PIN_3          (0U << (3U*2U))  
#define GPIO_IN_PIN_4          (0U << (4U*2U))  
#define GPIO_IN_PIN_5          (0U << (5U*2U))  
#define GPIO_IN_PIN_6          (0U << (6U*2U))  
#define GPIO_IN_PIN_7          (0U << (7U*2U))  
#define GPIO_IN_PIN_8          (0U << (8U*2U))  
#define GPIO_IN_PIN_9          (0U << (9U*2U))  
#define GPIO_IN_PIN_10         (0U << (10U*2U))  
#define GPIO_IN_PIN_11         (0U << (11U*2U))  
#define GPIO_IN_PIN_12         (0U << (12U*2U))  
#define GPIO_IN_PIN_13         (0U << (13U*2U))  
#define GPIO_IN_PIN_14         (0U << (14U*2U))  
#define GPIO_IN_PIN_15         (0U << (15U*2U))  

#define GPIO_OUT_PIN_0         (1U << (0U*2U))
#define GPIO_OUT_PIN_1         (1U << (1U*2U))
#define GPIO_OUT_PIN_2         (1U << (2U*2U))
#define GPIO_OUT_PIN_3         (1U << (3U*2U))
#define GPIO_OUT_PIN_4         (1U << (4U*2U))
#define GPIO_OUT_PIN_5         (1U << (5U*2U))
#define GPIO_OUT_PIN_6         (1U << (6U*2U))
#define GPIO_OUT_PIN_7         (1U << (7U*2U))
#define GPIO_OUT_PIN_8         (1U << (8U*2U))
#define GPIO_OUT_PIN_9         (1U << (9U*2U))
#define GPIO_OUT_PIN_10        (1U << (10U*2U))
#define GPIO_OUT_PIN_11        (1U << (11U*2U))
#define GPIO_OUT_PIN_12        (1U << (12U*2U))
#define GPIO_OUT_PIN_13        (1U << (13U*2U))
#define GPIO_OUT_PIN_14        (1U << (14U*2U))
#define GPIO_OUT_PIN_15        (1U << (15U*2U))

#define GPIO_ALT_PIN_0         (2U << (0U*2U))
#define GPIO_ALT_PIN_1         (2U << (1U*2U))
#define GPIO_ALT_PIN_2         (2U << (2U*2U))
#define GPIO_ALT_PIN_3         (2U << (3U*2U))
#define GPIO_ALD_PIN_4         (2U << (4U*2U))
#define GPIO_ALT_PIN_5         (2U << (5U*2U))
#define GPIO_ALT_PIN_6         (2U << (6U*2U))
#define GPIO_ALT_PIN_7         (2U << (7U*2U))
#define GPIO_ALT_PIN_8         (2U << (8U*2U))
#define GPIO_ALT_PIN_9         (2U << (9U*2U))
#define GPIO_ALT_PIN_10        (2U << (10U*2U))
#define GPIO_ALT_PIN_11        (2U << (11U*2U))
#define GPIO_ALT_PIN_12        (2U << (12U*2U))
#define GPIO_ALT_PIN_13        (2U << (13U*2U))
#define GPIO_ALT_PIN_14        (2U << (14U*2U))
#define GPIO_ALT_PIN_15        (2U << (15U*2U))

#define GPIO_ADC_PIN_0         (3U << (0U*2U))
#define GPIO_ADC_PIN_1         (3U << (1U*2U))
#define GPIO_ADC_PIN_2         (3U << (2U*2U))
#define GPIO_ADC_PIN_3         (3U << (3U*2U))
#define GPIO_ADC_PIN_4         (3U << (4U*2U))
#define GPIO_ADC_PIN_5         (3U << (5U*2U))
#define GPIO_ADC_PIN_6         (3U << (6U*2U))
#define GPIO_ADC_PIN_7         (3U << (7U*2U))
#define GPIO_ADC_PIN_8         (3U << (8U*2U))
#define GPIO_ADC_PIN_9         (3U << (9U*2U))
#define GPIO_ADC_PIN_10        (3U << (10U*2U))
#define GPIO_ADC_PIN_11        (3U << (11U*2U))
#define GPIO_ADC_PIN_12        (3U << (12U*2U))
#define GPIO_ADC_PIN_13        (3U << (13U*2U))
#define GPIO_ADC_PIN_14        (3U << (14U*2U))
#define GPIO_ADC_PIN_15        (3U << (15U*2U))

#  define GPIO_SET_MODE(port, mask, bits) \
 port->MODER = (port->MODER & ~(mask)) | (bits)
//#  define PIO_ENABLE_IN_FILTER(x)         __PIO_IFER = x
//#  define PIO_DISABLE_IN_FILTER(x)        __PIO_IFDR = x
#  define GPIO_SET1(port, x)               port->BSRR = x
#  define GPIO_SET_OUT_HIGH(port, x)       port->BSRR = x
#  define GPIO_SET_OUT_LOW(port, x)        port->BSRR = (DWORD)x << 16U
#  define GPIO_IS_IN_HIGH(port, x)         ((port->IDR) & (x))
#  define GPIO_IS_IN_LOW(port, x)          (((port->IDR) & (x)) == 0)
//#  define GPIO_ENABLE_INTERRUPT(x)         __PIO_IER = x
//#  define GPIO_DISABLE_INTERRUPT(x)        __PIO_IDR = x
//#  define GPIO_IS_INTERRUPT_PENDING(x)     (__PIO_ISR & (x))
//#  define GPIO_ENABLE_OPEN_DRAIN(x)        __PIO_MDER = x
//#  define GPIO_DISABLE_OPEN_DRAIN(x)       __PIO_MDDR = x
//#  define GPIO_ENABLE_PULLUP(x)            __PIO_PUER = x
//#  define GPIO_DISABLE_PULLUP(x)           __PIO_PUDR = x
//#  define GPIO_ENABLE_PERIPHERAL_A(x)      __PIO_ASR = x
//#  define GPIO_ENABLE_PERIPHERAL_B(x)      __PIO_BSR = x
#  define GPIO_GET(port)                    (port->IDR)

#else

#define SYS_K_T                       (1.0/1000.0)
#define SYS_CONVERT_MCS(x)            ((DWORD)(x*SYS_K_T))

#endif

#endif
