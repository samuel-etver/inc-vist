/*---------------------------------------------------------
  usb_dvc.c
---------------------------------------------------------*/

#include "usb.h"
#include "usb_pd.h"
#include "system.h"
#include "language.h"
#include "flash.h"
#include "mem.h"
#include "autooff.h"
#include "delay.h"
#include "lcd.h"
#include "cfg.h"
#include "DeviceType.h"

#define USB_CONNECTED_PORT                      GPIOA
#define USB_CONNECTED_BIT                       GPIO_PIN_9

#define USB_EP0_MAX_PACKET_SIZE                 64
#define USB_EP1_MAX_PACKET_SIZE                 64
#define USB_EP2_MAX_PACKET_SIZE                 64
#define USB_EP3_MAX_PACKET_SIZE                 64

#define USB_EP1_FIFO_SIZE                       128

#define USB_STANDARD_DEVICE_REQUEST		0x00
#define USB_STANDARD_INTERFACE_REQUEST	        0x01
#define USB_STANDARD_ENDPOINT_REQUEST	        0x02
#define USB_VENDOR_DEVICE_REQUEST		0x40
#define USB_VENDOR_ENDPOINT_REQUEST		0x42

#define USB_ENDPOINT_TYPE_CONTROL	        0x00
#define USB_ENDPOINT_TYPE_ISOCHRONOUS	        0x01
#define USB_ENDPOINT_TYPE_BULK		        0x02
#define USB_ENDPOINT_TYPE_INTERRUPT	        0x03

#define USB_TYPE_DEVICE_DESCRIPTOR              0x01
#define USB_TYPE_CONFIGURATION_DESCRIPTOR       0x02
#define USB_TYPE_STRING_DESCRIPTOR              0x03
#define USB_TYPE_INTERFACE_DESCRIPTOR           0x04
#define USB_TYPE_ENDPOINT_DESCRIPTOR            0x05
#define USB_TYPE_HID_DESCRIPTOR		        0x21

#define USB_GET_STATUS  			0
#define USB_CLEAR_FEATURE     		        1
#define USB_SET_FEATURE                 	3
#define USB_SET_ADDRESS                 	5
#define USB_GET_DESCRIPTOR              	6
#define USB_SET_DESCRIPTOR              	7
#define USB_GET_CONFIGURATION           	8
#define USB_SET_CONFIGURATION           	9
#define USB_GET_INTERFACE               	10
#define USB_SET_INTERFACE               	11
#define USB_SYNCH_FRAME                 	12

#define USB_VENDOR_GET_PRODUCT_SPECIFICATION    1
#define USB_VENDOR_SET_BULK_SIZE                2
#define USB_VENDOR_START_BULK                   3
#define USB_VENDOR_GET_FLASH_CHANGED            10
#define USB_VENDOR_CLEAR_FLASH_CHANGED          11
#define USB_VENDOR_READ_FLASH                   12
#define USB_VENDOR_IS_CMD_DONE                  13
#define USB_VENDOR_RECOVER                      50
#define USB_VENDOR_GET_SCREEN_WIDTH             100
#define USB_VENDOR_GET_SCREEN_HEIGHT            101
#define USB_VENDOR_READ_SCREEN_DOT              102
#define USB_VENDOR_GET_SCREEN_DOT               104
#define USB_VENDOR_START_SCREEN_DOTS            110
#define USB_VENDOR_CLEAR_BULK                   120

#define	USB_ENDPOINT_HALT			0

#define USB_CMD_DO_NOTHING                      0
#define USB_CMD_READ_DOT                        1
#define USB_CMD_READ_DOTS                       2
#define USB_CMD_READ_FLASH                      10
#define USB_CMD_CLEAR_BULK                      20

#pragma pack(1)

typedef struct {
  BYTE bmRequestType;
  BYTE bRequest;
  WORD wValue;
  WORD wIndex;
  WORD wLength;
} TUsbSetupRequest;

typedef struct {
  BYTE bLength;
  BYTE bDescriptorType;
  WORD bcdUSB;
  BYTE bDeviceClass;
  BYTE bDeviceSubClass;
  BYTE bDeviceProtocol;
  BYTE bMaxPacketSize0;
  WORD idVendor;
  WORD idProduct;
  WORD bcdDevice;
  BYTE iManufacturer;
  BYTE iProduct;
  BYTE iSerialNumber;
  BYTE bNumConfigurations;
} TUsbDeviceDescriptor;

typedef struct {
  BYTE bLength;
  BYTE bDescriptorType;
  BYTE bEndpointAddress;
  BYTE bmAttributes;
  WORD wMaxPacketSize;
  BYTE bInterval;
} TUsbEndpointDescriptor;

typedef struct {
  BYTE bLength;
  BYTE bDescriptorType;
  WORD wTotalLength;
  BYTE bNumInterfaces;
  BYTE bConfigurationValue;
  BYTE iConfiguration;
  BYTE bmAttributes;
  BYTE bMaxPower;
} TUsbConfigurationDescriptor;

typedef struct {
  BYTE bLength;
  BYTE bDescriptorType;
  BYTE bInterfaceNumber;
  BYTE bAlternateSetting;
  BYTE bNumEndpoints;
  BYTE bInterfaceClass;
  BYTE bInterfaceSubClass;
  BYTE bInterfaceProtocol;
  BYTE iInterface;
} TUsbInterfaceDescriptor;

typedef struct {
  BYTE bLenght;
  BYTE bDescriptorType;
  WORD wLANGID;
} TUsbLangIdDescriptor;

static const TUsbLangIdDescriptor UsbLangIdDescriptor = {
  sizeof(TUsbLangIdDescriptor),
  USB_TYPE_STRING_DESCRIPTOR,
  0x0409
};

static const char UsbManufacturerDescriptor[] = {
  2 + 2*14,
  USB_TYPE_STRING_DESCRIPTOR,
  '<',0,'M',0,'A',0,'N',0,'U',0,'F',0,'A',0,'C',0,'T',0,'U',0,'R',0,'E',0,'R',0,'>',0
};

static const char UsbIncProductDescriptor[] = {
  2 + 2*7,
  USB_TYPE_STRING_DESCRIPTOR,
  'I',0,'N',0,'C',0,'-',0,'3',0,'.',0,'0',0
};

static const char UsbVistProductDescriptor[] = {
  2 + 2*9,
  USB_TYPE_STRING_DESCRIPTOR,
  'V',0,'I',0,'S',0,'T',0,'-',0,'3',0,'.',0,'0',0
};

static const char UsbIncVistProductDescriptor[] = {
  2 + 2*12,
  USB_TYPE_STRING_DESCRIPTOR,
  'I',0,'N',0,'C',0,'-',0,'V',0,'I',0,'S',0,'T',0,'-',0,'3',0,'.',0,'0',0
};

static const TUsbDeviceDescriptor UsbDeviceDescriptor = {
  sizeof(TUsbDeviceDescriptor),                 // bLength
  USB_TYPE_DEVICE_DESCRIPTOR,                   // bDescriptorType
  0x0110,                                       // bcdUSB
  0,                                            // bDeviceClass
  0,                                            // bDeviceSubclass
  0,                                            // bDeviceProtocol
  USB_EP0_MAX_PACKET_SIZE,                      // bMaxPacketSize
  0x0471,                                       // idVendor
  0x0002,                                       // idProduct
  0x0000,                                       // bcdDevice
  1,                                            // iManufacturer
  2,                                            // iProduct
  0,                                            // iSerialNumber
  1                                             // bNumberConfiguration
};

static const struct {
  TUsbConfigurationDescriptor ConfigDescriptor;
  TUsbInterfaceDescriptor InterfaceDescriptor;
  TUsbEndpointDescriptor EndpointDescriptor0;
  TUsbEndpointDescriptor EndpointDescriptor1;
  TUsbEndpointDescriptor EndpointDescriptor2;
} UsbConfigData = {
  { /* Configuration Descriptor */
    sizeof(TUsbConfigurationDescriptor),        // bLength
    USB_TYPE_CONFIGURATION_DESCRIPTOR,          // bDescriptorType
    sizeof(UsbConfigData),                      // wTotalLength
    1,                                          // bNumInterfaces
    1,                                          // bConfigurationValue
    0,                                          // iConfiguration
    0x80,                                       // bmAttributes Bus Powered,
                                                // No Remote Wakeup
    0xC8                                        // bMaxPower, 400mA
  },
  { /* Interface Descriptor */
    sizeof(TUsbInterfaceDescriptor),            // bLength
    USB_TYPE_INTERFACE_DESCRIPTOR,              // bDescriptorType
    0,                                          // bInterfaceNumber
    0,                                          // bAlternateSetting
    3,                                          // bNumEndpoints
    0xFF,                                       // bInterfaceClass
    0xFF,                                       // bInterfaceSubclass
    0xFF,                                       // bInterfaceProtocol
    0,                                          // iInterface
  },
  { /* Endpoint Descriptor ¹0 */
    sizeof(TUsbEndpointDescriptor),             // bLength
    USB_TYPE_ENDPOINT_DESCRIPTOR,               // bDescriptorType
    0x81,                                       // bEndpointAddress (Ep1Out)
    0x02,                                       // bmAttributes - Bulk
    USB_EP1_MAX_PACKET_SIZE,                    // wMaxPacketSize
    0                                           // bInterval
  },
  { /* Endpoint Descriptor ¹1 */
    sizeof(TUsbEndpointDescriptor),             // bLength
    USB_TYPE_ENDPOINT_DESCRIPTOR,               // bDescriptorType
    0x02,                                       // bEndpointAddress (Ep2In)
    0x02,                                       // bmAttributes - Bulk
    USB_EP2_MAX_PACKET_SIZE,                    // wMaxPacketSize
    0                                           // bInterval
  },
  { /* Endpoint Descriptor ¹2 */
    sizeof(TUsbEndpointDescriptor),             // bLength
    USB_TYPE_ENDPOINT_DESCRIPTOR,               // bDescriptorType
    0x83,                                       // bEndpointAddress (Ep3Out)
    0x02,                                       // bmAttributes - Bulk
    USB_EP3_MAX_PACKET_SIZE,                    // wMaxPacketSize
    0                                           // bInterval
  }
};

#define USB_CONTROLOUTENDPOINT_BUFFER_SIZE      64
#define USB_CONTROLINENDPOINT_BUFFER_SIZE       64

static NOINIT struct {
  struct {
    BYTE Buffer[USB_CONTROLOUTENDPOINT_BUFFER_SIZE];
  } R; // Read
  struct {
    BYTE Buffer[USB_CONTROLINENDPOINT_BUFFER_SIZE];
    BYTE Cmd;
    WORD DotX;
    WORD DotY;
    DWORD DotColor;
  } W; // Write
} UsbEp0;

#define USB_ENDPOINT1_BUFFER_SIZE             4096

static NOINIT struct {
  struct {
    BYTE  Buffer[USB_ENDPOINT1_BUFFER_SIZE];
    WORD  BulkSize;
    DWORD FlashAddress;
    WORD  I;
    WORD  N;
  } W; // Write
} UsbEp1;

static NOINIT PCD_HandleTypeDef* UsbHandle;

static const char UsbRecoverStr[] = "#RECOVER-SQ";

static void UsbProcessStandardDeviceRequest(PCD_HandleTypeDef *hpcd);
static void UsbProcessStandardInterfaceRequest(PCD_HandleTypeDef *hpcd);
static void UsbProcessStandardEndpointRequest(PCD_HandleTypeDef *hpcd);
static void UsbProcessVendorEndpointRequest(PCD_HandleTypeDef *hpcd);

static void UsbStallControlEndpoint(void);
static void UsbWriteBufferToControlEndpoint(WORD n);
static void UsbStartWriteBufferToEndpoint1(WORD n);

//---------------------------------------------------------
void InitUsb_pd(void)
{
  UsbHandle = &hpcd_USB_OTG_FS;
  HAL_PCD_SetRxFiFo(UsbHandle, 0x40);
  HAL_PCD_SetTxFiFo(UsbHandle, 0, 0x40);
  HAL_PCD_SetTxFiFo(UsbHandle, 1, USB_EP1_FIFO_SIZE);

  UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
  UsbEp1.W.BulkSize = USB_ENDPOINT1_BUFFER_SIZE;
}

//---------------------------------------------------------
void SetupUsb_pd(void)
{
  /* Peripheral clock enable */
  __HAL_RCC_USB_OTG_FS_CLK_ENABLE();
  /* Peripheral interrupt init */
  HAL_NVIC_SetPriority(OTG_FS_IRQn, 0, 0);
  HAL_NVIC_EnableIRQ(OTG_FS_IRQn);
  HAL_PCD_EP_Open(UsbHandle, 0x80, 64, EP_TYPE_CTRL);
  HAL_PCD_EP_Open(UsbHandle,    0, 64, EP_TYPE_CTRL);
  HAL_PCD_Start(UsbHandle);
}

//---------------------------------------------------------
void DoUsb_pd(void)
{
  switch(UsbEp0.W.Cmd) {
    case USB_CMD_READ_FLASH:
      FlashAddress = UsbEp1.W.FlashAddress;
      FlashRead(UsbEp1.W.Buffer, USB_ENDPOINT1_BUFFER_SIZE);
      UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
      break;
    case USB_CMD_READ_DOTS:
      LcdGetPixels(0, UsbEp0.W.DotY, LCD_W, 2, UsbEp1.W.Buffer);
      UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
      break;
    case USB_CMD_READ_DOT: {
      WORD x = UsbEp0.W.DotX;
      WORD y = UsbEp0.W.DotY;
      if(x < LCD_W && y < LCD_H) {
        UsbEp0.W.DotColor = LcdGetPixel(x, y);
      }
      UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
      break;
    }
    case USB_CMD_CLEAR_BULK: {
      WORD i;
      WORD len = strlen(UsbRecoverStr);
      for(i = 0; i < 100; i++) {
        memcpy(UsbEp1.W.Buffer + i*len, UsbRecoverStr, len);
      }
      UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
      break;
    }
    case USB_CMD_DO_NOTHING:
      break;
    default:
      UsbEp0.W.Cmd = USB_CMD_DO_NOTHING;
  }


  if(PrgFlags.CentiSec) {
    UsbFlags.CableConnected =
     GPIO_IS_IN_HIGH(USB_CONNECTED_PORT, USB_CONNECTED_BIT) ? 1 : 0;
    if(UsbFlags.CableConnected)
      ClearAutoOff();
    else {
      UsbFlags.Connected = 0;
    }
  }
}

//---------------------------------------------------------
void HAL_PCD_SetupStageCallback(PCD_HandleTypeDef *hpcd)
{
  TUsbSetupRequest* request = (TUsbSetupRequest*)hpcd->Setup;

  switch(request->bmRequestType & 0x7F) {
    case USB_STANDARD_DEVICE_REQUEST:
      UsbProcessStandardDeviceRequest(hpcd);
      break;
    case USB_STANDARD_INTERFACE_REQUEST:
      UsbProcessStandardInterfaceRequest(hpcd);
      break;
    case USB_STANDARD_ENDPOINT_REQUEST:
      UsbProcessStandardEndpointRequest(hpcd);
      break;
    case USB_VENDOR_DEVICE_REQUEST:
      UsbProcessVendorEndpointRequest(hpcd);
  }
}

//---------------------------------------------------------
void HAL_PCD_ResetCallback(PCD_HandleTypeDef *hpcd)
{
  HAL_PCD_EP_Open(hpcd, 0x00, 64, EP_TYPE_CTRL);
	HAL_PCD_EP_Open(hpcd, 0x80, 64, EP_TYPE_CTRL);
}

//---------------------------------------------------------
void HAL_PCD_DataOutStageCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
}

//---------------------------------------------------------
void HAL_PCD_DataInStageCallback(PCD_HandleTypeDef *hpcd, uint8_t epnum)
{
  WORD written_bytes;
  BYTE* buff;

  if(epnum == 0) {
    HAL_PCD_EP_Receive(hpcd, epnum, NULL, 0);
  }
  else {
    buff = UsbEp1.W.Buffer + UsbEp1.W.I;
    written_bytes = UsbEp1.W.N - UsbEp1.W.I;
    if(written_bytes > USB_EP1_MAX_PACKET_SIZE)
      written_bytes = USB_EP1_MAX_PACKET_SIZE;
    UsbEp1.W.I += written_bytes;
    if(written_bytes) {
      HAL_PCD_EP_Transmit(UsbHandle, 0x81, buff, written_bytes);
      if(UsbEp1.W.I == UsbEp1.W.N)
        HAL_PCD_EP_Flush(UsbHandle, 0x81);
    }
  }
}

//---------------------------------------------------------
static void UsbProcessStandardDeviceRequest(PCD_HandleTypeDef *hpcd)
{
  TUsbSetupRequest* request = (TUsbSetupRequest*)hpcd->Setup;
  BYTE *buff = UsbEp0.W.Buffer;
  WORD n;

  switch(request->bRequest) {
    case USB_GET_STATUS:
      buff[0] = 0x01;
      buff[1] = 0x00;
      UsbWriteBufferToControlEndpoint(2);
      break;

    case USB_SET_ADDRESS:
      UsbFlags.Address = request->wValue & 0x7F;
      HAL_PCD_SetAddress(hpcd, UsbFlags.Address);
      UsbWriteBufferToControlEndpoint(0);
      UsbFlags.Connected = 1;
      break;

    case USB_GET_DESCRIPTOR:
      switch(request->wValue >> 8) {
        case USB_TYPE_DEVICE_DESCRIPTOR:
          n = sizeof(UsbDeviceDescriptor);
          if(n > request->wLength)
            n = (BYTE)request->wLength;
          memcpy(buff, (BYTE*)&UsbDeviceDescriptor, n);
          UsbWriteBufferToControlEndpoint(n);
          break;

        case USB_TYPE_CONFIGURATION_DESCRIPTOR:
          n = sizeof(UsbConfigData);
          if(n > request->wLength)
            n = (BYTE)request->wLength;
          memcpy(buff, (BYTE*)&UsbConfigData, n);
          UsbWriteBufferToControlEndpoint(n);
          break;

        case USB_TYPE_STRING_DESCRIPTOR:
          switch(request->wValue & 0xFF) {
            case 0:
              n = sizeof(UsbLangIdDescriptor);
              memcpy(buff, (BYTE*)&UsbLangIdDescriptor, n);
              break;

            case 1:
              n = sizeof(UsbManufacturerDescriptor);
              memcpy(buff, (BYTE*)&UsbManufacturerDescriptor, n);
              break;

            case 2:
              if(DeviceType == dtInc) {
                n = sizeof(UsbIncProductDescriptor);
                memcpy(buff, (BYTE*)&UsbIncProductDescriptor, n);
              }
              else if(DeviceType == dtVist) {
                n = sizeof(UsbVistProductDescriptor);
                memcpy(buff, (BYTE*)&UsbVistProductDescriptor, n);
              }
              else {
                n = sizeof(UsbIncVistProductDescriptor);
                memcpy(buff, (BYTE*)&UsbIncVistProductDescriptor, n);
              }
              break;

            default:
              n = 0;
          }
          if(n > request->wLength)
            n = (BYTE)request->wLength;
          UsbWriteBufferToControlEndpoint(n);
      }
      break;

    case USB_GET_CONFIGURATION:
      buff[0] = UsbFlags.DeviceConfigured;
      UsbWriteBufferToControlEndpoint(1);
      break;

    case USB_SET_CONFIGURATION:
      UsbFlags.DeviceConfigured = (BYTE)request->wValue;
      UsbWriteBufferToControlEndpoint(0);
      HAL_PCD_EP_Open(hpcd, 0x81, 512, EP_TYPE_BULK);
      HAL_PCD_EP_Open(hpcd, 0x02, 512, EP_TYPE_BULK);
      HAL_PCD_EP_Open(hpcd, 0x83, 512, EP_TYPE_BULK);
  }
}

//---------------------------------------------------------
static void UsbProcessStandardInterfaceRequest(PCD_HandleTypeDef *hpcd)
{
  BYTE *buff = UsbEp0.W.Buffer;
  TUsbSetupRequest* request = (TUsbSetupRequest*)hpcd->Setup;

  switch(request->bRequest) {
    case USB_GET_STATUS:
      buff[0] = 0x00;
      buff[1] = 0x00;
      UsbWriteBufferToControlEndpoint(2);
      break;

    case USB_SET_INTERFACE:
      if(request->wIndex == 0 && request->wValue == 0)
        UsbWriteBufferToControlEndpoint(0);
      else
        UsbStallControlEndpoint();
      break;

    case USB_GET_INTERFACE:
      buff[0] = 0;
      UsbWriteBufferToControlEndpoint(1);
      break;
  }
}

//---------------------------------------------------------
static void UsbProcessStandardEndpointRequest(PCD_HandleTypeDef *hpcd)
{
  BYTE *buff = UsbEp0.W.Buffer;
  TUsbSetupRequest* request = (TUsbSetupRequest*)hpcd->Setup;

  switch(request->bRequest) {
    case USB_SET_FEATURE:
      UsbWriteBufferToControlEndpoint(0);
      break;
    case USB_CLEAR_FEATURE:
      UsbWriteBufferToControlEndpoint(0);
      break;

    case USB_GET_STATUS:
      buff[0] = 0x00;
      buff[1] = 0x00;
      UsbWriteBufferToControlEndpoint(2);
  }
}

//---------------------------------------------------------
static void UsbProcessVendorEndpointRequest(PCD_HandleTypeDef *hpcd)
{
  BYTE *buff = UsbEp0.W.Buffer;
  TUsbSetupRequest* request = (TUsbSetupRequest*)hpcd->Setup;
  BYTE n = 0;
  WORD data_size;

  switch(request->bRequest) {
    case USB_VENDOR_GET_PRODUCT_SPECIFICATION:
      if(DeviceType == dtInc) {
        buff[0] = (BYTE)(PRODUCT_INC_ID);
        buff[1] = (BYTE)(PRODUCT_INC_ID >> 8U);
      } 
      else if(DeviceType == dtVist) {
        buff[0] = (BYTE)(PRODUCT_VIST_ID);
        buff[1] = (BYTE)(PRODUCT_VIST_ID >> 8U);
      }
      else {
        buff[0] = (BYTE)(PRODUCT_INCVIST_ID);
        buff[1] = (BYTE)(PRODUCT_INCVIST_ID >> 8U);
      }
      buff[2] = (BYTE)(PRODUCT_VERSION);
      buff[3] = (BYTE)(PRODUCT_VERSION >> 8);
      buff[4] = (BYTE)(PRODUCT_MODEL);
      buff[5] = (BYTE)(PRODUCT_MODEL >> 8);
      n = 6;
      break;

    case USB_VENDOR_SET_BULK_SIZE:
      data_size = request->wValue;
      if(data_size > USB_ENDPOINT1_BUFFER_SIZE)
        data_size = USB_ENDPOINT1_BUFFER_SIZE;
      UsbEp1.W.BulkSize = data_size;
      break;

    case USB_VENDOR_START_BULK:
      UsbStartWriteBufferToEndpoint1(UsbEp1.W.BulkSize);
      break;

    case USB_VENDOR_READ_FLASH:
      UsbEp1.W.FlashAddress = request->wValue | (((DWORD)request->wIndex) << 16U);
      UsbEp0.W.Cmd = USB_CMD_READ_FLASH;
      break;

    case USB_VENDOR_IS_CMD_DONE:
      buff[0] = (BYTE)(UsbEp0.W.Cmd == USB_CMD_DO_NOTHING ? 0xFF : 0x00);
      n = 1;
      break;

    case USB_VENDOR_GET_FLASH_CHANGED:
      buff[0] = FlashChanged == TRUE ? 0xFF : 0x00;
      n = 1;
      break;

    case USB_VENDOR_CLEAR_FLASH_CHANGED:
      FlashChanged = FALSE;
      break;

    case USB_VENDOR_GET_SCREEN_WIDTH:
      buff[0] = (BYTE)(LCD_W%0x100);
      buff[1] = (BYTE)(LCD_W/0x100);
      n = 2;
      break;

    case USB_VENDOR_GET_SCREEN_HEIGHT:
      buff[0] = (BYTE)(LCD_H%0x100);
      buff[1] = (BYTE)(LCD_H/0x100);
      n = 2;
      break;

    case USB_VENDOR_READ_SCREEN_DOT: {
      UsbEp0.W.DotX = request->wValue;
      UsbEp0.W.DotY = request->wIndex;
      UsbEp0.W.Cmd = USB_CMD_READ_DOT;
      break;
    }

    case USB_VENDOR_GET_SCREEN_DOT: {
      DWORD data = UsbEp0.W.DotColor;
      memcpy(buff, (BYTE*)&data, n = 4);
      break;
    }

    case USB_VENDOR_START_SCREEN_DOTS: {
      UsbEp0.W.DotY = request->wValue;
      UsbEp0.W.Cmd = USB_CMD_READ_DOTS;
      break;
    }

    case USB_VENDOR_CLEAR_BULK: {
      UsbEp0.W.Cmd = USB_CMD_CLEAR_BULK;
      break;
    }

    case USB_VENDOR_RECOVER: {
      WORD index = (WORD)request->wIndex;
      BYTE b = 0;
      if(index < sizeof(UsbRecoverStr))
        b = UsbRecoverStr[index];
      buff[0] = b;
      n = 1;
      break;
    }

    default:
      UsbStallControlEndpoint();
      return;
  }

  UsbWriteBufferToControlEndpoint(n);
}

//---------------------------------------------------------
static void UsbStallControlEndpoint(void)
{
  HAL_PCD_EP_SetStall(UsbHandle, 0);
}

//---------------------------------------------------------
static void UsbWriteBufferToControlEndpoint(WORD n)
{
  HAL_PCD_EP_Transmit(UsbHandle, 0, n == 0 ? NULL : UsbEp0.W.Buffer, n);
}

//---------------------------------------------------------
static void UsbStartWriteBufferToEndpoint1(WORD n)
{
  WORD written_bytes = n;
  if(written_bytes > USB_EP1_MAX_PACKET_SIZE)
    written_bytes = USB_EP1_MAX_PACKET_SIZE;
  UsbEp1.W.N = n;
  UsbEp1.W.I = written_bytes;
  HAL_PCD_EP_Transmit(UsbHandle, 0x81, UsbEp1.W.Buffer, written_bytes);
  if (written_bytes == n)
    HAL_PCD_EP_Flush(UsbHandle, 0x81);
}
