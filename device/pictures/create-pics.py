#-----------------------------------------------------------
# create-pics.py
#-----------------------------------------------------------

import java.awt.image.BufferedImage
import java.awt.RenderingHints
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.io.File
import javax.imageio.ImageIO
import array
import StringIO
import string
import os.path

NL = '\015\012'

BMP_FILES = (
#  File name; transparent color: R, G, B    
  ('material.bmp',      0, 255, 0),
  ('usb.bmp',           0, 255, 0),
  ('power.bmp',         0, 255, 0),
  ('LowPower.bmp',      0, 255, 0),
  ('ChargeLight.bmp',   0, 255, 0),
  ('ChargeDark.bmp',    0, 255, 0),
  ('SpinUp.bmp',        0, 255, 0),
  ('SpinDown.bmp',      0, 255, 0),
  ('SmallSpinUp.bmp',   0, 255, 0),
  ('SmallSpinDown.bmp', 0, 255, 0),
  ('rectangle.bmp',     0, 255, 0),
  ('circle.bmp',        0, 255, 0),
  ('Checked.bmp',       0, 255, 0),
  ('UnChecked.bmp',     0, 255, 0),
  ('ArrowRight.bmp',    0, 255, 0),
  ('ArrowLeft.bmp',     0, 255, 0),
  ('ArrowTop.bmp',      0, 255, 0),
  ('MenuDown.bmp',      0, 255, 0),
  ('MenuUp.bmp',        0, 255, 0),
  ('chart_f.bmp',       0, 255, 0),
  ('chart_3k.bmp',      0, 255, 0),
  ('chart_30k.bmp',     0, 255, 0),
  ('chart_t.bmp',       0, 255, 0),
  ('chart_90.bmp',      0, 255, 0),
)
TRANSPARENT_COLOR8  = 0x20
TRANSPARENT_COLOR16 = 0x0001
SUBSTITUTE_COLOR8   = 0x00
SUBSTITUTE_COLOR16  = 0x0002

ConvertColorProc = None
FormatColorProc  = None
TransparentColor = None

ImageClass          = java.awt.image.BufferedImage
RenderingHintsClass = java.awt.RenderingHints
ColorClass          = java.awt.Color
RectClass           = java.awt.geom.Rectangle2D
FileClass           = java.io.File
ImageIoClass        = javax.imageio.ImageIO

#-----------------------------------------------------------
# 8R8G8B -> 2R3G2B
def Color24to8(r, g, b):
    r = (r*0x08)/0x100
    g = (g*0x08)/0x100
    b = (b*0x04)/0x100
    return b | (g << 2) | (r << 5)

#-----------------------------------------------------------
# 8R8G8B -> 5R6G5B
def Color24to16(r, g, b):
    r = (r*0x20)/0x100
    g = (g*0x40)/0x100
    b = (b*0x20)/0x100
    return b | (g << 5) | (r << 11)

#-----------------------------------------------------------
def FormatColor8(color):
    return ' 0x%02X' % color

#-----------------------------------------------------------
def FormatColor16(color):
    return ' 0x%04X' % color

#-----------------------------------------------------------
def CreatePicture(index, bits):
    bmp_file = BMP_FILES[index]
    bmp_file_name = bmp_file[0]
    tr_color_r, tr_color_g, tr_color_b = bmp_file[1:4]
    bmp = ImageIoClass.read(FileClass(bmp_file_name))
    bmp_w = bmp.getWidth()
    bmp_h = bmp.getHeight()

    pic_name = os.path.splitext(bmp_file_name)[0]
    output = StringIO.StringIO()
    output.write('const TPicture Pictures' + \
     pic_name[0].upper() + pic_name[1:] + '[] = {' + NL)
    output.write(('  %3i, // Width'  % bmp_w) + NL)
    output.write(('  %3i, // Height' % bmp_h) + NL)
    data_index = 0
    for yi in xrange(0, bmp_h):
        y = yi
        for xi in xrange(0, bmp_w):
            x = xi
            rgb = bmp.getRGB(x, y)
            r = (rgb >> 16) & 0xFF
            g = (rgb >>  8) & 0xFF
            b = (rgb      ) & 0xFF
            if r == tr_color_r and \
               g == tr_color_g and \
               b == tr_color_b:
                color = TransparentColor
            else:
                color = ConvertColorProc(r, g, b)
                if color == TransparentColor:
                    color = SubstituteColor
            if data_index%10 == 0:
                output.write(' ')
            output.write(FormatColorProc(color))
            if data_index != bmp_w*bmp_h - 1:
                output.write(',')
            if data_index%10 == 9:
                output.write(NL)
            data_index += 1
    if (bmp_w*bmp_h)%10 != 9:
        output.write(NL)
    output.write('};' + NL)

    output_file_name = \
     os.path.splitext(bmp_file_name)[0] + str(bits) + '.dat'
    fh = open(output_file_name, 'wb')
    fh.write(output.getvalue())
    fh.close()
    output.close()

    
#ConvertColorProc = Color24to8
#FormatColorProc  = FormatColor8
#TransparentColor = TRANSPARENT_COLOR8
#SubstituteColor  = SUBSTITUTE_COLOR8
#for index in range(0, len(BMP_FILES)):
#    CreatePicture(index, 8)
    
ConvertColorProc = Color24to16
FormatColorProc  = FormatColor16
TransparentColor = TRANSPARENT_COLOR16
SubstituteColor  = SUBSTITUTE_COLOR16
for index in range(0, len(BMP_FILES)):
    CreatePicture(index, 16)

