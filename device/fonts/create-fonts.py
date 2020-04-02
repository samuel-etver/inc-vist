import java.awt.Font
import java.awt.image.BufferedImage
import java.awt.RenderingHints
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.io.File
import javax.imageio.ImageIO
import array
import StringIO
import string

IMAGE_W = 320
IMAGE_H = 240

NL = '\015\012'

UNICODE_CHARS = {
   1 : 0x2265, # greater-then or equal-to
   2 : 0x2264, # less-then or equal-to
   3 : 0x00D7, # cross x
   4 : 0x00D8, # diameter
   5 : 0x2116, # number
   6 : 0x03B5, # epsilon
   7 : 0x03B1, # alpha
   8 : 0x03C3, # sigma
   9 : 0x0394, # delta
  10 : 0x2080, # subscript 0
  11 : 0x2081, # subscript 1
  12 : 0x2082, # subscript 2
  13 : 0x2083, # subscript 3
  14 : 0x2084, # subscript 4
  15 : 0x2085, # subscript 5
  16 : 0x2086, # subscript 6
  17 : 0x2087, # subscript 7
  18 : 0x2088, # subscript 8
  19 : 0x2089, # subscript 9
  20 : 0x00B0, # superscript 0
  21 : 0x00B1, # superscript 1
  22 : 0x00B2, # superscript 2
  23 : 0x00B3, # superscript 3
  24 : 0x2074, # superscript 4
  25 : 0x2075, # superscript 5
  26 : 0x2076, # superscript 6
  27 : 0x2077, # superscript 7
  28 : 0x2078, # superscript 8
  29 : 0x2079, # superscript 9
  30 : 0x03BC, # mu 
}

ALL_CHARS = [ord(' ')]
for ch in range(ord('0'), ord('9') + 1):
    ALL_CHARS.append(ch)
for ch in range(ord('A'), ord('Z') + 1):
    ALL_CHARS.append(ch)
for ch in range(ord('a'), ord('z') + 1):
    ALL_CHARS.append(ch)
#                                                                                 number
for ch in ('.', ',', ':', ';', '!', '?', '"', "'", '%', '-', '+', '/', \
  '<', '>', chr(0xBD), ')', '(', '*', '='):
    ALL_CHARS.append(ord(ch))
for ch in range(0xC0, 0xDF + 1):
    ALL_CHARS.append(ch)
for ch in range(0xE0, 0xFF + 1):
    ALL_CHARS.append(ch)
for ch in UNICODE_CHARS:
    ALL_CHARS.append(ch)

BIG_CHARS = [ord(' ')]
for ch in ('.', '=', '-', '*', '?'):
    BIG_CHARS.append(ord(ch))
for ch in ('S', 'F', 'm', 'N', 'k', 'g', 'f', chr(236), chr(234), chr(227), chr(241), chr(205)):
    BIG_CHARS.append(ord(ch))
for ch in range(ord('0'), ord('9') + 1):
    BIG_CHARS.append(ch)
for ch in UNICODE_CHARS:
    BIG_CHARS.append(ch)

# FONTS_DATA
# 0 - Name
# 1 - Size
# 2 - Weight (normal, bold)
# 4 - File name
# 5 - Chars
# 6 - margin top
# 7 - margin bottom
FONTS_DATA = (
  ('Arial',        14, 'normal', 'Font08', ALL_CHARS, 3, 1),
  ('Arial',        19, 'normal', 'Font10', ALL_CHARS, 1, 1),
  ('Arial Narrow', 23, 'normal', 'Font11', ALL_CHARS, 1, 1),
  ('Arial',        28, 'normal', 'Font12', ALL_CHARS, 3, 1),
  ('Arial',        24, 'bold',   'Font13', ALL_CHARS, 2, 1),
  ('Arial',        32, 'normal', 'Font16', ALL_CHARS, 3, 1),
  ('Arial',        38, 'normal', 'Font20', ALL_CHARS, 4, 1),
)
FONTS_COUNT = len(FONTS_DATA)

FontClass           = java.awt.Font
ImageClass          = java.awt.image.BufferedImage
RenderingHintsClass = java.awt.RenderingHints
ColorClass          = java.awt.Color
RectClass           = java.awt.geom.Rectangle2D
FileClass           = java.io.File
ImageIoClass        = javax.imageio.ImageIO

Image = ImageClass(IMAGE_W, IMAGE_H, ImageClass.TYPE_INT_ARGB)
Gr    = Image.createGraphics()
Gr.setRenderingHint(RenderingHintsClass.KEY_TEXT_ANTIALIASING,
  RenderingHintsClass.VALUE_TEXT_ANTIALIAS_OFF)
Gr.setColor(ColorClass.BLACK)
Gr.fill(RectClass.Double(0, 0, IMAGE_W, IMAGE_H))

#-----------------------------------------------------------
def CreateFont(index):
    fd = FONTS_DATA[index]
    
    font_name, font_size, font_weight, file_name, chars, \
      margin_top, margin_bottom = fd[0:]
    if font_weight == 'bold':
        font_weight = FontClass.BOLD
    else:
        font_weight = FontClass.PLAIN
        
    font = FontClass(font_name, font_weight, font_size)
    Gr.setFont(font)
    font_metrics = Gr.getFontMetrics()
    font_height  = font_metrics.getHeight() - margin_top - margin_bottom
    font_descent = font_metrics.getDescent() - margin_bottom
    font_ascent  = font_metrics.getAscent() - margin_top

    out_buff = array.array('B', [0]*(IMAGE_W*IMAGE_H/8))
    indexes  = array.array('i', [0]*256)

    output = StringIO.StringIO()
    output.write('static const WORD ' + file_name + 'Height  = ' + \
      str(font_height) + ';' + NL)
    output.write('static const WORD ' + file_name + 'Descent = ' + \
      str(font_descent) + ';' + NL)
    output.write('static const WORD ' + file_name + 'Ascent  = ' + \
      str(int(font_ascent)) + ';' + NL)
    
    output.write(NL + 'static const BYTE ' + file_name + 'Data[] = {')

    char_index = 0
    index      = 0
    last_nl    = False
    test_char  = chr(14)#'A'
    
    for char_code in chars:
        txt = chr(char_code)
        if UNICODE_CHARS.has_key(char_code):
            txt_enc = unichr(UNICODE_CHARS[char_code])
        else:
            txt_enc = txt.decode('cp1251')
        txt_height = font_height
        txt_width  = font_metrics.stringWidth(txt_enc)
        txt_width_in_bytes = txt_width/8
        if txt_width%8 != 0:
		txt_width_in_bytes += 1
	if txt == test_char:
            Gr.setColor(ColorClass.BLACK)
            Gr.fill(RectClass.Double(0, 0, IMAGE_W, IMAGE_H))
        Gr.setColor(ColorClass.WHITE)
        Gr.fill(RectClass.Double(0, 0, txt_width, txt_height))
        Gr.setColor(ColorClass.BLACK)
        Gr.drawString(txt_enc, 0, font_ascent)
        output.write(NL + '  // ' + str(char_index) + '-' + \
         str(char_code) + NL)
        output.write('  ' + str(txt_width) + ', // width' + NL)
        # reset out_buff
        for i in xrange(0, txt_height*txt_width_in_bytes):
            out_buff[i] = 0
        #if txt == test_char:
        #    SaveImage(file_name + '.png')
        for y in xrange(0, txt_height):
            for x in xrange(0, txt_width):
                color = Image.getRGB(x, y) & 0xFFFFFF
                if color == 0:
                    i = y*txt_width_in_bytes + (x/8)
                    out_buff[i] |= 1 << (x%8)
        for i in xrange(0, txt_height*txt_width_in_bytes):
            txt = hex(out_buff[i])
            if i%10 == 0:
                output.write(' ')
            if len(txt) != 4:
                txt = '0x0' + txt[2]
            output.write(' ' + txt + ',')
            last_nl = False
            if i%10 == 9:
                output.write(NL)
                last_nl = True
        indexes[char_code] = index
        index += 1 + txt_height*txt_width_in_bytes
        char_index += 1
        
    if not last_nl:
        output.write(NL)
    output.write('};' + NL)

    output.write(NL + 'static const WORD ' + file_name + \
      'CharIndexes[] = {' + NL)
    for i in xrange(0, 256):
        if i%10 == 0:
            output.write(' ')
        output.write(' %4i' % indexes[i])
        if i != 255:
            output.write(',')
        if i%10 == 9:
            output.write(NL)
    output.write(NL + '};' + NL)
    
    fh = open(file_name + '.dat', 'wb')
    fh.write(output.getvalue())
    fh.close()
    output.close()    

def SaveImage(file_name):
    ImageIoClass.write(Image, 'png', FileClass(file_name))
    

for FontIndex in range(0, FONTS_COUNT):
    CreateFont(FontIndex)

#while True:
#    pass
