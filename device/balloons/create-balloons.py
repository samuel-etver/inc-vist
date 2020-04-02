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
#  File name; transparent color: R, G, B; Side width;
  ('Menu.bmp',                 0, 255, 0, 9),
  ('MenuSimple.bmp',           0, 255, 0, 9),
  ('36.bmp',                   0, 255, 0, 5),
  ('36Simple.bmp',             0, 255, 0, 5),
  ('36Dark.bmp',               0, 255, 0, 5),
  ('36DarkSimple.bmp',         0, 255, 0, 5),  
  ('SpinDark.bmp',             0, 255, 0, 3),
  ('SpinLight.bmp',            0, 255, 0, 3),
  ('edit.bmp',                 0, 255, 0, 2),
  ('EditSimple.bmp',           0, 255, 0, 2),
  ('edit24.bmp',               0, 255, 0, 2),
  ('Edit24Simple.bmp',         0, 255, 0, 2),
  ('SmallSpinDark.bmp',        0, 255, 0, 2),
  ('SmallSpinDarkSimple.bmp',  0, 255, 0, 2),  
  ('SmallSpinLight.bmp',       0, 255, 0, 2),
  ('SmallSpinLightSimple.bmp', 0, 255, 0, 2),
  ('gauge.bmp',                0, 255, 0, 1)
)
TRANSPARENT_COLOR = 0x20
SUBSTITUTE_COLOR  = 0x10


ImageClass          = java.awt.image.BufferedImage
RenderingHintsClass = java.awt.RenderingHints
ColorClass          = java.awt.Color
RectClass           = java.awt.geom.Rectangle2D
FileClass           = java.io.File
ImageIoClass        = javax.imageio.ImageIO


#-----------------------------------------------------------
def ConvertColor(r, g, b):
    return r

#-----------------------------------------------------------
def FormatColor(color):
    return ' 0x%02X' % color

#-----------------------------------------------------------
def CreatePicture(index):
    bmp_file = BMP_FILES[index]
    bmp_file_name = bmp_file[0]
    tr_color_r, tr_color_g, tr_color_b, bmp_side_w = bmp_file[1:5]
    bmp = ImageIoClass.read(FileClass(bmp_file_name))
    bmp_w = bmp.getWidth()
    bmp_h = bmp.getHeight()
    
    pic_name = os.path.splitext(bmp_file_name)[0]
    base_name = 'Balloons' + pic_name[0].upper() + pic_name[1:]
    
    def CopyData(x0, x1):
        data_index = 0
        for yi in xrange(0, bmp_h):
            y = yi
            for xi in xrange(x0, x1):
                x = xi
                index = (y*bmp_w + x)*3
                rgb = bmp.getRGB(x, y)
                r = (rgb >> 16) & 0xFF
                g = (rgb >>  8) & 0xFF
                b = (rgb      ) & 0xFF
                if r == tr_color_r and \
                   g == tr_color_g and \
                   b == tr_color_b:
                    color = TRANSPARENT_COLOR
                else:
                    color = ConvertColor(r, g, b)
                    if color == TRANSPARENT_COLOR:
                        color = SUBSTITUTE_COLOR
                if data_index%10 == 0:
                    output.write(' ')
                output.write(FormatColor(color))
                if data_index != (x1 - x0)*bmp_h - 1:
                    output.write(',')
                if data_index%10 == 9:
                    output.write(NL)
                data_index += 1
        if (bmp_w*bmp_h)%10 != 9:
            output.write(NL)

    output = StringIO.StringIO()
    output.write('static const BYTE ' + base_name + 'LeftSideData[] = {' + NL)
    CopyData(0, bmp_side_w)
    output.write('};' + NL)
    output.write('static const BYTE ' + base_name + 'RightSideData[] = {' + NL)
    CopyData(bmp_w - bmp_side_w, bmp_w)
    output.write('};' + NL)
    output.write('static const BYTE ' + base_name + 'BodyData[] = {' + NL)
    CopyData(bmp_w/2, bmp_w/2 + 1)
    output.write('};' + NL)

    bmp_w = 0

        
    output.write('const TBalloon Balloons' + \
     pic_name[0].upper() + pic_name[1:] + ' = {' + NL)
    output.write(('  %i, // Height' % bmp_h) + NL)
    output.write(('  %i, // Side width'  % bmp_side_w) + NL)
    output.write('  ' + base_name + 'LeftSideData,' + NL)
    output.write('  ' + base_name + 'RightSideData,' + NL)              
    output.write('  ' + base_name + 'BodyData' + NL)            
    output.write('};' + NL)

    output_file_name = base_name + '.dat'
    fh = open(output_file_name, 'wb')
    fh.write(output.getvalue())
    fh.close()
    output.close()
   
for index in range(0, len(BMP_FILES)):
    CreatePicture(index)

#while True:
#    pass
