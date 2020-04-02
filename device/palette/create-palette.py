#-----------------------------------------------------------
# create-palette.py
#-----------------------------------------------------------

import StringIO

NL = '\015\012'

FILE_NAME = 'palette.dat'

#-----------------------------------------------------------
def Color8to16(color8):
    r = (color8/0x04)/0x08
    g = (color8/0x04)%0x08
    b = color8%0x04
    r = (r*0x20)/0x08
    g = (g*0x40)/0x08
    b = (b*0x20)/0x04
    return b | (g << 5) | (r << 11)

#-----------------------------------------------------------
def CreatePalette():
    output = StringIO.StringIO()
    for i in xrange(0, 256):
        color = Color8to16(i)
        if i%10 == 0:
            output.write(' ')
        output.write(' 0x%.4X' % color)
        if i != 255:
            output.write(',')
        if i%10 == 9:
            output.write(NL)
    output.write(NL)

    output_file_name = FILE_NAME
    fh = open(output_file_name, 'wb')
    fh.write(output.getvalue())
    fh.close()
    output.close()

CreatePalette()
