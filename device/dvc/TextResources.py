#-----------------------------------------------------------
# TextResources.py
#
# Read src file (TextResources.c), find number of
# text resources, and create reference file (TextResources.inc)
#-----------------------------------------------------------

import re

SRC_FILE_NAME = 'TextResources.c'
DST_FILE_NAME = 'TextResources.inc'
NL = chr(13) + chr(10)
COMMA_NL = ',' + NL

src_file = open(SRC_FILE_NAME, 'rb')
src = src_file.read().decode('cp1251')
src_file.close()

items = re.findall('Tr[0-9]{4}', src)

max_item = ''
max_num = 0
for item in items:
    txt = item[2:].lstrip('0')
    if len(txt) > 0:
        tmp = int(txt)
        if max_num < tmp:
            max_num = tmp
            max_item = item

print '\nLAST ITEM:', max_item

dst = ''
for i in xrange(0, max_num + 1):
    txt = 'Tr' + format(i, '04d')
    dst += txt + COMMA_NL

dst_file = open(DST_FILE_NAME, 'wb')
dst_file.write(dst)
dst_file.close()

