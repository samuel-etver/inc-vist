#----------------------------------------------------------
# build.py
#----------------------------------------------------------

# date/time of building (refresh, when building something new)

import datetime

#----------------------------------------------------------
def main():
    dt = datetime.date.today()
    (day, mon, year) = (dt.day, dt.month, dt.year)
    
    f = open('build.inc', 'wt')

    f.write("#define BUILD_DAY\t\t" + str(day) + "\n")
    f.write("#define BUILD_MON\t\t" + str(mon) + "\n")
    f.write("#define BUILD_YEAR\t\t" + str(year) + "\n")

    f.write("\n")
    
    txt = '{:d}-{:02d}-{:02d}'.format(year, mon, day);

    f.write("#define BUILD_DATE_STR\t\t\"" + txt + "\"\n")
    f.write("#define BUILD_DATE_STR_LEN\t" + str(len(txt)) + "\n")        
    
    f.close()
    

main()
