###############################################################################
###                         Killdump-data-mover                             ###
###############################################################################
# Created by Jacob Williams for STAT 4870
# Built under Python 3.5
# Last modified: 2/13/2019

# A Python script for moving the Killdump.csv files from EVE Online's monthly
# economic report into the proper folder for analysis, as well as for modifying
# the files to remove unwanted data.

# Runs over each EVEOnline_MER_* directory (* a month and year); each directory
# contains one Killdump.csv. The script renames the file to YYYYMMMKilldump.csv
# where YYYY is the year and MMM is the month's abbreviation, and moves it to
# the killdumps directory.
# 
# It also deletes any row whose destroyedShipType is "Capsule" and deletes the
# columns whose entries are numeric for internal identification:
#   victimCorporationID             (the first column)
#   finalCorporationID              (the fourth column)
#   destroyedShipTypeID             (the seventh column)
#   solarSystemID                   (the eleventh column)
#   regionID                        (the thirteenth column)

import glob
import csv
import os
import re

basePath = os.path.dirname(os.path.realpath(__file__))

# Get the parent directory
def getParentDir(directory):
    return os.path.dirname(directory)

killDumpPath = basePath+"/killdumps"

for theDir in glob.iglob(basePath+'/EVEOnline_MER_*'):
    # Enter directory 
    os.chdir(theDir)
    parentDir = getParentDir(theDir)

    # Obtain the month and year from the current directory
    theYear = re.findall('\d+',theDir)[0]
    theMonth = (re.findall('[A-Z][a-z][a-z]\d',theDir)[0])[:-1]
        # some explanation: Finds capital letter followed by two lowercase
        # then a digit (to avoid 'Onl'); then deletes the digit
    
    # Rename and move the file
    for file in glob.glob('Killdump.csv'):
        os.rename(file,killDumpPath+'/'+theYear+theMonth+'Killdump.csv')

    # Return to main directory
    os.chdir(parentDir)

# Enter killdumps directory
os.chdir(killDumpPath)

# Perform data modification
for file in os.listdir(os.getcwd()):
    with open(file) as inp, open(file[:-4]+'_edited.csv','w') as out:
        writer = csv.writer(out)
        for r in csv.reader(inp):
            # Write only the desired rows and columns
            if r[8] != "Capsule":
                writer.writerow( (r[1],r[2],r[4],r[5],r[7],r[8],r[9],r[11],
                        r[13],r[14],r[15],r[16]) )
