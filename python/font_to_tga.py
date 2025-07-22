import sys                                      # enable reading of command line filename

bitlist = list()                                # holds ones and zeroes
bytelist = list()                               # holds ones and zeroes converted to bytes
row = list()
header = [0,0,3,0,0,0,0,0,0,0,0,0,128,0,128,0,8,0]
imageslice = list()                             # store vertical slice of the image
imagearray = list()                             # store all the slices of the image

## read image and convert it to ones and zeroes ###################################################

f = open(sys.argv[1], 'rb')                     # read file specified as first argument

with f:
    while (byte := f.read(1)):                  # walrus operator to read all bytes from file
            bytelist.append(byte)               # if value is above zero, append one

f.close()                                       # close input file

for r in range(16):
    for i in range(8):
        for x in range(16):
            row.append(bytelist[x*8+i+(128*r)])

for i in range(len(row)):
    for x in range(8):
        bitlist.append(int(format(int.from_bytes(row[i]), '08b')[x])*255)

## store imageslices to an array ##################################################################

for y in range(128):
    for x in range(128):
        imageslice.append(bitlist[y*128+x])
    imagearray.append(imageslice)
    imageslice = []                             # empty the slice for new row

imagearray.reverse()                            # tga is written from bottom to top so rows need to be reversed

## write out data in binary form ##################################################################

newFile = open("font.tga", "wb")
for i in range(len(header)):
    newFile.write(header[i].to_bytes(1, byteorder='big'))
for x in range(128):
    for y in range(128):
        newFile.write(imagearray[x][y].to_bytes(1, byteorder='big'))

newFile.close()

print(header)

## print debug info ###############################################################################

#print("  sprite resolution\t",spritex,"*",spritey)
#print("  image resolution\t",sizex,"*",sizey)
#print("  sprite array\t\t",int(sizex/spritex),"*",int(sizey/spritey))
