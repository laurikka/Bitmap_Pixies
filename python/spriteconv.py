import sys                                      # enable reading of command line filename
#import os

bitlist = list()                                # holds ones and zeroes
bytelist = list()                               # holds ones and zeroes converted to bytes
imageslice = list()                             # store vertical slice of the image
imagearray = list()                             # store all the slices of the image
cell = list()                                   # store individual sprite
blocks = list()                                 # store array of individual sprites
spritex = 24                                    # x-res of sprite
spritey = 21                                    # y-res of sprite
temp = 0


## read image and convert it to ones and zeroes ###################################################

newname = sys.argv[1].split(".")[0]+".bin"

f = open(sys.argv[1], 'rb')                     # read file specified as first argument

header = f.read(18)                             # get tga-header
sizex = header[12] + (header[13]*256)           # extraxt x-res of image
sizey = header[14] + (header[15]*256)           # extraxt y-res of image

f.seek(18)                                      # skip tga-header

with f:
    while (byte := f.read(1)):                  # walrus operator to read all bytes from file
        if (temp%3==0):                         # read values only from red channel of tga
            if int.from_bytes(byte, "big")  > 1:
                bitlist.append(1)               # if value is above zero, append one
            else:
                bitlist.append(0)               # else append zero
        temp+=1                                 # increase every loop for modulo check

f.close()                                       # close input file


## store imageslices to an array ##################################################################

for y in range(int(sizey)):
    for x in range(int(sizex)):
        imageslice.append(bitlist[y*sizex+x])
    imagearray.append(imageslice)
    imageslice = []                             # empty the slice for new row

imagearray.reverse()                            # tga is written from bottom to top so rows need to be reversed


## convert slices and rows to an array of sprite cells ############################################

for x in range(int(sizex/spritex)):             # store sprite-cells to block-array
    for y in range(int(sizey/spritey)):
        for yc in range(spritey):
            for xc in range(spritex):
                cell.append(imagearray[y*spritey+yc][x*spritex+xc])
        blocks.append(cell)
        cell = []


## convert bits to bytes ##########################################################################

for x in range(len(blocks)):
    for i in range(int(spritex*spritey/8)):
        bytelist.append((128*blocks[x][i*8])+(64*blocks[x][(i*8)+1])+(32*blocks[x][(i*8)+2])+(16*blocks[x][(i*8)+3])+(8*blocks[x][(i*8)+4])+(4*blocks[x][(i*8)+5])+(2*blocks[x][(i*8)+6])+(blocks[x][(i*8)+7]))
    bytelist.append(0)


## write out data in binary form ##################################################################

newFile = open(sys.argv[2], "wb")
for i in range(len(bytelist)):
    newFile.write(bytelist[i].to_bytes(1, byteorder='big'))

newFile.close()


## print debug info ###############################################################################

print("  sprite resolution\t",spritex,"*",spritey)
print("  image resolution\t",sizex,"*",sizey)
print("  sprite array\t\t",int(sizex/spritex),"*",int(sizey/spritey))
print("  filename\t\t",newname)