FNAME=$1
ONAME=${FNAME%.*}
ONAME1=${ONAME##*/}
ANAME="${ONAME1}.s"

make
./compile $FNAME
arm-linux-gnueabi-gcc -o $ONAME1 -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $ANAME
qemu-arm -L /usr/arm-linux-gnueabi/ $ONAME1
echo $?

