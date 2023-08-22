FNAME=$1
NAME=${FNAME%.*}
NAME1=${NAME##*/}
ANAME="${NAME1}.s"
ONAME="${NAME1}.o"

make
./compile $FNAME
gcc -c -g $ANAME -o $ONAME
gcc $ONAME -o $NAME1
./$NAME1
echo $?
