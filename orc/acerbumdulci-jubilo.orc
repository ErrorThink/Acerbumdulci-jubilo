sr=44100
kr=4410
ksmps=10
nchnls=2
0dbfs = 1

gk_tempo init 60
gk_now init 0

chnset 1, "MasterLevel"

gi_31edo ftgen 0,0,64,-2, 31, 2, 263, 0,\
1, 2^(1/31) ,2^(2/31),2^(3/31),2^(4/31),2^(5/31),2^(6/31),2^(7/31),2^(8/31),2^(9/31),2^(10/31),2^(11/31),2^(12/31),2^(13/31),2^(14/31),2^(15/31),2^(16/31),2^(17/31),2^(18/31),2^(19/31),2^(20/31),2^(21/31),2^(22/31),2^(23/31),2^(24/31),2^(25/31),2^(26/31),2^(27/31),2^(28/31),2^(29/31),2^(30/31),2

;                    d,e, f  g  a  b
;                    5,10,13,18,23,28, 
gi31Diatonic[] array 5,5,3,5,5,5,3
gk31Diatonic[] array 5,5,3,5,5,5,3
;Meantone[7]; diatonic scale

;
gi31NaturalMinor[] array 5,3,5,5,3,5,5
gi31DiaDom7[] array 5,5,3,5,5,2,6
gi31MinDom7[] array 5,3,5,5,3,4,6

gi31Neutral[] array 4,5,4,5,4,5,4

;more than 7 notes!
gi31Orwell[] array 4,3,4,3,4,3,4,3,3

;gi31Diminished[] array 3,7,1,7,3,5,5
gi31Diminished[] array 4,2,6,4,2,6,7

;create a matix of modes based on each 31edo pitch
gi31Modes[][] init 31,7

i31ndx = 0
until (i31ndx == lenarray(gi31Modes, 1)) do
    i7ndx = 0
    until (i7ndx == lenarray(gi31Modes, 2)) do
        if (i31ndx == 0 || i31ndx == 13 || i31ndx == 18 || i31ndx == 25 ||
            i31ndx == 3 || i31ndx == 16 || i31ndx == 21 || i31ndx == 28 ||
            i31ndx == 6 || i31ndx == 19 || i31ndx == 24) then
            gi31Modes [i31ndx][i7ndx] = gi31DiaDom7[i7ndx]
        elseif (i31ndx == 5 || i31ndx == 10 || i31ndx == 23 ||
                i31ndx == 8 || i31ndx == 13 || i31ndx == 26 ||
                i31ndx == 11 || i31ndx == 29) then
            gi31Modes [i31ndx][i7ndx] = gi31MinDom7[i7ndx]
        elseif (i31ndx == 2 || i31ndx == 4 || i31ndx == 7 || i31ndx == 9 ||
                i31ndx == 14 || i31ndx == 17 || i31ndx == 20 || i31ndx == 22 || i31ndx == 27) then
            gi31Modes [i31ndx][i7ndx] = gi31Neutral[i7ndx]            
        elseif (i31ndx == 1 || i31ndx == 12 || i31ndx == 15 || i31ndx == 28 || i31ndx == 30) then
            gi31Modes [i31ndx][i7ndx] = gi31Diminished[i7ndx]
        endif
        i7ndx += 1
    od
i31ndx += 1
od

gi_SuperScale = gi_31edo
gi_CurrentScale ftgen 0,0,128,-2,7, 2,    263, 0,
(table:i(4, gi_SuperScale)),
(table:i(9, gi_SuperScale)), 
(table:i(14, gi_SuperScale)), 
(table:i(17, gi_SuperScale)), 
(table:i(22, gi_SuperScale)),
(table:i(27, gi_SuperScale)),
(table:i(32, gi_SuperScale)),
(table:i(35, gi_SuperScale))

gkTonic_ndx init 0  ;index of the tonic in gi_CurrentScale

;foraltfm
gi_altfm ftgen 0, 0, 17, 2, 0, .2, .3, 0.8, 1.0, 0.8, 0.5, 0.2, 0, -0.3, -0.5, -0.8, -1.0, -0.9, -0.7, -0.3, 0 ;casio
seed 0

opcode minarray2, i,i[]
iInarray[] xin
iresult = iInarray[0]
indx init 1
until indx == lenarray(iInarray) do
    ival = iInarray[indx]
    if ival < iresult then
       iresult = ival
    endif
    indx += 1
od
xout iresult
endop

opcode minarray2, ii,i[]
iInarray[] xin
indxlow = 0
iresult = iInarray[indxlow]
indx = 1
until indx == lenarray(iInarray) do
    ival = iInarray[indx]
    if ival < iresult then
       iresult = ival
       indxlow = indx
    endif
    indx += 1
od
xout iresult, indxlow
endop

opcode maxarray2, i,i[]
iInarray[] xin
iresult = iInarray[0]
indx init 1
until indx == lenarray(iInarray) do
    ival = iInarray[indx]
    if ival > iresult then
       iresult = ival
    endif
    indx += 1
od
xout iresult
endop

opcode maxarray2, ii,i[]
iInarray[] xin
indxhigh = 0
iresult = iInarray[indxhigh]
indx = 1
until indx == lenarray(iInarray) do
    ival = iInarray[indx]
    if ival > iresult then
       iresult = ival
       indxhigh = indx
    endif
    indx += 1
od
xout iresult, indxhigh
endop

opcode minarray2, k,k[]
kInarray[] xin
kresult = kInarray[0]
kndx init 1
until kndx == lenarray(kInarray) do
    kval = kInarray[kndx]
    if kval < kresult then
       kresult = kval
    endif
    kndx += 1
od
xout kresult
endop

opcode minarray2, kk,k[]
kInarray[] xin
kndxlow init 0
kresult = kInarray[kndxlow]
kndx init 1
until kndx == lenarray(kInarray) do
    kval = kInarray[kndx]
    if kval < kresult then
       kresult = kval
       kndxlow = kndx
    endif
    kndx += 1
od
xout kresult, kndxlow
endop

opcode maxarray2, k,k[]
kInarray[] xin
kresult = kInarray[0]
kndx init 1
until kndx == lenarray(kInarray) do
    kval = kInarray[kndx]
    if kval > kresult then
       kresult = kval
    endif
    kndx += 1
od
xout kresult
endop

opcode maxarray2, kk,k[]
kInarray[] xin
kndxhigh init 0
kresult = kInarray[kndxhigh]
kndx init 1
until kndx == lenarray(kInarray) do
    kval = kInarray[kndx]
    if kval > kresult then
       kresult = kval
       kndxhigh = kndx
    endif
    kndx += 1
od
xout kresult, kndxhigh
endop

;wrapper around buggy sumarray
opcode sumarray2, i,i[]
iArr[] xin
iresult = 0
indx = 0
ilen lenarray iArr
until (indx == ilen) do
    iresult += iArr[indx]
    indx += 1
od
xout iresult
endop

opcode sumarray2, k,k[]
kArr[] xin
kresult init 0
kndx init 0
klen lenarray kArr
until (kndx == klen) do
    kresult += kArr[kndx]
    kndx += 1
od
xout kresult
endop


;a wrapper to for my preferred indexing around slicearray
opcode slicearray2, i[],i[]iip
  iArr[],ibeg,iend,istride xin  
  iend -= 1
iresult[] init (iend <= 0 ? 1:iend)
iresult[] slicearray iArr, ibeg, iend, istride
xout iresult
endop

;a wrapper to fix index 0 slicearray
opcode slicearray2, k[],k[]iip
kArr[],ibeg,iend,istride xin
iend -= 1
kresult[] slicearray kArr, ibeg, iend, istride
xout kresult
endop

opcode get2dArr, i[], ii[][]
;returns the array from the 2nd dimension of a 2d array
indx, imulti[][] xin
icount = 0
ilen lenarray imulti, 2
iout[] init ilen
until (icount == ilen) do
    iout[icount] = imulti[indx][icount]
    icount += 1
od
xout iout
endop

;very forgiving array indexing
;indices wrap around array length.
;negative indices count backwards from array kength
;note that just using wrap isn't suitable as wrap(-n,0,n) == n
;example
;iArr fillarray 4.5,4.6,4.7,4.8
;ndxarray(iArr,2.999) => 4.7
;ndxarray(iArr,5) => 4.6
;ndxarray(iArr,-2) => 4.7
opcode ndxarray, i,i[]i
iArr[], indx xin
ilen lenarray iArr
iresult = int(indx)
if (iresult >= ilen) then
   iresult = wrap(iresult, 0, ilen)
elseif (iresult < 0) then
   if (iresult = -ilen) then
      iresult = 0
   else
      iresult = wrap(iresult, 0, ilen)
   endif
endif
xout iArr[iresult]
endop


opcode ndxarray, k,k[]k
kArr[], kndx xin
klen lenarray kArr
kresult = int(kndx)
if (kresult >= klen) then
   kresult = wrap(kresult, 0, klen)
elseif (kresult < 0) then
   if (kresult = -klen) then
      kresult = 0
   else
      kresult = wrap(kresult, 0, klen)
   endif
endif
xout kArr[kresult]
endop


opcode membership, i, ik[]
;returns index position of ival, or  -1 if false
ival, kselection[] xin

iresult = -1
indx = 0
until (indx == lenarray(kselection)) do

if (ival == i(kselection, indx)) then
iresult = indx
endif
indx += 1
od

xout iresult
endop

opcode membership, i, ii[]
ival, iselection[] xin
iresult = -1
indx = 0
until (indx == lenarray(iselection)) do

if (ival == iselection[indx]) then
iresult = indx
endif
indx += 1
od

xout iresult
endop

;converts a karray to an iarray
opcode castarray, i[],k[]
kArr[] xin

iArr[] init lenarray(kArr)
indx = 0
until indx == lenarray:i(iArr) do
   ival = i(kArr, indx)
   iArr[indx] = ival
   indx += 1 
od
xout iArr
endop

opcode sortarray, k[], k[]
;slightly modified from CsUDO repository
kInArr[] xin
  kOutArr[]  init  lenarray(kInArr)
  kMax      maxarray2  kInArr
  kIndx     init  0
  until     kIndx == lenarray(kOutArr) do
      kMin, kMinIndx minarray2 kInArr
      kOutArr[kIndx] = kInArr[kMinIndx]
      kInArr[kMinIndx] = kMax+1
      kIndx += 1
  od
xout       kOutArr
endop

;irate version
opcode sortarray, i[], i[]
;slightly modified from CsUDO repository
iInArr[] xin
  iOutArr[]  init  lenarray(iInArr)
  iMax      maxarray2  iInArr
  iIndx     init  0
  until     iIndx == lenarray(iOutArr) do
      iMin, iMinIndx minarray2 iInArr
      iOutArr[iIndx] = iInArr[iMinIndx]
      iInArr[iMinIndx] = iMax+1
      iIndx += 1
  od
xout       iOutArr
endop


;truncatearray, shorten or extend an array.
;when extending, values in the input array are
;repeated until k/ilen is reached.
opcode truncatearray, k[],k[]k
  kArr[],klen xin
  kResult[] init i(klen)
  kndx = 0
  kval = 0
  until (kndx >= klen) do
    kval = kArr[kndx % lenarray(kArr)]
    kResult[kndx] = kval
    kndx += 1
  od
  xout kResult
endop


opcode truncatearray, i[],i[]i
  iArr[],ilen xin
  iResult[] init ilen
  indx = 0
  ival = 0
  until (indx >= ilen) do
    ival = iArr[indx % lenarray(iArr)]
    iResult[indx] = ival
    indx += 1
  od
  xout iResult
endop


;random choices from an array.
;The first array (kchoices[]) is the selection of values
;the second array (kweights) specifies relative probability of selecting
;the values in kchoices at the same indices.
;note that kweights will match the length of kchoices using the truncatearray opcode.
opcode weightedchoice, i,k[]k[]
kchoices[], kweights[]xin
  iweightsa[] castarray kweights
  ichoices[] castarray kchoices
  ilen lenarray ichoices
  indx = 0
  iweights[] truncatearray iweightsa, ilen
  isum sumarray2 iweights
  irnd random 0, isum
  iupto = 0
  until (indx >= ilen) do
    ic = ichoices[indx]
    iw = iweights[indx % lenarray(iweights)]
    if ((iupto + iw) >= irnd) then
      igoto RESULT
    else
      iupto += iw
    endif
    indx += 1
  od
  RESULT:
  xout ic
endop



;returns an array of differences between values in an array.
;e.g. arraydeltas(fillarray(2,3,2,1))
;returns an array of 2,1,-1,-1
opcode arraydeltas, k[],k[]
kinArr[] xin
koutArr[] init lenarray(kinArr) - 1
kndx = 1
kval = 0
klastval = kinArr[0]
until (kndx == lenarray(kinArr)) do
    kval = kinArr[kndx]
    koutval = kval - klastval
    koutArr[kndx - 1] = koutval
    klastval = kval
    kndx += 1
od
xout koutArr
endop

opcode arraydeltas, i[],i[]
iinArr[] xin
ioutArr[] init lenarray(iinArr) - 1
indx = 1
ival = 0
ilastval = iinArr[0]
until (indx == lenarray(iinArr)) do
    ival = iinArr[indx]
    ioutval = ival - ilastval
    ioutArr[indx - 1] = ioutval
    ilastval = ival
    indx += 1
od
xout ioutArr
endop

;;
opcode rotatearray, i[],i[]i
iinArr[], ishift  xin

ilen lenarray iinArr
ioutArr[] init ilen
indx = 0
until (indx == ilen) do
;    ioutArr[indx] = iinArr[(indx + ishift) % ilen]
    ioutArr[indx] = ndxarray(iinArr, (indx + ishift))
    indx += 1
od

xout ioutArr
endop

opcode rotatearray, k[],k[]i
kinArr[], ishift  xin
klen init lenarray(kinArr)
koutArr[] init i(klen)
kndx = 0
until (kndx == klen) do
;    koutArr[kndx] = kinArr[(kndx + ishift) % klen]
    koutArr[kndx] = ndxarray(kinArr, (kndx + ishift))
    kndx += 1
od

xout koutArr
endop


;compares the contents of two tables
;returns 1 if the tables are identical, or 0 if there are differences.
opcode tableeqv, i,ii
iaft, ibft xin

ialen tableng iaft
iblen tableng ibft
ishortlen = (ialen < iblen ? ialen : iblen)
iresult = 1
indx = 0
until (indx == ishortlen) do
    iaval tab_i indx, iaft
    ibval tab_i indx, ibft
    cigoto (iaval != ibval), EXITFAIL
    indx += 1
od
igoto EXIT
EXITFAIL:
iresult = 0
EXIT:
xout iresult
endop

;compares values stored in two ftables.
;returns an array of indices where values stored in
;ifta match values in iftb
;iastart, ibstart and iaend, ibend select a sections of the tables to examine.
;defaults are zero - whole tables are compared
opcode matchindices, i[],iioooo
iaft, ibft, iastart, ibstart, iaend, ibend xin
if (iaend == 0) then
   iaend tableng iaft
endif
if (ibend == 0) then
   ibend tableng ibft
endif
iresult[] init (iaend - iastart)
ifound = 0
until (iastart >= iaend) do
    iaval tab_i iastart, iaft
    ibndx = ibstart
    until (ibndx >= ibend) do
        ibval tab_i ibndx, ibft
        if (ibval == iaval) then
           iresult[ifound] = ibndx - ibstart
           ifound += 1
        endif
        ibndx += 1
    od
    iastart += 1
od
xout iresult
endop


;irate version
opcode scalemode, 0, iii[]
;sets the current scale to a mode from superscale
; scalemode gi_12edo, 2, array(0,2,4,5,7,9,11)
;returns position of keyctr in new mode, and sets gk_tonicndx
isuperscale, ikeyctr, iSuperscaleIntervalPattern[] xin

isubpattern[] init lenarray(iSuperscaleIntervalPattern)
isubpattern[0] = 0
isubpndx init 1

until (isubpndx == lenarray(isubpattern)) do
isubpattern[isubpndx] = isubpattern[isubpndx - 1] + iSuperscaleIntervalPattern[isubpndx - 1]
isubpndx += 1
od

inumgrades table 0, isuperscale
iindinums[] init lenarray(isubpattern)
indx init 0
ilast init 0
iscaleroot init 0
iscalekeyctrndx init -1
ival init 0

iindices[] = iindinums

until indx == lenarray(iindices) do
ival = isubpattern[indx]
iindices[indx] = (ival + ikeyctr) % inumgrades
indx += 1
od

ilastvalfromsuper table inumgrades + 4, isuperscale
imodularval table ikeyctr + 4, isuperscale
;ilastval = imodularval * ilastvalfromsuper


isorted[] sortarray iindices ; 
iscaleroot = isorted[0]

ilen init 0
ilen lenarray isorted
tableiw ilen, 0, gi_CurrentScale
;tableiw ilastval, ilen + 4, gi_CurrentScale

indx = 0
until (indx == ilen) do
  isortedndx = isorted[indx]
  irevisedval table isortedndx + 4, isuperscale
  tableiw irevisedval, indx + 4, gi_CurrentScale
  if (irevisedval == imodularval) then
      iscalekeyctrndx = indx
  endif
  indx += 1
od
tableiw table:i(4,gi_CurrentScale) * ilastvalfromsuper, ilen + 4, gi_CurrentScale

giTonic_ndx = iscalekeyctrndx

if (gi_SuperScale == 0) then
   gi_SuperScale = isuperscale
elseif (tableeqv(gi_SuperScale, isuperscale) == 0) then
   gi_SuperScale = isuperscale
   ;tablecopy gi_SuperScale, isuperscalefn
else
   ;pass
endif   
endop

opcode scalemode, 0, iik[]
;This is just a wrapper for the irate array version 
;but is useful when using inline arrays which default to krate.
;e.g. scalemode gi_31edo, 0, array(5,5,3,5,5,5,3)
isuperscale, ikeyctr, kSuperscaleIntervalPattern[] xin
iSuperscaleIntervalPattern[] castarray kSuperscaleIntervalPattern
scalemode isuperscale, ikeyctr, iSuperscaleIntervalPattern
endop

;31edo version
opcode scalemode31, 0, io
;wrapper just for the 31edo mode matrix
;imode 0=default, 3=DiaDom7, 4=MinDom7, 1=Diatonic, 2=Minor,5=Diminished,6=neutral,7=Orwell[9]
idegree, imode  xin
if (imode == 0) then
  scalemode gi_31edo, idegree, get2dArr(idegree, gi31Modes)
elseif (imode == 3) then
  scalemode gi_31edo, idegree, gi31DiaDom7
elseif (imode == 4) then
  scalemode gi_31edo, idegree, gi31MinDom7
elseif (imode == 1) then
  scalemode gi_31edo, idegree, gi31Diatonic
elseif (imode == 2) then
  scalemode gi_31edo, idegree, gi31NaturalMinor
elseif (imode == 5) then
  scalemode gi_31edo, idegree, gi31Diminished
elseif (imode == 6) then
  scalemode gi_31edo, idegree, gi31Neutral
elseif (imode == 7) then
  scalemode gi_31edo, idegree, gi31Orwell
else
  scalemode gi_31edo, idegree, get2dArr(idegree, gi31Modes)
endif
endop

opcode now, i, 0
xout i(gk_now)
endop

opcode cosr, i, ijo
;translated from impromptu's cosr macro
;does a complete cosine cycle, with multiplier and offset for convenience
;iamp defaults to 0.5
;ioffset defaults to 0.5 if iamp is not set, otherwise defaults to 1
iperiod, iamp, ioffset xin

if (iamp == -1) then
 iamp = 0.5
 ioffset = 0.5
endif

xout cos(divz:i(1,iperiod,0) * (2 * $M_PI) * now()) * iamp + ioffset
endop



;alternative version, doesn't use an event.
opcode temposet, 0, i
ibpm xin
gk_tempo init ibpm
endop

opcode tempoget, i, 0
xout i(gk_tempo)
endop

opcode tempodur, i, p
idur xin
iresult divz 60, i(gk_tempo), -1
xout iresult * idur
endop

opcode tempodur_k, k, k
kdur xin
kresult divz 60, gk_tempo, -1
xout kresult * kdur
endop


opcode nextbeat, i, p
ibeatcount xin
if ibeatcount == 0 then
iresult = 0
else
inow = now()
ibc = frac(ibeatcount)
inudge = int(ibeatcount)
iresult = inudge + ibc + (round(divz(inow, ibc, inow)) * (ibc == 0 ? 1 : ibc)) - inow
endif
xout tempodur(iresult)
endop

opcode counterChan, i, Spoo
;state saving counter. 
Schan, iincrement, imodulo, ilower  xin
icurrent chnget Schan
if (imodulo != 0) then
   icurrent = wrap(icurrent, ilower, imodulo)
   inewval = wrap((icurrent + iincrement), ilower, imodulo)
else
   inewval = icurrent + iincrement
endif
chnset inewval, Schan
xout icurrent
endop

opcode counterChan, K, SPOO
;state saving counter. mightneed once code
Schan, kincrement, kmodulo, klower  xin
konce init 0
ckgoto (konce == 1), terminate
kcurrent chnget Schan
if (kmodulo != 0) then
   kcurrent = wrap(kcurrent, klower, kmodulo)
   knewval = wrap((kcurrent + kincrement), klower, kmodulo)
else
   knewval = kcurrent + kincrement
endif
chnset knewval, Schan
konce = 1
terminate:
xout kcurrent
endop


;returns successive values in an array on every call
;works with irate arrays
;Sid is a string (any unique string), to store the state of each instance.
opcode iterArr, i,i[]Sp
iArr[], Sid, idirection xin
ival counterChan Sid, idirection, lenarray(iArr); [, iincrement, imodulo, ilower]
iout = iArr[floor(ival)]
xout iout
endop

;version for krate (inline) arrays
opcode iterArr, i,k[]Sp
kArr[], Sid, idirection xin
ival counterChan Sid, idirection, lenarray(kArr); [, iincrement, imodulo, ilower]
iout = i(kArr, floor(ival))
xout iout
endop

opcode randselect_i, i, ijjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj
ival1, ival2, ival3, ival4, ival5, ival6, ival7, ival8, ival9, ival10, ival11, ival12, ival13, ival14, ival15, ival16, \
ival17, ival18, ival19, ival20, ival21, ival22, ival23, ival24, ival25, ival26, ival27, ival28, ival29, ival30, ival31, ival32 xin

iargArray[] array ival32, ival31, ival30, ival29, ival28, ival27, ival26, ival25, ival24, ival23, ival22, ival21, ival20, ival19, ival18, ival17, ival16, ival15, ival14, ival13, ival12, ival11, ival10, ival9, ival8, ival7, ival6, ival5, ival4, ival3, ival2, ival1

istart = 0
until (iargArray[istart] != -1) do
istart += 1
od

iout = iargArray[int(random(istart, lenarray(iargArray)))]

xout iout
endop

opcode rescale, i, iiiii
ival, ioldmin, ioldmax, inewmin, inewmax  xin

ioldrange = ioldmax - ioldmin
inewrange = inewmax - inewmin
ioldsize = ival - ioldmin

xout ((inewrange / ioldrange) * ioldsize) + inewmin

endop

opcode rescalek, k, kkkkk
kval, koldmin, koldmax, knewmin, knewmax  xin

koldrange = koldmax - koldmin
knewrange = knewmax - knewmin
koldsize = kval - koldmin

xout ((knewrange / koldrange) * koldsize) + knewmin

endop

opcode accumarray, k[], k[]
kArrSrc[] xin

kArrRes[] init lenarray(kArrSrc)

konce init 0
ckgoto (konce == 1), terminate

kndx    init       0

ksum init 0
until (kndx == lenarray(kArrSrc)) do
  kresult   =  ksum + kArrSrc[kndx]
  kArrRes[kndx] = kresult
  ksum      =  kresult
  kndx     = kndx + 1
od

konce = 1

terminate:
xout kArrRes
endop


opcode onsetArrayLength_i, i, k[]KO
;return the the minimum length required to build an onset array, with overlap
kArrSrc[], kdur, koverlap xin
isum = 0
indx = 0

ilenSrc lenarray kArrSrc
until (indx >= ilenSrc) do
  isum      +=        i(kArrSrc, indx)
  indx      +=        1
od

isum = isum + i(koverlap)

iresult = ceil(i(kdur) / isum)
inewlen = iresult * ilenSrc 

isumtruncated = iresult * isum 
istepback = 0

if (isumtruncated > i(kdur)) then 
isumtruncated -= i(koverlap) 
endif

until (isumtruncated <= i(kdur)) do
  istepback +=        1 
  ilook     =  (ilenSrc - istepback) % ilenSrc 
  isumtruncated       -=  i(kArrSrc, ilook)
od
inewlen -= istepback 

xout inewlen
endop


opcode onsetaccum, k[], k[]KO
;return an onset array
kArrSrc[], kdur, koverlap xin
ionsetlength onsetArrayLength_i kArrSrc, kdur, koverlap ;
kArrRes[] init ionsetlength
iSrclen lenarray kArrSrc

kndx init 0
ksum init 0
ktrythisndx init 0  ;must re-init on subsequent calls.

while (kndx < ionsetlength) do
  ktrythisndx = kndx % iSrclen
  ksum      +=        kArrSrc[ktrythisndx]
  kArrRes[kndx] = ksum
  kndx      +=        1
  if ktrythisndx == 0 then
     ksum      +=        koverlap
  endif 
od

xout kArrRes
endop


opcode lazyvent, 0, kkkkkkkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7, kp8, kp9, kp10, kp11, kp12 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7, kp8, kp9, kp10, kp11, kp12
endop

opcode lazyvent, 0, kkkkkkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7, kp8, kp9, kp10, kp11 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7, kp8, kp9, kp10, kp11
endop

opcode lazyvent, 0, kkkkkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7, kp8, kp9, kp10 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7, kp8, kp9, kp10
endop

opcode lazyvent, 0, kkkkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7, kp8, kp9 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7, kp8, kp9
endop

opcode lazyvent, 0, kkkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7, kp8 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7, kp8
endop

opcode lazyvent, 0, kkkkkkk
knno, ksta, kdur, kamp, kpit, kp6, kp7 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6, kp7
endop

opcode lazyvent, 0, kkkkkk
knno, ksta, kdur, kamp, kpit, kp6 xin
event "i", 4, ksta, kdur, knno, kamp, kpit, kp6
endop

opcode lazyvent, 0, kkkkk
knno, ksta, kdur, kamp, kpit xin
event "i", 4, ksta, kdur, knno, kamp, kpit
endop

; ornament UDO
;   koriginal[], \
;   kwhens[], kdurs[], kamps[], kintervals[] [, kp6] [,kp7] [,...]\
;   korndur, kscale, koverlap xin

;   also... overloaded versions for convenience...
;   koriginal[], \
;   kwhens[], kamps[], kintervals[] [, kp6] [,kp7] [,...]\
;   korndur, kscale, koverlap xin

;   koriginal[], \
;   kwhens[], kintervals[] [, kp6] [,kp7] [,...]\
;   korndur, kscale, koverlap xin

;   koriginal[], \
;   iwhens, kintervals[] [, kp6] [,kp7] [,...]\
;   korndur, kscale, koverlap xin

; Generates a sequence of score events 
; 
; koriginal[] is an array of values which are treated like pfields in a score event.
; e.g. array(101, 0, 1, 0.8, 5) is equivalent to i101 0 1 0.8 5
;
;kwhens is an array specifying rhythmic intervals between generated events.

;kdurs[], kamps[], kintervals[], kp6[] etc... are numeric arrays. 
;The values in the arrays specify deviations in the generated events from the corresponding pfield in koriginal. 
;The deviations accumulate and are applied in a looped sequence for the length of orndur.
;For example using, kintervals[] array 2,-2, 1  will cause pitches in the generated events to ascend by 2steps, fall by 2steps, then ascend by 1, and repeat until orndur is reached.
;to keep a pfield constant, use array(0). To loop a set of values, ensure the sum of the array values equals zero.

; korndur sets the duration of the entire ornament. Defaults to the calling instruments p3.

; kscale is a table used by cpstun. defaults to gi_CurrentScale

; kpitbound sets upper or lower bounds to pitch generation. 
; If the sum of kintervals is positive an upper bound is set, with the lower bound being the pitch in koriginal.
; The reverse situation operates when the sum of kintervals is negative: i.e. a lower bound is set, with the upper bound being the pitch in koriginal.
; A positive kpitbound refects pitches using mirror. A negative kpitbound uses wrap.
; Default is 0 - no pitch boundaries.
;EXAMPLE
/*
instr 101

ares bellpno p4, p5, p6, p7, p8;kamp, ifrq, imod [, ir1, ir2]

ares declickr ares
chnmix ares, "outputC" 

endin

instr 11

ornament array(101, 0, 0.4, 0.2, -7, 2, 1, 1), array(0.125, 0.125, 0.125, randselect_i(0.125)), array(0), array(0), array(1), array(-0.1),\
array(0), array(1, -1), array(0), array(0),\
1.25,0

schedule p1, nextbeat(3), 5
turnoff
endin

schedule 11, nextbeat(1), 1
*/

opcode ornmaster, 0, k[]k[]k[]k[]k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], kp7[], kp8[], kp9[], kp10[], \
  kpitbound, korndur, kscale xin

  ;a negative p1 is special, and forces instance instrument numbers  to increment
  kinsincr = (koriginal[0] < 0 ? 0.01:0)
; defaults to p3, nb: negative orndur is special and overides durvals  
  if (i(korndur) == 0) then
  korndur init p3
  endif

  kndx      init      0                           
  konce     init      0
  iplen lenarray koriginal
  ckgoto    (konce == 1), terminate     ; one pass... change this to a trigger? extra p. hmmm.

  kscale    =  (kscale == 0 ? gi_CurrentScale : kscale)
  kstarts[] onsetaccum   kwhens, init:k(abs(i(korndur))), 0
  kstartval =  tempodur_k(koriginal[1]) ; I think this is always overwritten... could be removed?
  kdurval   =  (korndur < 0 ? abs(korndur) - kstartval + koriginal[2] : koriginal[2]) ;if orndur is negative, then generated note durs converge to abs(orndur) - startval.
  kampval   =  koriginal[3]
  kpitval   =  koriginal[4]
  kdurndx   init      0
  kampndx   init      0
  kpitndx   init      0
  
  ;using this modulo purely to save on lots of conditionals
  ;the values are ignored if greater than iplen anyway
  kp6ndx    init      0  
  kp6val   =  koriginal[5 % iplen]
  kp7ndx    init      0
  kp7val   =  koriginal[6 % iplen]
  kp8ndx    init      0
  kp8val   =  koriginal[7 % iplen]
  kp9ndx    init      0
  kp9val   =  koriginal[8 % iplen]
  kp10ndx    init      0
  kp10val   =  koriginal[9 % iplen]

  until     (kndx >= lenarray(kstarts)) do
    kstartval =  tempodur_k(kstarts[kndx])

    if (korndur < 0) then
    kdurval = abs(korndur) - kstartval + koriginal[2]
    else
    kdurndx = kndx % lenarray(kdurs)
    kdurval   +=        tempodur_k(kdurs[kdurndx])
    	  
    endif

    kampndx = kndx % lenarray(kamps)
    kampval   +=        kamps[kampndx]
    kpitndx = kndx % lenarray(kintervals)
    kpitval   +=        kintervals[kpitndx]

    
    if (kpitbound != 0) then
       kdirection sumarray kintervals
       kboundlimit = abs(kpitbound)
       if (kdirection < 0) then
          kupper = maxarray2(kintervals) + koriginal[4]
          klower = koriginal[4] - kboundlimit
       elseif (kdirection > 0) then
          kupper = koriginal[4] + kboundlimit
          klower = minarray2(kintervals) + koriginal[4]
       else
          kupper = koriginal[4] + kboundlimit
          klower = koriginal[4] - kboundlimit
       endif
       if (kpitbound < 0) then
          kpitvalb wrap kpitval, klower, kupper
       elseif (kpitbound > 0) then
          kpitvalb mirror kpitval, klower, kupper
       endif
    else
       kpitvalb = kpitval
    endif


    if (iplen == 5) then
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitvalb, kscale)
    kgoto break
    endif
    kp6ndx = kndx % lenarray(kp6)
    kp6val    +=        kp6[kp6ndx]
    if (iplen == 6) then        
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitvalb, kscale), \
                   kp6val
    kgoto break
    endif
    kp7ndx = kndx % lenarray(kp7)
    kp7val    +=        kp7[kp7ndx]
    if (iplen == 7) then
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitvalb, kscale), \
                   kp6val, kp7val
    kgoto break
    endif
    kp8ndx = kndx % lenarray(kp8)
    kp8val    +=        kp8[kp8ndx]
    if (iplen == 8) then
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitvalb, kscale), \
                   kp6val, kp7val, kp8val
    kgoto break
    endif
    kp9ndx = kndx % lenarray(kp9)
    kp9val    +=        kp9[kp9ndx]
    if (iplen == 9) then
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitval, kscale), \
                   kp6val, kp7val, kp8val, kp9val
    kgoto break
    endif

    kp10ndx = kndx % lenarray(kp10)
    kp10val    +=        kp10[kp10ndx]
    if (iplen == 10) then
    event     "i", abs(koriginal[0]) + (kinsincr * (kndx + 1)), kstartval, (abs(kdurval) <= 0.0001 ? 0.0001 : kdurval), \
                  (kampval < 0 ? 0 : kampval), cpstun(1, kpitvalb, kscale), \
                   kp6val, kp7val, kp8val, kp9val, kp10val
    endif
   
    break:
    kndx      +=        1
  od
  konce = 1
  terminate:
endop

;ornament interface opcodes
;2 arrays and constant (i) rhythm
opcode ornament, 0, k[]ik[]OOO
  koriginal[], \
  iwhens, kintervals[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, array(iwhens), array(0), array(0), kintervals, array(0), array(0), array(0), array(0), array(0), kpitbound, korndur, kscale
endop

;3 arrays = original, whens, intervals only, durs and amps constant
opcode ornament, 0, k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kintervals[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, array(0), array(0), kintervals, array(0), array(0), array(0), array(0), array(0), kpitbound, korndur, kscale
endop

;4 arrays = original, whens, amps, intervals only - durs are constant
opcode ornament, 0, k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kamps[], kintervals[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, array(0), kamps, kintervals, array(0), array(0), array(0), array(0), array(0), kpitbound, korndur, kscale
endop

;p5
opcode ornament, 0, k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, array(0), array(0), array(0), array(0), array(0), kpitbound, korndur, kscale
endop


;p6
opcode ornament, 0, k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, kp6, array(0), array(0), array(0), array(0), kpitbound, korndur, kscale
endop

;p7
opcode ornament, 0, k[]k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], kp7[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, kp6, kp7, array(0), array(0), array(0), kpitbound, korndur, kscale
endop

;p8
opcode ornament, 0, k[]k[]k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], kp7[], kp8[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, kp6, kp7, kp8, array(0), array(0), kpitbound, korndur, kscale
endop

;p9
opcode ornament, 0, k[]k[]k[]k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], kp7[], kp8[], kp9[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, kp6, kp7, kp8, kp9, array(0), kpitbound, korndur, kscale
endop

;p10
opcode ornament, 0, k[]k[]k[]k[]k[]k[]k[]k[]k[]k[]OOO
  koriginal[], \
  kwhens[], kdurs[], kamps[], kintervals[], kp6[], kp7[], kp8[], kp9[], kp10[], \
  kpitbound, korndur, kscale xin
  ornmaster koriginal, kwhens, kdurs, kamps, kintervals, kp6, kp7, kp8, kp9, kp10, kpitbound, korndur, kscale
endop

;; opcode chordal,0,k[]k[]ooO
;; ;quick chords. koriginal holds the pfields on an event. (limit of 10 pfields)
;; ;kintervals specifies concurrant pitches with that event.
;; ;idbdamp reduces the p4 value as the number of notes increases. Assuming p4 is amplitude, a value of 1 reduces this by 3db every doubling of notes. Default is 0 (no reduction)
;; ;insincr is an increment added to p1 in each generated event. Useful for tied notes. If p3 in koriginal is negative, insincr defaults to 0.01
;; ;uses cpstun for pitches, with scale specified by kscale (defaults to gi_CurrentScale)
;; koriginal[],kintervals[],idbdamp,insincr,kscale xin
;;   kndx      init      0                           
;;   konce     init      0  
;;   ckgoto    (konce == 1), terminate     ; one pass
;;   insincr = (i(koriginal, 2) < 0 ? 0.01 : (insincr == 1 ? 0.01 : insincr))
;;   kscale    =  (kscale == 0 ? gi_CurrentScale : kscale)
;;   kpitval init 0
;;   ilen lenarray kintervals
;;   iplen lenarray koriginal
;;   iampfac = ampdbfs(-ilen*1.5*idbdamp); drop 3db every doubling.
;;   kdur    =  koriginal[2]
;;   until     (kndx >= ilen) do
;;     kpitval   =  koriginal[4] + kintervals[kndx]
;;     if (iplen == 5) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale)
;;     elseif (iplen == 6) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale), koriginal[5]
;;     elseif (iplen == 7) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale), koriginal[5], koriginal[6]
;;     elseif (iplen == 8) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7]
;;     elseif (iplen == 9) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7], koriginal[8]
;;     elseif (iplen == 10) then
;;       event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
;;                       cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7], koriginal[8], koriginal[9]
;;     endif

;;   kndx      +=        1
;;   od

;;   konce = 1
;;   terminate:

;; endop

opcode chordal,0,k[]k[]oojO
;quick chords. koriginal holds the pfields on an event. (limit of 10 pfields)
;kintervals specifies concurrant pitches with that event.
;idbdamp reduces the p4 value as the number of notes increases. Assuming p4 is amplitude, a value of 1 reduces this by 3db every doubling of notes. Default is 0 (no reduction)
;insincr is an increment added to p1 in each generated event. Useful for tied notes. If p3 in koriginal is negative, insincr defaults to 0.01
;iautoinvert constrains pitches to pitch classes within the octaves spanned by kintervals (centred on the iautoinvert scale degree).
;For example, kintervals = array(4,6,8), and iautoinvert = 0 in a diatonic major scale, then pitches 1,4,6 are generated. Defaults to -1 (no inversion).
;uses cpstun for pitches, with scale specified by kscale (defaults to gi_CurrentScale)
koriginal[],kintervals[],idbdamp,insincr,iautoinvert,kscale xin
  kndx      init      0                           
  konce     init      0  
  ckgoto    (konce == 1), terminate     ; one pass
  insincr = (i(koriginal, 2) < 0 ? 0.01 : (insincr == 1 ? 0.01 : insincr))
  kscale    =  (kscale == 0 ? gi_CurrentScale : kscale)
  kpitval init 0
  ilen lenarray kintervals
  iplen lenarray koriginal
  iampfac = ampdbfs(-ilen*1.5*idbdamp); drop 3db every doubling.
  kdur    =  koriginal[2]
  kautoinvert init iautoinvert  
  if (kautoinvert != -1) then
     koctdegs = tablekt(0,kscale)
     kmininterval = minarray2(kintervals)
     kmaxinterval = maxarray2(kintervals)
  endif
  until (kndx >= ilen) do
    kpitval   =  koriginal[4] + kintervals[kndx]
    if (kautoinvert != -1) then
        kpitval = wrap:k(kpitval,
              (floor(kmininterval / koctdegs) * koctdegs + kautoinvert),
              (ceil(kmaxinterval / koctdegs) * koctdegs + kautoinvert))
    endif
    if (iplen == 5) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale)
    elseif (iplen == 6) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale), koriginal[5]
    elseif (iplen == 7) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale), koriginal[5], koriginal[6]
    elseif (iplen == 8) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7]
    elseif (iplen == 9) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7], koriginal[8]
    elseif (iplen == 10) then
      event     "i", koriginal[0] + ((kndx + 1) * insincr), tempodur_k(koriginal[1]), tempodur_k(kdur), koriginal[3]*iampfac,\
                      cpstun(1,kpitval,kscale), koriginal[5], koriginal[6], koriginal[7], koriginal[8], koriginal[9]
    endif

  kndx      +=        1
  od

  konce = 1
  terminate:

endop


opcode arpeggiates, 0, k[]k[]k[]opo
;spreads notes in a chord by a time interval. 
;koriginal holds pfields of an event.
;kintervals are pitches aobve or below p5 in koriginal
;konsets are time intervals following p2 in koriginal.
;idur has 3 modes: idur = 0 (default) gives each note the value in the event.
;                  idur > 0 all notes expire at the same time: When the original event duration + idur is reached  
;                  negative idur makes a monophonic arpeggiation. durations are cut short so they don't overlap.
;ionsetfac compresses or expands the onset times of the arpeggiation. default is 1 (no compression)
;          negative ionset reduces or expands onset times throughout the duration of an arpeggiation. For example, -0.5 causes
;          an accelerando doubling tempo of notes by p3. A value of -1.5 halves the tempo. -1.0 leaves the tempo unchanged.   
;iampfac, applies a power curve to  p4 values throughout the duration of the arpeggiation.
;         positive values reduce p4 to zero (e.g. fade out).  
;         negative values increase p4 from zero to 1 (e.g. fade in).
;         Steepness of the curve increases as iampfac approaches 1 (or -1).
;         expected range is 0 to +-1, default is 0 (no modification applied).
  koriginal[],kintervals[],konsets[],idur, ionsetfac, iampfac xin
  kndx      init      0                           
  konce     init      0  
  ckgoto    (konce == 1), terminate

  kscale init  gi_CurrentScale

  kpitval init 0

  klen lenarray kintervals
  iplen lenarray koriginal
  konsetlength lenarray konsets

  kdur    init 0
  konset init 0
  konsetprogress init 0
  knextonset init 0


  ; initialise kampfac: negative values == fade in, + == fade out, 0 == no change
  kampmode init iampfac
  if (iampfac == 0) then
  kampfac init 1
  elseif (iampfac < 0) then
  kampfac init 0
  else
  kampfac init 1
  endif

  if (ionsetfac < 0) then
      konsetfac init 1
      kaccelmode init 1
  else
      konsetfac init ionsetfac
      kaccelmode init 0
  endif

 ;;BEGIN LOOP
  until (konset > p3) do

  kpitval   =  koriginal[4] + kintervals[kndx % klen]

  if (idur == 0) then
  kdur      =  koriginal[2]
  konset    =  konset + konsets[kndx % konsetlength] * konsetfac
  elseif (idur > 0) then
  konset    =  konset + konsets[kndx % konsetlength] * konsetfac
  kdur      =  idur - konset
  else      
  konset    =  konset + konsets[kndx % konsetlength] * konsetfac
  knextonset   =      konset + konsets[(kndx + 1) % konsetlength] * konsetfac
  kdur      =  knextonset - konset
  endif

  karg1 = koriginal[0]
  karg2 = tempodur_k(koriginal[1] + konset)
  karg4 = koriginal[3]*kampfac
  if (iplen == 5) then
  ;lazyvent karg1, karg2, kdur, karg4, kpitval
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale)
  elseif (iplen == 6) then
  karg6 = koriginal[5]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6
  elseif (iplen == 7) then
    karg6 = koriginal[5]
    karg7 = koriginal[6]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7
  elseif (iplen == 8) then
  karg6 = koriginal[5]
  karg7 = koriginal[6]
  karg8 = koriginal[7]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7, karg8
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7, karg8
  elseif (iplen == 9) then
  karg6 = koriginal[5]
  karg7 = koriginal[6]
  karg8 = koriginal[7]
  karg9 = koriginal[8]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7, karg8, karg9
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7, karg8, karg9
  elseif (iplen == 10) then
  karg6 = koriginal[5]
  karg7 = koriginal[6]
  karg8 = koriginal[7]
  karg9 = koriginal[8]
  karg10 = koriginal[9]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7, karg8, karg9, karg10
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7, karg8, karg9, karg10
  elseif (iplen == 11) then
  karg6 = koriginal[5]
  karg7 = koriginal[6]
  karg8 = koriginal[7]
  karg9 = koriginal[8]
  karg10 = koriginal[9]
  karg11 = koriginal[10]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7, karg8, karg9, karg10, karg11
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7, karg8, karg9, karg10, karg11
  elseif (iplen == 12) then
  karg6 = koriginal[5]
  karg7 = koriginal[6]
  karg8 = koriginal[7]
  karg9 = koriginal[8]
  karg10 = koriginal[9]
  karg11 = koriginal[10]
  karg12 = koriginal[11]
  ;lazyvent karg1, karg2, kdur, karg4, kpitval, karg6, karg7, karg8, karg9, karg10, karg11, karg12
  event "i", karg1, karg2, kdur, karg4, cpstun(1, kpitval, gi_CurrentScale), karg6, karg7, karg8, karg9, karg10, karg11, karg12
  endif

  enddo:

  konsetprogress = (konset/p3)

  if (kampmode == 0) then

  kampfac = 1

  elseif (kampmode > 0) then

    if (kampmode > 0.5) then
      kampfac pow (1 - konsetprogress), rescalek(kampmode, 0, 1, 1, 5), 1
    else
      kampfac pow (1 - konsetprogress), (kampmode * 2), 1
    endif

  else

    if (abs(kampmode) > 0.5) then
      kampfac pow konsetprogress, rescalek(abs(kampmode), 0, 1, 1, 5), 1
    else
      kampfac pow konsetprogress, (abs(kampmode) * 2), 1
    endif

  endif
 
  if (kaccelmode == 1) then
     konsetfac = (1 - (konsetprogress * (1 - abs(ionsetfac))))
  endif
  kndx      +=        1
  od

  konce = 1
  terminate:

endop

;declicks attacks and decays.
;Doesn't interrupt tied notes.
;Adds a release envelope.
opcode declickr, a, ajjo
itie tival  
ain, irisetime, idectime, itype xin
irisetm = (irisetime == -1 ? 0.003 : irisetime)
idectm = (idectime == -1 ? 0.08 : idectime)
aenv    transegr itie, irisetm, itype, 1, 0, 0, 1, idectm, itype, 0
xout ain * aenv         ; apply envelope and write output
endop

;stereo version 
opcode declickrst, aa, aajjo
itie tival
ainL, ainR, irisetime, idectime, itype xin
irisetm = (irisetime == -1 ? 0.003 : irisetime)
idectm = (idectime == -1 ? 0.08 : idectime)
aenv    transegr itie, irisetm, itype, 1, 0, 0, 1, idectm, itype, 0
xout ainL * aenv, ainR * aenv         ; apply envelope and write output
endop

;reduces amplitude of an audio signal based on the number of currently active instruments.
;reduction is 3db every doubling of current instances.
;ktime smooths the changes to avoid clicks (defaults to 0.08 seconds)
;iinsnum is the instrument number to monitor (defaults to calling instrument 1. 0 monitors all instruments.)
opcode activedamp, a,aOj
  asig, ktime, iinsnum xin
  setksmps 1
  iinsnum = (iinsnum == -1 ? p1 : iinsnum)  
  kampfac = ampdbfs(active:k(iinsnum, 0, 1)*-1.5); drop 3db every doubling.
  asig *= lineto(kampfac, limit:k(ktime, 0.008, 5))
  xout asig
endop

opcode tomfm, a,iijj
  iamp, ifqc, ihit, ifco xin

  ihit = ((ihit == -1) ? 0.2:ihit)
  ifco = ((ifco == -1) ? 0.5:ifco)
  ihit = ihit + 0.01

  afqc1  linseg    1+iamp*0.1, ihit*0.5*abs(p3), 1, 0.1, 1 ; Pitch bend
  afqc   =         afqc1*afqc1                       ; Pitch bend squared
  aamp1  expseg    0.01, 0.001, 1, abs(p3) - 0.001, 0.04      ; Tone envelope
  aamp2  expseg    0.01, 0.001, 1, abs(p3) * ihit, 0.01 ; Noise envelope

  arnd1  noise      iamp, rescale(ifco, 0, 1, -0.99, 0.99); -1 to 1 changes sound
  asig   oscili     iamp, ifqc*(1 + arnd1*aamp2*2);, 1 ; Frequency modulation with noise

  asig butterhp asig, 20

  aout   =         asig*iamp*aamp1*1.6

xout aout
endop

opcode bassUV, a, kKVVj
kamp, kfrq, kbw, ksep, inum xin

  ksep limit ksep, 0, 1

  inum = ((inum == -1) ? 5:inum)

  ipit = i(kfrq)

  kenv = xadsr(0.015, 0.01, 1, 0.03)*kamp
  asig1    pluck     kenv, kfrq, ipit*4, 0, 4, 0.5, 99
  asig2 vco2 kenv, kfrq, 12

  asig = asig1 + (asig2 * 0.17)

  krespit expseg ipit*4, 1.7, ipit*0.25, p3, ipit*4.25

  af resony asig, krespit, krespit*expcurve(kbw, 48), inum, (logcurve(ksep, 0.01))*20
  aout    balance af, asig

xout aout
endop

opcode slidefm,a,iiijo
iamp,ifrq,imodval,islidetime,ifn xin

  imod      rescale   imodval, 0, 1, 0, 1000

  if (islidetime <= 0) then
    islidetime = (abs(p3)) * 0.3
  else
    islidetime = tempodur(islidetime)
  endif

  ifn = (ifn == 0 ? gi_altfm:ifn)

  tigoto      tieinit
  ;buggy tival and tigoto in Android. Use this if instead
  ;cigoto (tival2() == 1), tieinit

  ibegpitch   = ifrq
  iprevpitch  = ifrq
  ibegamp =  iamp
  iprevamp =  iamp
  ibegint	 =  imod
  iprevint =  imod
  ibegvib = 0
  iendvib = imodval

  goto        cont

  tieinit:

    ibegpitch   =       iprevpitch
    iprevpitch  =       ifrq
    ibegamp   	=       iprevamp
    iprevamp  	=       iamp
    ibegint		=		iprevint
    iprevint	=		imod
    ibegvib = iendvib
    iendvib = imodval

  cont:

    kvibcontour linseg ibegvib, abs(p3*0.3), ibegvib, abs(p3*0.5), iendvib
    kvib vibr kvibcontour, 5, -1
    kvib portk kvib, 0.08, -1

    kpitchenv   linseg  ibegpitch, islidetime, ifrq
    kampenv	transeg ibegamp, islidetime, 2, iamp 
    kintensity  transeg  ibegint, islidetime, 0.5, imod


    a1        oscili      kampenv + (kvib * 0.1), kpitchenv + kvib, -1, -1
    aout      oscili    kampenv, kpitchenv+(a1*kintensity), ifn, -1

xout aout

endop


opcode stringsfm,a,KKjj
kamp, kfqc, inoisdur, intens xin
  idur    = abs(p3)
  itie tival
  if (itie == 1) then
    intens = 0
    inoisdur = 0
  else
    if (intens == -1) then
      intens = 3
    else intens *= 10
    endif
    if (inoisdur == -1) then
       inoisdur limit idur*0.08, 0.05, 0.1
     else
       inoisdur limit  idur*inoisdur, 0.08, idur
   endif 
  endif    
  ;intens = (intens == -1 ? 3 : (intens * 10))
  kndx1   = 7.5/log(kfqc)    
  kndx2   = 15/sqrt(kfqc)    
  kndx3   = 1.25/sqrt(kfqc)  
  ktrans  linseg  intens,inoisdur,0.1,1,0.1   
  anoise pinker
  attack  oscil  anoise*ktrans*0.3*kamp,kfqc*8
  kvibamp linseg 0, idur*0.3, 0, idur*0.4, 0.009, idur*0.3, 0
  kvibfrq linseg 0, idur*0.3, 5, idur*0.7, 3
  kvib vibr kvibamp, kvibfrq, -1
  
  amod1   oscil  kfqc*(kndx1+ktrans),kfqc*1.007, -1, (itie == 1 ? -1:0)
  amod2   oscil  kfqc*3*(kndx2+ktrans),kfqc*2.01, -1, (itie == 1 ? -1:0)
  amod3   oscil  kfqc*4*(kndx3+ktrans),kfqc*5, -1, (itie == 1 ? -1:0)
  asig    oscili  kamp,(kfqc+amod1+amod2+amod3)*(1+kvib), gi_altfm, (itie == 1 ? -1:0)
  asig2 = asig * attack
  asig = (asig + attack) * linseg(itie, limit(idur*0.1, 0.05, 0.1), 1, idur, 1)
  
  xout asig*0.8
endop


instr 2
gk_now += (gk_tempo / 60) / kr
endin
event_i "i", 2, 0, -1 

;lazyvent spawns this, so globals (scale and tempo) are caught at the time the event
;runs, not when scheduled.
instr 4
pset p1, p2, p3, p4, p5, p6, -99999, -99999, -99999, -99999, -99999, -99999


if (p7 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale)
elseif (p8 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7
elseif (p9 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7, p8
elseif (p10 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7, p8, p9
elseif (p11 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7, p8, p9, p10
elseif (p12 == -99999) then
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7, p8, p9, p10, p11
else
schedule p4, 0, tempodur(p3), p5, cpstuni(p6, gi_CurrentScale), p7, p8, p9, p10, p11, p12
endif

endin

instr 7

exitnow

endin

instr 300
  
ainL chnget "outputL"
ainR chnget "outputR"
ainCtr chnget "outputC"
ainL += ainCtr
ainR += ainCtr

arevL, arevR reverbsc ainL, ainR, 0.15, sr/2, sr, 0.1, 1

ainL = (ainL * 0.4) + arevL
ainR = (ainR * 0.4) + arevR

ainL = taninv(taninv(ainL))
ainR = taninv(taninv(ainR))

kmasterLevel chnget "MasterLevel"

outs ainL*kmasterLevel, ainR*kmasterLevel

chnclear "outputL"
chnclear "outputR"
chnclear "outputC"

endin

event_i "i", 300, 0, -1

;;;;;;;;;;;;;;
temposet 110

scalemode31 0

giSection = 1


instr 101
itie tival
tigoto cont
iamp, ipit = p4, p5
cont:
kamp transeg iamp, abs(p3), 0.5, p4
kpit transegb ipit, abs(p3)*0.33, 1, ipit, abs(p3)*0.666, -2.5, p5, abs(p3), 1, p5    
ares stringsfm kamp, kpit, p6, p7
iamp, ipit = p4, p5
ares activedamp ares; [, ktime, iinsnum]
ares declickr ares

if giSection == 1 then
chnmix ares, "outputC"
else
chnmix ares*0.8, "outputC"
chnmix ares*1, "delay"
endif
    
endin

instr 102

ares tomfm, p4,p5, p6, p7; [, ihitlen, ifco]
;ares declickr ares
  ;chnmix ares, "outputC"
  outs ares, ares
endin

instr 103
if (active:k(p1) > 1) then
              if (timeinstk() > ksmps) then
              turnoff
              endif
           endif
  
ares bassUV p4, p5, (cosr(96) * 1.2) + 0.3, cosr(64) + 0.1

ares declickr ares

chnmix ares, "outputC"

  
endin

instr 104

ares slidefm p4, p5, p6

ares declickr ares
chnmix ares, "outputC"
chnmix ares*0.7, "delay"
endin

instr 201
ain chnget "delay"

aresL multitap ain, tempodur(5/4), 0.4, tempodur(1/3), 0.7,tempodur(2/3), 0.5  
aresR multitap ain, tempodur(5/3), 0.4,tempodur(1/2), 0.7,tempodur(3/4), 0.5  

aresL declickr aresL
aresR declickr aresR
chnmix aresL, "outputL"
chnmix aresR, "outputR"

chnclear "delay"
endin


instr 10

scalemode31 p4, p5

turnoff
endin


instr 15
cggoto (giSection != 1), NOTHING

schedule 10, 1,1,18,0

arpeggiates array(104, 0, 1/12 * randselect_i(-1, 1), 0.3, 7, 0.2), array(0,0,2,3,4,5,4), array(1/6, 3/6), 0; [,idur, ionsetfac, iampfac]

NOTHING:  
schedule weightedchoice(array(15, 16), array(1/2, 1/4, 1/4)) , nextbeat(6), 2

turnoff
endin

instr 16
cggoto (giSection != 1), NOTHING
schedule 10, 1,1,0, 0
arpeggiates array(104, 0, 1/12 * randselect_i(-1, 1), 0.2, 7, 5), array(4,5,4,3,2,0,0), array(1/6, 3/6), 0; [,idur, ionsetfac, iampfac]
NOTHING:  
schedule weightedchoice(array(15, 16, 17), array(1/2, 0, 1/2)), nextbeat(6), 2
turnoff
endin

instr 17
cggoto (giSection != 1), NOTHING  
schedule 10, 0,1,13, 0
arpeggiates array(104, 0, 1/12 * randselect_i(-1, 1), 0.4, 7, 2), array(1,1,2,1,0,1,2,3,4), array(1/6, 3/6), 0; [,idur, ionsetfac, iampfac]

NOTHING:  
schedule 16, nextbeat(6), 1.9
turnoff
endin


instr 11
cggoto (giSection != 1), NOTHING
schedule 102, tempodur(0/12), 0.05, 0.4, cpstuni(21, gi_CurrentScale), 3, 0.007
schedule 102, tempodur(6/12), 0.1, 0.4, cpstuni(21, gi_CurrentScale), 3, 0.007
schedule 102, tempodur(9/12), 0.1, 0.2, cpstuni(21, gi_CurrentScale), 3, 0.007
schedule 102, tempodur(6/12), 0.13, 0.65, cpstuni(-17, gi_CurrentScale), 0.9, 0.1
  
chordal array(101, 6/12, 1/12, 0.6, giTonic_ndx, -1, -1), array(0,2,4), 1, 0, 0; [,idbdamp ,insincr,kscale]  
chordal array(101.1, 0, 1/12, 0.35, giTonic_ndx -7, 0.01, 0.1), array(-1, 0,2, 4),1, 0, 5; [,idbdamp ,insincr,kscale]  
chordal array(101, 9/12, 1/12, 0.5, giTonic_ndx, -1, -1), array(0,2,4), 1, 0, 0; [,idbdamp ,insincr,kscale]  
NOTHING:
schedule p1, nextbeat(1), 1
endin




instr 12
cggoto (giSection != 1), NOTHING

arpeggiates array(103, 0, 6/12, 0.22, giTonic_ndx - 14), array(0, 0, -7, -3), array(3/12, 3/12, 6/12); [,idur, ionsetfac, iampfac]
  
NOTHING:
schedule p1, nextbeat(3), 3
endin



instr 24
cggoto (giSection != 2), NOTHING  
scalemode31 iterArr(array(23, 28, 0, 18), "sec2")
arpeggiates array(102, 0, 0.11, 0.35, giTonic_ndx + 21, 3.1, 0.01), array(0), array(0.25), -3, -0.25, -0.5; [,idur, ionsetfac, iampfac]
arpeggiates array(102, 3, 0.37, 0.3, giTonic_ndx + 21, 4.1, 0.008), array(0, 7), array(0.25), 0, 0.5, 0.5; [,idur, ionsetfac, iampfac]


schedule 104, weightedchoice(array(0, tempodur(3)), array(0.75, 0.25)), weightedchoice(array(-1, -0.5, -3, tempodur(2)), array(0.5, 0.25, 0.1,0.15)), 0.4, cpstuni(iterArr(array(-2, -1, 0, 4, 2, 1, 0) + 7, "sec2mel"),gi_CurrentScale),2

schedule 103, 0, tempodur(6), 0.6, cpstuni(giTonic_ndx - 21, gi_CurrentScale)
  
schedule 101.2, 0, tempodur(6), 0.5, cpstuni(giTonic_ndx - 14, gi_CurrentScale), 1, 0.5  
chordal array(101.1, 0, 3, 0.4, giTonic_ndx, 0.8, 0.01), array(2,4,iterArr(array(7,6,5), "spice")), 0, 0, 0

ornament array(101, 0, -0.03 * randselect_i(-1, 1), 0.7, 0, -1, -1), 0.125, array(2,1), 21, 6
  
schedule p1, nextbeat(6), tempodur(6)
NOTHING:

turnoff
endin

;section changer
instr 30
if (giSection == 1) then
    giSection = 2
    schedule 24, 0, tempodur(6)
    schedule p1, nextbeat(72), 1
  else
    giSection = 1
  schedule p1, nextbeat(144), 1

endif
  

turnoff
endin

schedule 201, 0, -1

schedule 11, nextbeat(1), 1

schedule 12, nextbeat(12), tempodur(6)

schedule 15, nextbeat(24), 2

schedule 30, nextbeat(144), tempodur(1)
