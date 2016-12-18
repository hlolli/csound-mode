(setq csdoc-opcode-database (makehash 'equal))
(puthash "shiftin" '(:template "kout[] shiftin asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octpch" '(:template "octpch (pch) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "follow2" '(:template "ares follow2 asig, katt, krel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gendyc" '(:template "ares gendyc kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]
kres gendyc kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ihold" '(:template "ihold" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbaplsinit" '(:template "vbaplsinit idim, ilsnum [, idir1] [, idir2] [...] [, idir32]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbapzmove" '(:template "vbapzmove inumchnls, istartndx, asig, idur, ispread, ifldnum, ifld1, ifld2, [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spsend" '(:template "a1, a2, a3, a4 spsend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readk4" '(:template "kr1, kr2, kr3, kr4 readk4 ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKResonate" '(:template "asignal STKResonate ifrequency, iamplitude, [kfreq, kv1[, kpole, kv2[, knotch, kv3[, kzero, kv4[, kenv, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ephasor" '(:template "aexp,aph ephasor kfreq, kR" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cuserrnd" '(:template "aout cuserrnd kmin, kmax, ktableNum
iout cuserrnd imin, imax, itableNum
kout cuserrnd kmin, kmax, ktableNum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midiin" '(:template "kstatus, kchan, kdata1, kdata2 midiin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelay3" '(:template "ares vdelay3 asig, adel, imaxdel [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dssiinit" '(:template "ihandle dssiinit ilibraryname, iplugindex [, iverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvstencil" '(:template "fsig pvstencil fsigin, kgain, klevel, iftable" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoInfo" '(:template "JackoInfo" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "A4" '(:template "A4 = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filepeak" '(:template "ir filepeak ifilcod [, ichnl]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spat3d" '(:template "aW, aX, aY, aZ spat3d ain, kX, kY, kZ, idist, ift, imode, imdel, iovr [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strchark" '(:template "kchr strchark Sstr[, kpos]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbapg" '(:template "k1[, k2...] vbapg kazim [,kelev] [, kspread] [, ilayout]
karray[] vbapg kazim [,kelev] [, kspread] [, ilayout]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "delayw" '(:template "delayw asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nlfilt2" '(:template "ares nlfilt2 ain, ka, kb, kd, kC, kL" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mclock" '(:template "mclock ifreq" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBlowHole" '(:template "asignal STKBlowHole ifrequency, iamplitude, [kreed, kv1[, knoise, kv2[, khole, kv3[, kreg, kv4[, kbreath, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nsamp" '(:template "nsamp(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midipitchbend" '(:template "midipitchbend xpitchbend [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strlower" '(:template "Sdst strlower Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filenchnls" '(:template "ir filenchnls ifilcod [, iallowraw]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmultv_i" '(:template "vmultv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoAudioOutConnect" '(:template "JackoAudioOutConnect ScsoundPortName, SexternalPortName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zaw" '(:template "zaw asig, kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vco2ft" '(:template "kfn vco2ft kcps, iwave [, inyx]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidCCk" '(:template "fluidCCk iEngineNumber, iChannelNumber, iControllerNumber, kValue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "freeverb" '(:template "aoutL, aoutR freeverb ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "repluck" '(:template "ares repluck iplk, kamp, icps, kpick, krefl, axcite" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsout" '(:template "pvsout fsig, kchan" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mp3scal" '(:template "asig, asig2, ktime mp3scal Sfile,ktimescal,kpitch, kamp [,iskip,ifftsize, idecim, ilock]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gogobel" '(:template "ares gogobel kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fink" '(:template "fink ifilename, iskipframes, iformat, kin1 [, kin2] [, kin3] [,...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "clear" '(:template "clear avar1 [, avar2] [, avar3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moogvcf2" '(:template "ares moogvcf2 asig, xfco, xres [,iscale, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagesize" '(:template "iwidth, iheight imagesize iimagenum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ktableseg" '(:template "ktableseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLpackEnd" '(:template "FLpackEnd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "table3" '(:template "ares table3 andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires table3 indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres table3 kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mp3len" '(:template "ir mp3len ifilcod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKMoog" '(:template "asignal STKMoog ifrequency, iamplitude, [kq, kv1[, krate, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbandp" '(:template "fsig pvsbandp fsigin, xlowcut, xlowfull, xhighfull, xhighcut[, ktype]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "splitrig" '(:template "splitrig ktrig, kndx, imaxtics, ifn, kout1 [,kout2,...,koutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLhvsBoxSetValue" '(:template "FLhvsBox kx, ky, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strtol" '(:template "ir strtol Sstr
ir strtol indx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "miditempo" '(:template "ksig miditempo" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vrandi" '(:template "vrandi ifn, krange, kcps, ielements [, idstoffset] [, iseed] [, isize] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moogvcf" '(:template "ares moogvcf asig, xfco, xres [,iscale, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vco" '(:template "ares vco xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] [, iphs] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sensekey" '(:template "kres[, kkeydown] sensekey" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap8" '(:template "ar1, ..., ar8 vbap8 asig, kazim [, kelev] [, kspread]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnkGetHandle" '(:template "ihandle FLslidBnkGetHandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pchmidinn" '(:template "pchmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "turnon" '(:template "turnon insnum [, itime]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfinstrm" '(:template "ares sfinstrm ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "timout" '(:template "timout istrt, idur, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoFreewheel" '(:template "JackoFreewheel [ienabled]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vcomb" '(:template "ares vcomb asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "compilestr" '(:template "ires compilestr Sorch" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "clip" '(:template "ares clip asig, imeth, ilimit [, iarg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "noteondur" '(:template "noteondur ichn, inum, ivel, idur" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidSetInterpMethod" '(:template "fluidSetInterpMethod ienginenum, ichannelnum, iInterpMethod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linen" '(:template "ares linen xamp, irise, idur, idec
kres linen kamp, irise, idur, idec" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outleta" '(:template "outleta Sname, asignal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnkSetk" '(:template "FLslidBnkSetk ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetsnap" '(:template "inumsnap, inumval FLsetsnap index [, ifn, igroup]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inh" '(:template "ar1, ar2, ar3, ar4, ar5, ar6 inh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "checkbox" '(:template "kres checkbox knum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmult_i" '(:template "vmult_i ifn, ival, ielements [, idstoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fin" '(:template "fin ifilename, iskipframes, iformat, ain1 [, ain2] [, ain3] [,...]
fin ifilename, iskipframes, iformat, arr[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLroller" '(:template "kout, ihandle FLroller quot;labelquot;, imin, imax, istep, iexp, itype, idisp, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opbitshr" '(:template "a gt;gt; b (bitshift left)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsfread" '(:template "fsig pvsfread ktimpt, ifn [, ichan]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dumpk3" '(:template "dumpk3 ksig1, ksig2, ksig3, ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagefree" '(:template "imagefree iimagenum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trigger" '(:template "kout trigger ksig, kthreshold, kmode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKMandolin" '(:template "asignal STKMandolin ifrequency, iamplitude, [kbody, kv1[, kpos, kv2[, ksus, kv3[, kdetune, kv4[, kmic, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readf" '(:template "Sres, kline readf ifilname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pluck" '(:template "ares pluck kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSreadnz" '(:template "kenergy ATSreadnz ktimepnt, iatsfile, iband" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayk" '(:template "kout vdelayk ksig, kdel, imaxdel [, iskip, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsinfo" '(:template "ioverlap, inumbins, iwinsize, iformat pvsinfo fsrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "divides" '(:template "a sol; b (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvswarp" '(:template "fsig pvswarp fsigin, kscal, kshift[, klowest, kmeth, kgain, kcoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmbell" '(:template "ares fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn, isus]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableigpw" '(:template "tableigpw ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "raises" '(:template "a circ; b (b not audio-rate)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "areson" '(:template "ares areson asig, kcf, kbw [, iscl] [, iskip]
ares areson asig, acf, kbw [, iscl] [, iskip]
ares areson asig, kcf, abw [, iscl] [, iskip]
ares areson asig, acf, abw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsfreeze" '(:template "fsig pvsfreeze fsigin, kfreeza, kfreezf" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loopsegp" '(:template "ksig loopsegp kphase, kvalue0, kdur0, kvalue1 [, kdur1, ... , kdurN-1, kvalueN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specdisp" '(:template "specdisp wsig, iprd [, iwtflg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoAudioIn" '(:template "asignal JackoAudioIn ScsoundPortName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscilikts" '(:template "ares oscilikts xamp, xcps, kfn, async, kphs [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcpyk" '(:template "Sdst strcpyk Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butterbp" '(:template "ares butterbp asig, xfreq, xband [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "guiro" '(:template "ares guiro kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outq3" '(:template "outq3 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialWrite_i" '(:template "serialWrite_i iPort, iByte
serialWrite_i iPort, SBytes" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dctinv" '(:template "kout[] dctinv kin[]
iout[] dctinv iin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midiout" '(:template "midiout kstatus, kchan, kdata1, kdata2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sr" '(:template "sr = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "endif" '(:template "endif" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKClarinet" '(:template "asignal STKClarinet ifrequency, iamplitude, [kstiff, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vincr" '(:template "vincr accum, aincr" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "seed" '(:template "seed ival" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mp3in" '(:template "ar1, ar2 mp3in ifilcod[, iskptim, iformat, iskipinit, ibufsize]
ar1 mp3in ifilcod[, iskptim, iformat, iskipinit, ibufsize]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wguide1" '(:template "ares wguide1 asig, xfreq, kcutoff, kfeedback" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "compileorc" '(:template "ires compileorc Sfilename" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ampdbfs" '(:template "ampdbfs(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "randomh" '(:template "ares randomh kmin, kmax, xcps [,imode] [,ifirstval]
kres randomh kmin, kmax, kcps [,imode] [,ifirstval]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoTransport" '(:template "JackoTransport kcommand, [kposition]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lowpass2" '(:template "ares lowpass2 asig, kcf, kq [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKSitar" '(:template "asignal STKSitar ifrequency, iamplitude" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabifd" '(:template "ffr,fphs tabifd ktimpt, kamp, kpitch, ifftsize, ihopsize, iwintype,ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rigoto" '(:template "rigoto label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "event_i" '(:template "event_i quot;scorecharquot;, iinsnum, idelay, idur, [, ip4] [, ip5] [, ...]
event_i quot;scorecharquot;, quot;insnamequot;, idelay, idur, [, ip4] [, ip5] [, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "diskin2" '(:template "a1[, a2[, ... aN]] diskin2 ifilcod[, kpitch[, iskiptim [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]
ar1[] diskin2 ifilcod[, kpitch[, iskiptim [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expsegba" '(:template "ares expsegba ia, itim1, ib [, itim2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "stack" '(:template "stack iStackSize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsosc" '(:template "fsig pvsosc kamp, kfreq, ktype, isize [,ioverlap] [, iwinsize] [, iwintype] [, iformat]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetText" '(:template "FLsetText quot;itextquot;, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "in32" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16, ar17, ar18, ar19, ar20, ar21, ar22, ar23, ar24, ar25, ar26, ar27, ar28, ar29, ar30, ar31, ar32 in32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "websocket" '(:template "xout1[, xout2, xout3, ..., xoutN] websocket iport, xin1[, xin2, xin3, ..., xinN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialRead" '(:template "kByte serialRead iPort" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "taninv" '(:template "taninv(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "kr" '(:template "kr = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "notnum" '(:template "ival notnum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "jacktransport" '(:template "jacktransport icommand [, ilocation]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablegpw" '(:template "tablegpw kfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKStifKarp" '(:template "asignal STKStifKarp ifrequency, iamplitude, [kpos, kv1[, ksus, kv2[, kstretch, kv3]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hvs2" '(:template "hvs2 kx, ky, inumParms, inumPointsX, inumPointsY, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKFlute" '(:template "asignal STKFlute ifrequency, iamplitude, [kjet, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "prints" '(:template "prints quot;stringquot; [, kval1] [, kval2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pitch" '(:template "koct, kamp pitch asig, iupdte, ilo, ihi, idbthresh [, ifrqs] [, iconf] [, istrt] [, iocts] [, iq] [, inptls] [, irolloff] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dssictls" '(:template "dssictls ihandle, iport, kvalue, ktrigger" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "remove" '(:template "remove insnum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "setctrl" '(:template "setctrl inum, ival, itype" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "notequal" '(:template "(a != b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mirror" '(:template "ares mirror asig, klow, khigh
ires mirror isig, ilow, ihigh
kres mirror ksig, klow, khigh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagesetpixel" '(:template "imagesetpixel iimagenum, ax, ay, ared, agreen, ablue
imagesetpixel iimagenum, kx, ky, kred, kgreen, kblue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wterrain" '(:template "aout wterrain kamp, kpch, k_xcenter, k_ycenter, k_xradius, k_yradius, itabx, itaby" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftgen" '(:template "gir ftgen ifn, itime, isize, igen, iarga [, iargb ] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "in" '(:template "ar1 in
aarray in" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgbowedbar" '(:template "ares wgbowedbar kamp, kfreq, kpos, kbowpres, kgain [, iconst] [, itvel] [, ibowpos] [, ilow]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xscanu" '(:template "xscanu init, irate, ifnvel, ifnmass, ifnstif, ifncentr, ifndamp, kmass, kstif, kcentr, kdamp, ileft, iright, kpos, kstrngth, ain, idisp, id" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readscratch" '(:template "ival readscratch[index]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gaussi" '(:template "ares gaussi krange, xamp, xcps
ires gaussi krange, xamp, xcps
kres gaussi krange, xamp, xcps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fiopen" '(:template "ihandle fiopen ifilename, imode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsmorph" '(:template "fsig pvsmorph fsig1, fsig2, kampint, kfrqint" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mandel" '(:template "kiter, koutrig mandel ktrig, kx, ky, kmaxIter" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLpack" '(:template "FLpack iwidth, iheight, ix, iy, itype, ispace, iborder" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableiw" '(:template "tableiw isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLshow" '(:template "FLshow ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pyinit" '(:template "pyinit" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKVoicForm" '(:template "asignal STKVoicForm ifrequency, iamplitude, [kmix, kv1[, ksel, kv2[, klfo, kv3[, klfodepth, kv4[, kloud, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resonx" '(:template "ares resonx asig, xcf, xbw [, inumlayer] [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inleta" '(:template "asignal inleta Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gendy" '(:template "ares gendy kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]
kres gendy kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "massign" '(:template "massign ichnl, insnum[, ireset]
massign ichnl, quot;insnamequot;[, ireset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "diskin" '(:template "ar1 [, ar2 [, ar3 [, ... arN]]] diskin ifilcod[, kpitch[, iskiptim [, iwraparound[, iformat[, iskipinit]]]]]
ar1[] diskin ifilcod[, kpitch[, iskiptim [, iwraparound[, iformat[, iskipinit]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outc" '(:template "outc asig1 [, asig2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSaddnz" '(:template "ar ATSaddnz ktimepnt, iatsfile, ibands[, ibandoffset, ibandincr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ficlose" '(:template "ficlose ihandle
ficlose Sfilename" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "socksend" '(:template "socksend asig, Sipaddr, iport, ilength
socksend ksig, Sipaddr, iport, ilength
socksends asigl, asigr, Sipaddr, iport, ilength
stsend asig, Sipaddr, iport" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKModalBar" '(:template "asignal STKModalBar ifrequency, iamplitude, [khard, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kmix, kv5[, kvol, kv6[, kinstr, kv7]]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "insglobal" '(:template "insglobal isource, instrnum [,instrnum...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "diff" '(:template "ares diff asig [, iskip]
kres diff ksig [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rand" '(:template "ares rand xamp [, iseed] [, isel] [, ioffset]
kres rand xamp [, iseed] [, isel] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider64table" '(:template "kflag slider64table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum64, imin64, imax64, init64, ifn64" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfplay3m" '(:template "ares sfplay3m ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ksmps" '(:template "ksmps = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inletk" '(:template "ksignal inletk Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rtclock" '(:template "ires rtclock
kres rtclock" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "timeinstk" '(:template "kres timeinstk" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvread" '(:template "kfreq, kamp pvread ktimpnt, ifile, ibin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pycall" '(:template "pycall "callable", karg1, ...
kresult pycall1 "callable", karg1, ...
kresult1, kresult2 pycall2 "callable", karg1, ...
kr1, kr2, kr3 pycall3 "callable", karg1, ...
kr1, kr2, kr3, kr4 pycall4 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5 pycall5 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6 pycall6 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7 pycall7 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7, kr8 pycall8 "callable", karg1, ...
pycallt ktrigger, "callable", karg1, ...
kresult pycall1t ktrigger, "callable", karg1, ...
kresult1, kresult2 pycall2t ktrigger, "callable", karg1, ...
kr1, kr2, kr3 pycall3t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4 pycall4t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5 pycall5t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6 pycall6t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7 pycall7t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7, kr8 pycall8t ktrigger, "callable", karg1, ...
pycalli "callable", karg1, ...
iresult pycall1i "callable", iarg1, ...
iresult1, iresult2 pycall2i "callable", iarg1, ...
ir1, ir2, ir3 pycall3i "callable", iarg1, ...
ir1, ir2, ir3, ir4 pycall4i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5 pycall5i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6 pycall6i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6, ir7 pycall7i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6, ir7, ir8 pycall8i "callable", iarg1, ...
pycalln "callable", nresults, kresult1, ..., kresultn, karg1, ...
pycallni "callable", nresults, iresult1, ..., iresultn, iarg1, ...
pylcall "callable", karg1, ...
kresult pylcall1 "callable", karg1, ...
kresult1, kresult2 pylcall2 "callable", karg1, ...
kr1, kr2, kr3 pylcall3 "callable", karg1, ...
kr1, kr2, kr3, kr4 pylcall4 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5 pylcall5 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6 pylcall6 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7 pylcall7 "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7, kr8 pylcall8 "callable", karg1, ...
pylcallt ktrigger, "callable", karg1, ...
kresult pylcall1t ktrigger, "callable", karg1, ...
kresult1, kresult2 pylcall2t ktrigger, "callable", karg1, ...
kr1, kr2, kr3 pylcall3t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4 pylcall4t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5 pylcall5t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6 pylcall6t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7 pylcall7t ktrigger, "callable", karg1, ...
kr1, kr2, kr3, kr4, kr5, kr6, kr7, kr8 pylcall8t ktrigger, "callable", karg1, ...
pylcalli "callable", karg1, ...
iresult pylcall1i "callable", iarg1, ...
iresult1, iresult2 pylcall2i "callable", iarg1, ...
ir1, ir2, ir3 pylcall3i "callable", iarg1, ...
ir1, ir2, ir3, ir4 pylcall4i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5 pylcall5i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6 pylcall6i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6, ir7 pylcall7i "callable", iarg1, ...
ir1, ir2, ir3, ir4, ir5, ir6, ir7, ir8 pylcall8i "callable", iarg1, ...
pylcalln "callable", nresults, kresult1, ..., kresultn, karg1, ...
pylcallni "callable", nresults, iresult1, ..., iresultn, iarg1, ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sum" '(:template "ares sum asig1 [, asig2] [, asig3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoMidiOut" '(:template "JackoMidiOut ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "initc14" '(:template "initc14 ichan, ictlno1, ictlno2, ivalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "adsynt" '(:template "ares adsynt kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "soundout" '(:template "soundout asig1, ifilcod [, iformat]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pop" '(:template "xval1, [xval2, ... , xval31] pop
ival1, [ival2, ... , ival31] pop" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tempoval" '(:template "kres tempoval" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zkcl" '(:template "zkcl kfirst, klast" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftlptim" '(:template "ftlptim(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fog" '(:template "ares fog xamp, xdens, xtrans, aspd, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur [, iphs] [, itmode] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cudanal" '(:template "fsig cudanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rbjeq" '(:template "ar rbjeq asig, kfco, klvl, kQ, kS[, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mvclpf1" '(:template "asig mvclpf1 ain, xcf, xres[,istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "endop" '(:template "endop" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetAlign" '(:template "FLsetAlign ialign, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rewindscore" '(:template "rewindscore" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loop_le" '(:template "loop_le indx, incr, imax, label
loop_le kndx, kncr, kmax, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKPercFlut" '(:template "asignal STKPercFlut ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLkeyIn" '(:template "kascii FLkeyIn [ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "harmon2" '(:template "ares harmon2 asig, koct, kfrq1, kfrq2, icpsmode, ilowest[, ipolarity]
ares harmon3 asig, koct, kfrq1, kfrq2, kfrq3, icpsmode, ilowest[, ipolarity]
ares harmon4 asig, koct, kfrq1, kfrq2, kfrq3, kfrq4, icpsmode, ilowest[, ipolarity]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablera" '(:template "ares tablera kfn, kstart, koff" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "exciter" '(:template "ares exciter asig, kfreq, kceil, kharmonics, kblend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outletf" '(:template "outletf Sname, fsignal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "multiplies" '(:template "a ast; b (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftchnls" '(:template "ftchnls(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstnote" '(:template "vstnote instance, kchan, knote, kveloc, kdur" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider8" '(:template "i1,...,i8 slider8 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8
k1,...,k8 slider8 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resyn" '(:template "asig resyn fin, kscal, kpitch, kmaxtracks, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoAudioInConnect" '(:template "JackoAudioInConnect SexternalPortName, ScsoundPortName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "transegr" '(:template "ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
kres transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "portk" '(:template "kres portk ksig, khtim [, isig]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mac" '(:template "ares mac ksig1, asig1 [, ksig2] [, asig2] [, ksig3] [, asig3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dollar" '(:template "dollar;NAME#160;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "s32b14" '(:template "i1,...,i32 s32b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb32, ictlno_lsb32, imin32, imax32, initvalue32, ifn32
k1,...,k32 s32b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb32, ictlno_lsb32, imin32, imax32, initvalue32, ifn32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tempo" '(:template "tempo ktempo, istartempo" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "partikkel" '(:template "a1 [, a2, a3, a4, a5, a6, a7, a8] partikkel agrainfreq, kdistribution, idisttab, async, kenv2amt, ienv2tab, ienv_attack, ienv_decay, ksustain_amount, ka_d_ratio, kduration, kamp, igainmasks, kwavfreq, ksweepshape, iwavfreqstarttab, iwavfreqendtab, awavfm, ifmamptab, kfmenv, icosine, ktraincps, knumpartials, kchroma, ichannelmasks, krandommask, kwaveform1, kwaveform2, kwaveform3, kwaveform4, iwaveamptab, asamplepos1, asamplepos2, asamplepos3, asamplepos4, kwavekey1, kwavekey2, kwavekey3, kwavekey4, imax_grains [, iopcode_id]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "syncgrain" '(:template "asig syncgrain kamp, kfreq, kpitch, kgrsize, kprate, ifun1, ifun2, iolaps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "statevar" '(:template "ahp,alp,abp,abr statevar ain, xcf, xq [, iosamps, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outletk" '(:template "outletk Sname, ksignal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmvoice" '(:template "ares fmvoice kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate[, ifn1, ifn2, ifn3, ifn4, ivibfn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inletkid" '(:template "ksignal inletkid Sname, SinstanceID" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fprintks" '(:template "fprintks quot;filenamequot;, quot;stringquot;, [, kval1] [, kval2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvadd" '(:template "ares pvadd ktimpnt, kfmod, ifilcod, ifn, ibins [, ibinoffset] [, ibinincr] [, iextractmode] [, ifreqlim] [, igatefn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableikt" '(:template "ares tableikt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
kres tableikt kndx, kfn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lineto" '(:template "kres lineto ksig, ktime" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "frac" '(:template "frac(x) (init-rate or control-rate args; also works at audio rate in Csound5)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tone" '(:template "ares tone asig, khp [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetBox" '(:template "FLsetBox itype, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKTubeBell" '(:template "asignal STKTubeBell ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "turnoff" '(:template "turnoff
turnoff inst
turnoff knst" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "shiftout" '(:template "asig shiftout kIn[][, ioff]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "exprandi" '(:template "ares exprandi klambda, xamp, xcps
ires exprandi klambda, xamp, xcps
kres exprandi klambda, xamp, xcps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkc" '(:template "outkc kchn, knum, kvalue, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butterlp" '(:template "ares butterlp asig, kfreq [, iskip]
ares butterlp asig, afreq [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "setrow" '(:template "kout[] setrowkin[],krow" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "framebuffer" '(:template "kout[] framebuffer ain, isize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "turnoff2" '(:template "turnoff2 kinsno, kmode, krelease" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "reson" '(:template "ares reson asig, xcf, xbw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midifilestatus" '(:template "ksig midifilestatus" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dbamp" '(:template "dbamp(x) (init-rate or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specfilt" '(:template "wsig specfilt wsigin, ifhtim" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vpvoc" '(:template "ares vpvoc ktimpnt, kfmod, ifile [, ispecwp] [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pinker" '(:template "ares pinker" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdivv" '(:template "vdivv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "remoteport" '(:template "remoteport iportnum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cos" '(:template "cos(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvs2tab" '(:template "kframe pvs2tab tvar|kvar[], fsig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "delay" '(:template "ares delay asig, idlt [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbapmove" '(:template "ar1[, ar2...] vbapmove asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]
aarray[] vbapmove asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstprogset" '(:template "vstprogset instance, kprogram" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tigoto" '(:template "tigoto label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rms" '(:template "kres rms asig [, ihp] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specptrk" '(:template "koct, kamp specptrk wsig, kvar, ilo, ihi, istr, idbthresh, inptls, irolloff [, iodd] [, iconfs] [, interp] [, ifprd] [, iwtflg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfer" '(:template "aleft, aright hrtfer asig, kaz, kelev," :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtaba" '(:template "vtaba andx, ifn, aout1 [, aout2, aout3, .... , aoutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "schedkwhen" '(:template "schedkwhen ktrigger, kmintim, kmaxnum, kinsnum, kwhen, kdur [, ip4] [, ip5] [...]
schedkwhen ktrigger, kmintim, kmaxnum, quot;insnamequot;, kwhen, kdur [, ip4] [, ip5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "line" '(:template "ares line ia, idur, ib
kres line ia, idur, ib" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftmorf" '(:template "ftmorf kftndx, iftfn, iresfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "partials" '(:template "ftrks partials ffr, fphs, kthresh, kminpts, kmaxgap, imaxtracks" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mvclpf4" '(:template "asig1,asig2,asig3,asig4 mvclpf4 ain, xcf, xres[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chani" '(:template "kval chani kchan
aval chani kchan" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zkw" '(:template "zkw ksig, kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strtolk" '(:template "kr strtolk Sstr
kr strtolk kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "envlpxr" '(:template "ares envlpxr xamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]
kres envlpxr kamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sqrt" '(:template "sqrt(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specsum" '(:template "ksum specsum wsig [, interp]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lorisplay" '(:template "ar lorisplay ireadidx, kfreqenv, kampenv, kbwenv" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ampmidi" '(:template "iamp ampmidi iscal [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "db" '(:template "db(x)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cigoto" '(:template "cigoto condition, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lowresx" '(:template "ares lowresx asig, xcutoff, xresonance [, inumlayer] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cosh" '(:template "cosh(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfearly" '(:template "aleft, aright, irt60low, irt60high, imfp hrtfearly asrc, ksrcx, ksrcy, ksrcz, klstnrx, klstnry, klstnrz, ifilel, ifiler, idefroom [,ifade, isr, iorder, ithreed, kheadrot, iroomx, iroomy, iroomz, iwallhigh, iwalllow, iwallgain1, iwallgain2, iwallgain3, ifloorhigh, ifloorlow, ifloorgain1, ifloorgain2, ifloorgain3, iceilinghigh, iceilinglow, iceilinggain1, iceilinggain2, iceilinggain3]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsifd" '(:template "ffr,fphs pvsifd ain, ifftsize, ihopsize, iwintype[,iscal]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "foscil" '(:template "ares foscil xamp, kcps, xcar, xmod, kndx, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strindex" '(:template "ipos strindex S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "logbtwo" '(:template "logbtwo(x) (init-rate or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "jspline" '(:template "ares jspline xamp, kcpsMin, kcpsMax
kres jspline kamp, kcpsMin, kcpsMax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvslock" '(:template "fsig pvslock fsigin, klock" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strtod" '(:template "ir strtod Sstr
ir strtod indx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "svfilter" '(:template "alow, ahigh, aband svfilter asig, kcf, kq [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tambourine" '(:template "ares tambourine kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSinfo" '(:template "idata ATSinfo iatsfile, ilocation" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tonex" '(:template "ares tonex asig, khp [, inumlayer] [, iskip]
ares tonex asig, ahp [, inumlayer] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLcolor2" '(:template "FLcolor2 ired, igreen, iblue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "syncloop" '(:template "asig syncloop kamp, kfreq, kpitch, kgrsize, kprate, klstart, klend, ifun1, ifun2, iolaps[,istart, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableshuffle" '(:template "tableshuffle ktablenum
tableshufflei itablenum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "max_k" '(:template "knumkout max_k asig, ktrig, itype" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftsave" '(:template "ftsave quot;filenamequot;, iflag, ifn1 [, ifn2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabmorpha" '(:template "aout tabmorpha aindex, aweightpoint, atabnum1, atabnum2, ifn1, ifn2 [, ifn3, ifn4, ... ifnN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "minaccum" '(:template "minaccum aAccumulator, aInput" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap4" '(:template "ar1, ar2, ar3, ar4 vbap4 asig, kazim [, kelev] [, kspread]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pset" '(:template "pset icon1 [, icon2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "greaterequal" '(:template "(a gt;= b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vaddv_i" '(:template "vaddv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "setscorepos" '(:template "setscorepos ipos" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ptable3" '(:template "ares ptable3 andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptable3 indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptable3 kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bqrez" '(:template "ares bqrez asig, xfco, xres [, imode] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmpercfl" '(:template "ares fmpercfl kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vsubv_i" '(:template "vsubv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsdemix" '(:template "fsig pvsdemix fleft, fright, kpos, kwidth, ipoints" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inch" '(:template "ain1[, ...] inch kchan1[,...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vco2" '(:template "ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfpassign" '(:template "sfpassign istartindex, ifilhandle[, imsgs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "limit" '(:template "ares limit asig, klow, khigh
ires limit isig, ilow, ihigh
kres limit ksig, klow, khigh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "waveset" '(:template "ares waveset ain, krep [, ilen]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nreverb" '(:template "ares nreverb asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] [, inumAlpas] [, ifnAlpas]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "interp" '(:template "ares interp ksig [, iskip] [, imode] [, ivalue]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filescal" '(:template "asig[,asig2] filescal ktimescal, kamp, kpitch, Sfile, klock [,ifftsize, idecim, ithresh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "aftouch" '(:template "kaft aftouch [imin] [, imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outh" '(:template "outh asig1, asig2, asig3, asig4, asig5, asig6" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgpluck" '(:template "ares wgpluck icps, iamp, kpick, iplk, idamp, ifilt, axcite" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "jitter" '(:template "kout jitter kamp, kcpsMin, kcpsMax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lorenz" '(:template "ax, ay, az lorenz ksv, krv, kbv, kh, ix, iy, iz, iskip [, iskipinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inletf" '(:template "fsignal inletf Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsfwrite" '(:template "pvsfwrite fsig, ifile" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butlp" '(:template "ares butlp asig, kfreq [, iskip]
ares butlp asig, afreq [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "connect" '(:template "connect Tsource1, Soutlet1, Tsink1, Sinlet1" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "exitnow" '(:template "exitnow [ivalue]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inq" '(:template "ar1, ar2, ar3, a4 inq" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap16" '(:template "ar1, ..., ar16 vbap16 asig, kazim [, kelev] [, kspread]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "babo" '(:template "a1, a2 babo asig, ksrcx, ksrcy, ksrcz, irx, iry, irz [, idiff] [, ifno]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fftinv" '(:template "kout[] fftinv kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hvs1" '(:template "hvs1 kx, inumParms, inumPointsX, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vcella" '(:template "vcella ktrig, kreinit, ioutFunc, initStateFunc, iRuleFunc, ielements, irulelen [, iradius]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mxadsr" '(:template "ares mxadsr iatt, idec, islev, irel [, idel] [, ireltim]
kres mxadsr iatt, idec, islev, irel [, idel] [, ireltim]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "OSClisten" '(:template "kans OSClisten ihandle, idest, itype [, xdata1, xdata2, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerGetLevel" '(:template "kgain MixerGetLevel isend, ibuss" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readk3" '(:template "kr1, kr2, kr3 readk3 ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tlineto" '(:template "kres tlineto ksig, ktime, ktrig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "binit" '(:template "fsig binit fin, isize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpfreson" '(:template "ares lpfreson asig, kfrqratio" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ifndef" '(:template "num;ifndef NAME
....
num;else#160;
....
num;end#160;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opand" '(:template "a amp;amp; b (logical AND; not audio-rate)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablew" '(:template "tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lenarray" '(:template "ir lenarray karray[, iwhich]
kr lenarray karray[, iwhich]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "reverbsc" '(:template "aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rezzy" '(:template "ares rezzy asig, xfco, xres [, imode, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trshift" '(:template "fsig trshift fin, kpshift[, kgain]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expsegb" '(:template "ares expsegb ia, itim1, ib [, itim2] [, ic] [...]
kres expsegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "poscil3" '(:template "ares poscil3 aamp, acps [, ifn, iphs]
ares poscil3 aamp, kcps [, ifn, iphs]
ares poscil3 kamp, acps [, ifn, iphs]
ares poscil3 kamp, kcps [, ifn, iphs]
ires poscil3 kamp, kcps [, ifn, iphs]
kres poscil3 kamp, kcps [, ifn, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsarp" '(:template "fsig pvsarp fsigin, kbin, kdepth, kgain" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpstmid" '(:template "icps cpstmid ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvscale" '(:template "fsig pvscale fsigin, kscal[, kkeepform, kgain, kcoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expon" '(:template "ares expon ia, idur, ib
kres expon ia, idur, ib" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tempest" '(:template "ktemp tempest kin, iprd, imindur, imemdur, ihp, ithresh, ihtim, ixfdbak, istartempo, ifn [, idisprd] [, itweek]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsmooth" '(:template "fsig pvsmooth fsigin, kacf, kfcf" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "puts" '(:template "puts Sstr, ktrig[, inonl]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vibrato" '(:template "kout vibrato kAverageAmp, kAverageFreq, kRandAmountAmp, kRandAmountFreq, kAmpMinRate, kAmpMaxRate, kcpsMinRate, kcpsMaxRate, ifn [, iphs" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnparams" '(:template "itype, imode, ictltype, idflt, imin, imax chnparams Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmrhode" '(:template "ares fmrhode kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vpow" '(:template "vpow ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ampmidid" '(:template "iamplitude ampmidid ivelocity, idecibels
kamplitude ampmidid kvelocity, idecibels" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftlen" '(:template "ftlen(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gausstrig" '(:template "ares gausstrig kamp, kcps, kdev [, imode] [, ifrst1]
kres gausstrig kamp, kcps, kdev [, imode] [, ifrst1]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfinstr" '(:template "ar1, ar2 sfinstr ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dam" '(:template "ares dam asig, kthreshold, icomp1, icomp2, irtime, iftime" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "print" '(:template "print iarg [, iarg1] [, iarg2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vexpseg" '(:template "vexpseg ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "samphold" '(:template "ares samphold asig, agate [, ival] [, ivstor]
kres samphold ksig, kgate [, ival] [, ivstor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pchoct" '(:template "pchoct (oct) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSpartialtap" '(:template "kfrq, kamp ATSpartialtap ipartialnum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidOut" '(:template "aleft, aright fluidOut ienginenum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bformdec" '(:template "ao1, ao2 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "aresonk" '(:template "kres aresonk ksig, kcf, kbw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "veloc" '(:template "ival veloc [ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableng" '(:template "ires tableng ifn
kres tableng kfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vadd" '(:template "vadd ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvcross" '(:template "ares pvcross ktimpnt, kfmod, ifile, kampscale1, kampscale2 [, ispecwp]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKWurley" '(:template "asignal STKWurley ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mididefault" '(:template "mididefault xdefault, xvalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfilist" '(:template "sfilist ifilhandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidEngine" '(:template "ienginenum fluidEngine [iReverbEnabled] [, iChorusEnabled] [,iNumChannels] [, iPolyphony]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sndloop" '(:template "asig, krec sndloop ain, kpitch, ktrig, idur, ifad" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cudasynth" '(:template "asig cudasynth kamp, kfreq, itab, iftab, iatab[, inum]
asig cudasynth fsig, kamp, kfreq[, inum]
asig cudasynth fsig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pgmassign" '(:template "pgmassign ipgm, inst[, ichn]
pgmassign ipgm, quot;insnamequot;[, ichn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gbuzz" '(:template "ares gbuzz xamp, xcps, knh, klh, kmul, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filesr" '(:template "ir filesr ifilcod [, iallowraw]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "minabs" '(:template "amin minabs ain1, ain2 [, ain3] [, ain4] [...]
kmin minabs kin1, kin2 [, kin3] [, kin4] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outs" '(:template "outs asig1, asig2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chano" '(:template "chano kval, kchan
chano aval, kchan" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lessthan" '(:template "(a lt; b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sprintfk" '(:template "Sdst sprintfk Sfmt, xarg1[, xarg2[, ... ]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "igoto" '(:template "igoto label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "transeg" '(:template "ares transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
kres transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablefilter" '(:template "knumpassed tablefilter kouttable, kintatble, kmode, kparam" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSadd" '(:template "ar ATSadd ktimepnt, kfmod, iatsfile, ifn, ipartials[, ipartialoffset, ipartialincr, igatefn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sflooper" '(:template "ar1, ar2 sflooper ivel, inotenum, kamp, kpitch, ipreindex, kloopstart, kloopend, kcrossfade [, istart, imode, ifenv, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetFont" '(:template "FLsetFont ifont, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider16tablef" '(:template "kflag slider16tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum16, imin16, imax16, init16, ifn16, icutoff16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "randh" '(:template "ares randh xamp, xcps [, iseed] [, isize] [, ioffset]
kres randh kamp, kcps [, iseed] [, isize] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tab" '(:template "ir tab_i indx, ifn[, ixmode]
kr tab kndx, ifn[, ixmode]
ar tab xndx, ifn[, ixmode]
tabw_i isig, indx, ifn [,ixmode]
tabw ksig, kndx, ifn [,ixmode]
tabw asig, andx, ifn [,ixmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fof2" '(:template "ares fof2 xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur, kphs, kgliss [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmirror" '(:template "vmirror ifn, kmin, kmax, ielements" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lorismorph" '(:template "lorismorph isrcidx, itgtidx, istoreidx, kfreqmorphenv, kampmorphenv, kbwmorphenv" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ptablew" '(:template "ptablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
ptablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
ptablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cent" '(:template "cent(x)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trcross" '(:template "fsig trcross fin1, fin2, ksearch, kdepth [, kmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gainslider" '(:template "kout gainslider kindex" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zawm" '(:template "zawm asig, kndx [, imix]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstedit" '(:template "vstedit instance" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "space" '(:template "a1, a2, a3, a4 space asig, ifn, ktime, kreverbsend, kx, ky" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerClear" '(:template "MixerClear" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpsxpch" '(:template "icps cpsxpch ipch, iequal, irepeat, ibase" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zamod" '(:template "ares zamod asig, kzamod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pow" '(:template "ares pow aarg, kpow [, inorm]
ires pow iarg, ipow [, inorm]
kres pow karg, kpow [, inorm]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midinoteoff" '(:template "midinoteoff xkey, xvelocity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lposcil" '(:template "ares lposcil kamp, kfreqratio, kloop, kend, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvshift" '(:template "fsig pvshift fsigin, kshift, klowest[, kkeepform, igain, kcoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loopseg" '(:template "ksig loopseg kfreq, ktrig, iphase, kvalue0, ktime0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2][...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strindexk" '(:template "kpos strindexk S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsmaska" '(:template "fsig pvsmaska fsrc, ifn, kdepth" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpread" '(:template "krmsr, krmso, kerr, kcps lpread ktimpnt, ifilcod [, inpoles] [, ifrmrate]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hdf5read" '(:template "xout1[, xout2, xout3, ..., xoutN] hdf5read ifilename, ivariablename1[, ivariablename2, ivariablename3, ..., ivariablenameN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "median" '(:template "ares median asig, ksize, imaxsize [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltapn" '(:template "ares deltapn xnumsamps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inz" '(:template "inz ksig1" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablewk" '(:template "vtablewk kndx, kfn, ixmode, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moog" '(:template "ares moog kamp, kfreq, kfiltq, kfiltrate, kvibf, kvamp, iafn, iwfn, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "faustcompile" '(:template "ihandle faustcompile Scode, Sargs" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trlowest" '(:template "fsig, kfr, kamp trlowest fin1, kscal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "soundouts" '(:template "soundouts asigl, asigr, ifilcod [, iformat]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLvslidBnk" '(:template "FLvslidBnk quot;namesquot;, inumsliders [, ioutable] [, iwidth] [, iheight] [, ix] [, iy] [, itypetable] [, iexptable] [, istart_index] [, iminmaxtable]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "max" '(:template "amax max ain1, ain2 [, ain3] [, ain4] [...]
kmax max kin1, kin2 [, kin3] [, kin4] [...]
imax max iin1, iin2 [, iin3] [, iin4] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap" '(:template "ar1[, ar2...] vbap asig, kazim [, kelev] [, kspread] [, ilayout]
array[] vbap asig, kazim [, kelev] [, kspread] [, ilayout]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sliderKawai" '(:template "k1, k2, ...., k16 sliderKawai imin1, imax1, init1, ifn1, imin2, imax2, init2, ifn2, ..., imin16, imax16, init16, ifn16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expcurve" '(:template "kout expcurve kindex, ksteepness" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablei" '(:template "vtablei indx, ifn, interp, ixmode, iout1 [, iout2, iout3, .... , ioutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "plusbecomes" '(:template "ares += xarg
ires += iarg
kres += karg
table [ kval] += karg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbin" '(:template "kamp, kfr pvsbin fsig, kbin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linsegr" '(:template "ares linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "setcol" '(:template "kout[] setcolkin[],kcol" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvspitch" '(:template "kfr, kamp pvspitch fsig, kthresh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "round" '(:template "round(x) (init-, control-, or audio-rate arg allowed)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "distort1" '(:template "ares distort1 asig, kpregain, kpostgain, kshape1, kshape2[, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpsmidib" '(:template "icps cpsmidib [irange]
kcps cpsmidib [irange]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableicopy" '(:template "tableicopy idft, isft" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dssiaudio" '(:template "[aout1, aout2, ..., aout9] dssiaudio ihandle, [ain1, ain2, ..., ain9]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wiirange" '(:template "wiirange icontrol, iminimum, imaximum[, inum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ziw" '(:template "ziw isig, indx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "display" '(:template "display xsig, iprd [, inprds] [, iwtflg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octmidib" '(:template "ioct octmidib [irange]
koct octmidib [irange]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "voice" '(:template "ares voice kamp, kfreq, kphoneme, kform, kvibf, kvamp, ifn, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidLoad" '(:template "isfnum fluidLoad soundfont, ienginenum[, ilistpresets]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablexkt" '(:template "ares tablexkt xndx, kfn, kwarp, iwsize [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gendyx" '(:template "ares gendyx kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]
kres gendyx kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scalearray" '(:template "scalearray tarray, kmin, kmax[, kleft, kright]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "atonex" '(:template "ares atonex asig, khp [, inumlayer] [, iskip]
ares atonex asig, ahp [, inumlayer] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sinsyn" '(:template "asig sinsyn fin, kscal, kmaxtracks, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resonz" '(:template "ares resonz asig, xcf, xbw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "granule" '(:template "ares granule xamp, ivoice, iratio, imode, ithd, ifn, ipshift, igskip, igskip_os, ilength, kgap, igap_os, kgsize, igsize_os, iatt, idec [, iseed] [, ipitch1] [, ipitch2] [, ipitch3] [, ipitch4] [, ifnenv]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zakinit" '(:template "zakinit isizea, isizek" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpshold" '(:template "ksig lpshold kfreq, ktrig, iphase, ktime0, kvalue0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpsoct" '(:template "cpsoct (oct) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sndwarpst" '(:template "ar1, ar2 [,ac1] [, ac2] sndwarpst xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, irandw, ioverlap, ifn2, itimemode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "insremot" '(:template "insremot idestination, isource, instrnum [,instrnum...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSbufread" '(:template "ATSbufread ktimepnt, kfmod, iatsfile, ipartials[, ipartialoffset, ipartialincr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "invalue" '(:template "ivalue invalue quot;channel namequot;
kvalue invalue quot;channel namequot;
Sname invalue quot;channel namequot;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider16f" '(:template "k1,...,k16 slider16f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1,..., ictlnum16, imin16, imax16, init16, ifn16, icutoff16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outch" '(:template "outch kchan1, asig1 [, kchan2] [, asig2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetColor" '(:template "FLsetColor ired, igreen, iblue, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zfilter2" '(:template "ares zfilter2 asig, kdamp, kfreq, iM, iN, ib0, ib1, ..., ibM, ia1,ia2, ..., iaN" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vlimit" '(:template "vlimit ifn, kmin, kmax, ielements" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "syncphasor" '(:template "aphase, asyncout syncphasor xcps, asyncin, [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xscans" '(:template "ares xscans kamp, kfreq, ifntraj, id [, iorder]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mincer" '(:template "asig mincer atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "powoftwo" '(:template "powoftwo(x) (init-rate or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "prealloc" '(:template "prealloc insnum, icount
prealloc quot;insnamequot;, icount" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider16" '(:template "i1,...,i16 slider16 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum16, imin16, imax16, init16, ifn16
k1,...,k16 slider16 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum16, imin16, imax16, init16, ifn16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filelen" '(:template "ir filelen ifilcod, [iallowraw]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpstuni" '(:template "icps cpstuni index, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vibes" '(:template "ares vibes kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pop_f" '(:template "fsig pop_f" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider64" '(:template "i1,...,i64 slider64 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum64, imin64, imax64, init64, ifn64
k1,...,k64 slider64 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum64, imin64, imax64, init64, ifn64" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sinh" '(:template "sinh(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tan" '(:template "tan(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnkSet" '(:template "FLslidBnkSet ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLcount" '(:template "kout, ihandle FLcount quot;labelquot;, imin, imax, istep1, istep2, itype, iwidth, iheight, ix, iy, iopcode [, kp1] [, kp2] [, kp3] [...] [, kpN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "crunch" '(:template "ares crunch iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bexprnd" '(:template "ares bexprnd krange
ires bexprnd krange
kres bexprnd krange" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgpluck2" '(:template "ares wgpluck2 iplk, kamp, icps, kpick, krefl" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLhide" '(:template "FLhide ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap8move" '(:template "ar1, ..., ar8 vbap8move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midremot" '(:template "midremot idestination, isource, instrnum [,instrnum...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butterbr" '(:template "ares butterbr asig, xfreq, xband [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "noteoff" '(:template "noteoff ichn, inum, ivel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vpowv" '(:template "vpowv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATScross" '(:template "ar ATScross ktimepnt, kfmod, iatsfile, ifn, kmylev, kbuflev, ipartials [, ipartialoffset, ipartialincr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftgentmp" '(:template "ifno ftgentmp ip1, ip2dummy, isize, igen, iarga, iargb, ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ampdb" '(:template "ampdb(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "genarray_i" '(:template "karray genarray_i istart, iend [,inc]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maxabs" '(:template "amax maxabs ain1, ain2 [, ain3] [, ain4] [...]
kmax maxabs kin1, kin2 [, kin3] [, kin4] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fft" '(:template "kout[] fft kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscili" '(:template "ares oscili xamp, xcps[, ifn, iphs]
kres oscili kamp, kcps[, ifn, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pdclip" '(:template "aout pdclip ain, kWidth, kCenter [, ibipolar [, ifullscale]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cossegb" '(:template "ares cossegb ia, itim1, ib [, itim2] [, ic] [...]
kres cossegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rnd31" '(:template "ax rnd31 kscl, krpow [, iseed]
ix rnd31 iscl, irpow [, iseed]
kx rnd31 kscl, krpow [, iseed]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsdisp" '(:template "pvsdisp fsig[, ibins, iwtflg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "unirand" '(:template "ares unirand krange
ires unirand krange
kres unirand krange" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "flooper2" '(:template "asig1[,asig2] flooper2 kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn [, istart, imode, ifenv, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "elseif" '(:template "elseif xa R xb then" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resonr" '(:template "ares resonr asig, xcf, xbw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "phaser1" '(:template "ares phaser1 asig, kfreq, kord, kfeedback [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "platerev" '(:template "a1[, a2, ...] platerev itabexcite. itabouts, kbndry, iaspect, istiff, idecay, iloss, aexcite1[, aexcite2, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zarg" '(:template "ares zarg kndx, kgain" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mfb" '(:template "kout[] mfb kin[],klow,khigh,ibands" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "faustgen" '(:template "ihandle,a1[,a2,...] faustgen SCode[,ain1,...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidAllOut" '(:template "aleft, aright fluidAllOut" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cggoto" '(:template "cggoto condition, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butterhp" '(:template "ares butterhp asig, kfreq [, iskip]
ares butterhp asig, afreq [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "upsamp" '(:template "ares upsamp ksig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcmp" '(:template "ires strcmp S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "osciln" '(:template "ares osciln kamp, ifrq, ifn, itimes" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLlabel" '(:template "FLlabel isize, ifont, ialign, ired, igreen, iblue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nlfilt" '(:template "ares nlfilt ain, ka, kb, kd, kC, kL" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpf18" '(:template "ares lpf18 asig, xfco, xres, xdist [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetTextSize" '(:template "FLsetTextSize isize, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "reverb2" '(:template "ares reverb2 asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] [, inumAlpas] [, ifnAlpas]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "comb" '(:template "ares comb asig, krvt, ilpt [, iskip] [, insmps]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cauchyi" '(:template "ares cauchyi klambda, xamp, xcps
ires cauchyi klambda, xamp, xcps
kres cauchyi klambda, xamp, xcps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pyexec" '(:template "pyexec "filename"
pyexeci "filename"
pylexec "filename"
pylexeci "filename"
pyexect ktrigger, "filename"
plyexect ktrigger, "filename"" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerReceive" '(:template "asignal MixerReceive ibuss, ichannel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sekere" '(:template "ares sekere iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strlowerk" '(:template "Sdst strlowerk Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftsavek" '(:template "ftsavek quot;filenamequot;, ktrig, iflag, ifn1 [, ifn2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scoreline" '(:template "scoreline Sin, ktrig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nchnls_hw" '(:template "idacc,iadcc nchnls_hw" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bformdec1" '(:template "ao1, ao2 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]]
aout[] bformdec1 isetup, abform[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsmix" '(:template "fsig pvsmix fsigin1, fsigin2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslider" '(:template "kout, ihandle FLslider quot;labelquot;, imin, imax, iexp, itype, idisp, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "convolve" '(:template "ar1 [, ar2] [, ar3] [, ar4] convolve ain, ifilcod [, ichannel]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfplist" '(:template "sfplist ifilhandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "table" '(:template "ares table andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires table indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres table kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gain" '(:template "ares gain asig, krms [, ihp] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "min" '(:template "amin min ain1, ain2 [, ain3] [, ain4] [...]
kmin min kin1, kin2 [, kin3] [, kin4] [...]
imin min iin1, iin2 [, iin3] [, iin4] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vexp" '(:template "vexp ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vport" '(:template "vport ifn, khtime, ielements [, ifnInit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcatk" '(:template "Sdst strcatk Ssrc1, Ssrc2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pcount" '(:template "icount pcount" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mute" '(:template "mute insnum [, iswitch]
mute quot;insnamequot; [, iswitch]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerSetLevel_i" '(:template "MixerSetLevel_i isend, ibuss, igain" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strsub" '(:template "Sdst strsub Ssrc[, istart[, iend]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftfree" '(:template "ftfree ifno, iwhen" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zir" '(:template "ir zir indx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "faustctl" '(:template "faustctl idsp,Scontrol,kval" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "until" '(:template "until condition do ... od" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cps2pch" '(:template "icps cps2pch ipch, iequal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midglobal" '(:template "midglobal isource, instrnum [,instrnum...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specscal" '(:template "wsig specscal wsigin, ifscale, ifthresh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider32" '(:template "i1,...,i32 slider32 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum32, imin32, imax32, init32, ifn32
k1,...,k32 slider32 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum32, imin32, imax32, init32, ifn32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "exprand" '(:template "ares exprand klambda
ires exprand klambda
kres exprand klambda" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moogladder2" '(:template "asig moogladder2 ain, kcf, kres[, istor]
asig moogladder2 ain, acf, kres[, istor]
asig moogladder2 ain, kcf, ares[, istor]
asig moogladder2 ain, acf, ares[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "port" '(:template "kres port ksig, ihtim [, isig]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opbitor" '(:template "a verbar; b (bitwise OR)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pcauchy" '(:template "ares pcauchy kalpha
ires pcauchy kalpha
kres pcauchy kalpha" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outic14" '(:template "outic14 ichn, imsb, ilsb, ivalue, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octave" '(:template "octave(x)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vibr" '(:template "kout vibr kAverageAmp, kAverageFreq, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "temposcal" '(:template "asig temposcal ktimescal, kamp, kpitch, ktab, klock [,ifftsize, idecim, ithresh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstbankload" '(:template "vstbankload instance, ipath" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rect2pol" '(:template "kout[] rect2pol kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider8f" '(:template "k1,...,k8 slider8f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, ..., ictlnum8, imin8, imax8, init8, ifn8, icutoff8" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cepsinv" '(:template "kenv cepsinv keps[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pitchamdf" '(:template "kcps, krms pitchamdf asig, imincps, imaxcps [, icps] [, imedi] [, idowns] [, iexcps] [, irmsmedi]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetColor2" '(:template "FLsetColor2 ired, igreen, iblue, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maxalloc" '(:template "maxalloc insnum, icount
maxalloc Sinsname, icount" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "adsyn" '(:template "ares adsyn kamod, kfmod, ksmod, ifilcod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lposcilsa2" '(:template "ar1, ar2 lposcilsa2 aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftloadk" '(:template "ftloadk quot;filenamequot;, ktrig, iflag, ifn1 [, ifn2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vexpv_i" '(:template "vexpv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lposcila" '(:template "ar lposcila aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftsr" '(:template "ftsr(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider8table" '(:template "kflag slider8table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsceps" '(:template "keps[] pvsceps fsig[, icoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "seqtime2" '(:template "ktrig_out seqtime2 ktrig_in, ktime_unit, kstart, kloop, kinitndx, kfn_times" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkpc" '(:template "outkpc kchn, kprog, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayxs" '(:template "aout1, aout2 vdelayxs ain1, ain2, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "multitap" '(:template "ares multitap asig [, itime1, igain1] [, itime2, igain2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "diskgrain" '(:template "asig diskgrain Sfname, kamp, kfreq, kpitch, kgrsize, kprate, ifun, iolaps [,imaxgrsize , ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "joystick" '(:template "kres joystick kdevice ktab" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fold" '(:template "ares fold asig, kincr" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readk2" '(:template "kr1, kr2 readk2 ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vcopy" '(:template "vcopy ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [, kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xin" '(:template "xinarg1 [, xinarg2] ... [xinargN] xin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvscross" '(:template "fsig pvscross fsrc, fdest, kamp1, kamp2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider64tablef" '(:template "kflag slider64tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum64, imin64, imax64, init64, ifn64, icutoff64" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vrandh" '(:template "vrandh ifn, krange, kcps, ielements [, idstoffset] [, iseed] [, isize] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerSend" '(:template "MixerSend asignal, isend, ibuss, ichannel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidControl" '(:template "fluidControl ienginenum, kstatus, kchannel, kdata1, kdata2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "active" '(:template "ir active insnum [,iopt [,inorel]]
ir active Sinsname [,iopt [,inorel]]
kres active kinsnum [,iopt [,inorel]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nchnls" '(:template "nchnls = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "control" '(:template "kres control knum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "date" '(:template "ir[, inano] date
kr[, knano] date" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "p" '(:template "p(x)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "modulus" '(:template "a percnt; b (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLbutton" '(:template "kout, ihandle FLbutton quot;labelquot;, ion, ioff, itype, iwidth, iheight, ix, iy, iopcode [, kp1] [, kp2] [, kp3] [, kp4] [, kp5] [....] [, kpN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "exp" '(:template "exp(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "undef" '(:template "num;undef NAME" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midion2" '(:template "midion2 kchn, knum, kvel, ktrig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strrindexk" '(:template "kpos strrindexk S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linsegb" '(:template "ares linsegb ia, itim1, ib [, itim2] [, ic] [...]
kres linsegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bbcutm" '(:template "a1 bbcutm asource, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trigseq" '(:template "trigseq ktrig_in, kstart, kloop, kinitndx, kfn_values, kout1 [, kout2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftsamplebank" '(:template "iNumberOfFile ftsamplebank SDirectory, iFirstTableNumber, iTrigger, iSkipTime, iFormat, iChannel,
kNumberOfFile ftsamplebank SDirectory, kFirstTableNumber, kTrigger, kSkipTime, kFormat, kChannel," :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chn" '(:template "chn_k Sname, imode[, itype, idflt, imin, ima, ix, iy, iwidth, iheight, Sattributes]
chn_a Sname, imode
chn_S Sname, imode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbapz" '(:template "vbapz inumchnls, istartndx, asig, kazim [, kelev] [, kspread]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dust" '(:template "ares dust kamp, kdensity
kres dust kamp, kdensity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsdiskin" '(:template "fsig pvsdiskin SFname,ktscal,kgain[,ioffset, ichan]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "define" '(:template "num;define NAME num; replacement text num;
num;define NAME(aapos; bapos; capos;) num; replacement text num;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vadd_i" '(:template "vadd_i ifn, ival, ielements [, idstoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vlowres" '(:template "ares vlowres asig, kfco, kres, iord, ksep" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayxwq" '(:template "aout1, aout2, aout3, aout4 vdelayxwq ain1, ain2, ain3, ain4, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xscanmap" '(:template "kpos, kvel xscanmap iscan, kamp, kvamp [, iwhich]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "planet" '(:template "ax, ay, az planet kmass1, kmass2, ksep, ix, iy, iz, ivx, ivy, ivz, idelta [, ifriction] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKSimple" '(:template "asignal STKSimple ifrequency, iamplitude, [kpos, kv1[, kcross, kv2[, kenv, kv3[, kgain, kv4]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "locsend" '(:template "a1, a2 locsend
a1, a2, a3, a4 locsend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ino" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8 ino" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mrtmsg" '(:template "mrtmsg imsgtype" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap16move" '(:template "ar1, ..., ar16 vbap16move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "copyf2array" '(:template "copyf2array tab, kftbl" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnclear" '(:template "chnclear Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midion" '(:template "midion kchn, knum, kvel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "getrow" '(:template "kout[] getrowkin[],krow" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "weibull" '(:template "ares weibull ksigma, ktau
ires weibull ksigma, ktau
kres weibull ksigma, ktau" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtable1k" '(:template "vtable1k kfn,kout1 [, kout2, kout3, .... , koutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "valpass" '(:template "ares valpass asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scans" '(:template "ares scans kamp, kfreq, ifn, id [, iorder]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pyassign" '(:template "pyassign "variable", kvalue
pyassigni "variable", ivalue
pylassign "variable", kvalue
pylassigni "variable", ivalue
pyassignt ktrigger, "variable", kvalue
pylassignt ktrigger, "variable", kvalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagecreate" '(:template "iimagenum imagecreate iwidth, iheight" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fprints" '(:template "fprints quot;filenamequot;, quot;stringquot; [, ival1] [, ival2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ops" '(:template "S(x) (control-rate or init-rate arg)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKRhodey" '(:template "asignal STKRhodey ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dust2" '(:template "ares dust2 kamp, kdensity
kres dust2 kamp, kdensity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfinstr3m" '(:template "ares sfinstr3m ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablefilteri" '(:template "inumpassed tablefilteri iouttable, iintatble, imode, iparam" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midichannelaftertouch" '(:template "midichannelaftertouch xchannelaftertouch [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "log2" '(:template "log2(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "divz" '(:template "ares divz xa, xb, ksubst
ires divz ia, ib, isubst
kres divz ka, kb, ksubst
...divz(ka, kb, ksubst)... (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "biquada" '(:template "ares biquada asig, ab0, ab1, ab2, aa0, aa1, aa2 [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfplaym" '(:template "ares sfplaym ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfmove" '(:template "aleft, aright hrtfmove asrc, kAz, kElev, ifilel, ifiler [, imode, ifade, isr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cngoto" '(:template "cngoto condition, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "minarray" '(:template "kmin [,kindx] minarray karray" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "urandom" '(:template "ax urandom [imin, imax]
ix urandom [imin, imax]
kx urandom [imin, imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outq" '(:template "outq asig1, asig2, asig3, asig4" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specdiff" '(:template "wsig specdiff wsigin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fillarray" '(:template "karray[] fillarray ival1, ival2,.....ivaln
karray fillarray ival1, ival2,.....ivaln" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tbvcf" '(:template "ares tbvcf asig, xfco, xres, kdist, kasym [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetSize" '(:template "FLsetSize iwidth, iheight, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tonek" '(:template "kres tonek ksig, khp [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midic7" '(:template "idest midic7 ictlno, imin, imax [, ifn]
kdest midic7 ictlno, kmin, kmax [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chanctrl" '(:template "ival chanctrl ichnl, ictlno [, ilow] [, ihigh]
kval chanctrl ichnl, ictlno [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vlinseg" '(:template "vlinseg ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKPlucked" '(:template "asignal STKPlucked ifrequency, iamplitude" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "printks2" '(:template "printks2 quot;stringquot;, kval" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fofilter" '(:template "asig fofilter ain, xcf, xris, xdec[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cauchy" '(:template "ares cauchy kalpha
ires cauchy kalpha
kres cauchy kalpha" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "noteon" '(:template "noteon ichn, inum, ivel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dripwater" '(:template "ares dripwater kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablewi" '(:template "vtablewi indx, ifn, ixmode, inarg1 [, inarg2, inarg3 , .... , inargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zkr" '(:template "kres zkr kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pdhalfy" '(:template "aout pdhalfy ain, kShapeAmount [, ibipolar [, ifullscale]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmult" '(:template "vmult ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outipc" '(:template "outipc ichn, iprog, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "noteondur2" '(:template "noteondur2 ichn, inum, ivel, idur" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trsplit" '(:template "fsiglow, fsighi trsplit fin, ksplit[, kgainlow, kgainhigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftconv" '(:template "a1[, a2[, a3[, ... a8]]] ftconv ain, ift, iplen[, iskipsamples [, iirlen[, iskipinit]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trfilter" '(:template "fsig trfilter fin, kamnt, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linrand" '(:template "ares linrand krange
ires linrand krange
kres linrand krange" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ziwm" '(:template "ziwm isig, indx [, imix]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "combinv" '(:template "ares combinv asig, krvt, ilpt [, iskip] [, insmps]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loop_lt" '(:template "loop_lt indx, incr, imax, label
loop_lt kndx, kncr, kmax, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vphaseseg" '(:template "vphaseseg kphase, ioutab, ielems, itab1,idist1,itab2 [,idist2,itab3, ... ,idistN-1,itabN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cmplxprod" '(:template "kout[] cmplxprod kin1[], kin2[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "atonek" '(:template "kres atonek ksig, khp [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "denorm" '(:template "denorm a1[, a2[, a3[, ... ]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfpreset" '(:template "ir sfpreset iprog, ibank, ifilhandle, ipreindex" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zkwm" '(:template "zkwm ksig, kndx [, imix]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vaset" '(:template "vaset kval, kndx, avar" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfstat" '(:template "aleft, aright hrtfstat asrc, iAz, iElev, ifilel, ifiler [,iradius, isr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "follow" '(:template "ares follow asig, idt" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "phasorbnk" '(:template "ares phasorbnk xcps, kndx, icnt [, iphs]
kres phasorbnk kcps, kndx, icnt [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelay" '(:template "ares vdelay asig, adel, imaxdel [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octmidinn" '(:template "octmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBandedWG" '(:template "asignal STKBandedWG ifrequency, iamplitude, [kpress, kv1[, kmot, kv2[, klfo, kv3[, klfodepth, kv4[, kvel, kv5[, kstrk, kv6[, kinstr, kv7]]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vwrap" '(:template "vwrap ifn, kmin, kmax, ielements" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "evalstr" '(:template "ires evalstr Scode
kres evalstr Scode, ktrig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readscore" '(:template "readscore Sin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtabwk" '(:template "vtabwk kndx, ifn, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabrec" '(:template "tabrec ktrig_start, ktrig_stop, knumtics, kfn, kin1 [,kin2,...,kinN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpsholdp" '(:template "ksig lpsholdp kphase, kvalue0, ktime0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "kgoto" '(:template "kgoto label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vcopy_i" '(:template "vcopy_i ifn1, ifn2, ielements [,idstoffset, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKSaxofony" '(:template "asignal STKSaxofony ifrequency, iamplitude, [kstiff, kv1[, kapert, kv2[, kblow, kv3[, knoise, kv4[, klfo, kv5[, klfodepth, kv6[, kbreath, kv7]]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scale" '(:template "kscl scale kinput, kmax, kmin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "push_f" '(:template "push_f fsig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vpowv_i" '(:template "vpowv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "event" '(:template "event quot;scorecharquot;, kinsnum, kdelay, kdur, [, kp4] [, kp5] [, ...]
event quot;scorecharquot;, quot;insnamequot;, kdelay, kdur, [, kp4] [, kp5] [, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbuffer" '(:template "ihandle, ktime pvsbuffer fsig, ilen" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mediank" '(:template "kres mediank kin, ksize, imaxsize [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wiiconnect" '(:template "ires wiiconnect [itimeout, imaxnum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pan2" '(:template "a1, a2 pan2 asig, xp [, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loopxseg" '(:template "ksig loopxseg kfreq, ktrig, iphase, ktime0, kvalue0 [, ktime1] [, kvalue1] [, ktime2] [, kvalue2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midic14" '(:template "idest midic14 ictlno1, ictlno2, imin, imax [, ifn]
kdest midic14 ictlno1, ictlno2, kmin, kmax [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "grain2" '(:template "ares grain2 kcps, kfmd, kgdur, iovrlp, kfn, iwfn [, irpow] [, iseed] [, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lowres" '(:template "ares lowres asig, kcutoff, kresonance [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lua_exec" '(:template "lua_exec Sluacode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tradsyn" '(:template "asig tradsyn fin, kscal, kpitch, kmaxtracks, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scoreline_i" '(:template "scoreline_i Sin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hvs3" '(:template "hvs3 kx, ky, kz, inumParms, inumPointsX, inumPointsY, inumPointsZ, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filevalid" '(:template "ir filevalid ifilcod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKFMVoices" '(:template "asignal STKFMVoices ifrequency, iamplitude, [kvowel, kv1[, kspec, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vexpv" '(:template "vexpv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lfo" '(:template "kres lfo kamp, kcps [, itype]
ares lfo kamp, kcps [, itype]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spectrum" '(:template "wsig spectrum xsig, iprd, iocts, ifrqa [, iq] [, ihann] [, idbout] [, idsprd] [, idsinrs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "atone" '(:template "ares atone asig, khp [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outipat" '(:template "outipat ichn, inotenum, ivalue, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midichn" '(:template "ichn midichn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xtratim" '(:template "xtratim iextradur" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "compilecsd" '(:template "ires compilecsd Sfilename" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strlenk" '(:template "klen strlenk Sstr" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outletv" '(:template "outletv Sname, array" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "goto" '(:template "goto label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLcolor" '(:template "FLcolor ired, igreen, iblue [, ired2, igreen2, iblue2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butbp" '(:template "ares butbp asig, kfreq, kband [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "gauss" '(:template "ares gauss krange
ires gauss krange
kres gauss krange" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vco2ift" '(:template "ifn vco2ift icps, iwave [, inyx]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sprintf" '(:template "Sdst sprintf Sfmt, xarg1[, xarg2[, ... ]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "distort" '(:template "ar distort asig, kdist, ifn[, ihp, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "if" '(:template "if ia R ib igoto label
if ka R kb kgoto label
if xa R xb goto label
if xa R xb then" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "getseed" '(:template "ians getseed
kans getseed" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "soundin" '(:template "ar1[, ar2[, ar3[, ... a24]]] soundin ifilcod [, iskptim] [, iformat] [, iskipinit] [, ibufsize]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zacl" '(:template "zacl kfirst, klast" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcat" '(:template "Sdst strcat Ssrc1, Ssrc2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabplay" '(:template "tabplay ktrig, knumtics, kfn, kout1 [,kout2,..., koutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "printk2" '(:template "printk2 kvar [, inumspaces]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filter2" '(:template "ares filter2 asig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN
kres filter2 ksig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mvchpf" '(:template "asig mvchpf ain, xcf[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "downsamp" '(:template "kres downsamp asig [, iwlen]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "r2c" '(:template "kout[] r2c kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscil1i" '(:template "kres oscil1i idel, kamp, idur [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabmorphak" '(:template "aout tabmorphak aindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ... ifnN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbapgmove" '(:template "kr1[, kr2...] vbapgmove idur, ispread, ifldnum, ifld1 [, ifld2] [...]
karray[] vbapgmove idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLgroup" '(:template "FLgroup quot;labelquot;, iwidth, iheight, ix, iy [, iborder] [, image]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "signum" '(:template "signum(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBrass" '(:template "asignal STKBrass ifrequency, iamplitude, [klip, kv1[, kslide, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "passign" '(:template "ivar1, ... passign [istart]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLpanel" '(:template "FLpanel quot;labelquot;, iwidth, iheight [, ix] [, iy] [, iborder] [, ikbdcapture] [, iclose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "writescratch" '(:template "writescratchival[, index]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLjoy" '(:template "koutx, kouty, ihandlex, ihandley FLjoy quot;labelquot;, iminx, imaxx, iminy, imaxy, iexpx, iexpy, idispx, idispy, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readk" '(:template "kres readk ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "OSCsend" '(:template "OSCsend kwhen, ihost, iport, idestination, itype [, xdata1, xdata2, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pinkish" '(:template "ares pinkish xin [, imethod] [, inumbands] [, iseed] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linenr" '(:template "ares linenr xamp, irise, idec, iatdec
kres linenr kamp, irise, idec, iatdec" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tival" '(:template "ir tival" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLxyin" '(:template "koutx, kouty, kinside FLxyin ioutx_min, ioutx_max, iouty_min, iouty_max, iwindx_min, iwindx_max, iwindy_min, iwindy_max [, iexpx, iexpy, ioutx, iouty]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outo" '(:template "outo asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cabasa" '(:template "ares cabasa iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "jitter2" '(:template "kout jitter2 ktotamp, kamp1, kcps1, kamp2, kcps2, kamp3, kcps3[ , iopt]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sininv" '(:template "sininv(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider32tablef" '(:template "kflag slider32tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum32, imin32, imax32, init32, ifn32, icutoff32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "log10" '(:template "log10(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "urd" '(:template "aout = urd(ktableNum)
iout = urd(itableNum)
kout = urd(ktableNum)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscils" '(:template "ares oscils iamp, icps, iphs [, iflg]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvstanal" '(:template "fsig pvstanal ktimescal, kamp, kpitch, ktab, [kdetect, kwrap, ioffset,ifftsize, ihop, idbthresh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spdist" '(:template "k1 spdist ifn, ktime, kx, ky" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opnonequiv" '(:template "a # b (bitwise NON EQUIVALENCE)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "timeinsts" '(:template "kres timeinsts" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fractalnoise" '(:template "ares fractalnoise kamp, kbeta" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pwd" '(:template "Sres pwd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfmove2" '(:template "aleft, aright hrtfmove2 asrc, kAz, kElev, ifilel, ifiler [,ioverlap, iradius, isr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsvoc" '(:template "fsig pvsvoc famp, fexc, kdepth, kgain [,kcoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "grain" '(:template "ares grain xamp, xpitch, xdens, kampoff, kpitchoff, kgdur, igfn, iwfn, imgdur [, igrnd]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablewkt" '(:template "tablewkt asig, andx, kfn [, ixmode] [, ixoff] [, iwgmode]
tablewkt ksig, kndx, kfn [, ixmode] [, ixoff] [, iwgmode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zkmod" '(:template "kres zkmod ksig, kzkmod" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "changed2" '(:template "ktrig changed2 kvar1 [, kvar2,..., kvarN]
ktrig changed2 karr[]
ktrig changed2 aarr[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midicontrolchange" '(:template "midicontrolchange xcontroller, xcontrollervalue [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ceps" '(:template "keps[] ceps kmags[][, icoefs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sumarray" '(:template "ksum sumarray karray" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outz" '(:template "outz ksig1" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLvslidBnk2" '(:template "FLvslidBnk2 quot;namesquot;, inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "include" '(:template "num;include" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpspch" '(:template "cpspch (pch) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtabwi" '(:template "vtabwi indx, ifn, inarg1 [, inarg2, inarg3 , .... , inargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mode" '(:template "aout mode ain, xfreq, xQ [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialBegin" '(:template "iPort serialBegin SPortName [, ibaudRate]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "peak" '(:template "kres peak asig
kres peak ksig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmap" '(:template "vmap ifn1, ifn2, ielements [,idstoffset, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "schedkwhennamed" '(:template "schedkwhennamed ktrigger, kmintim, kmaxnum, quot;namequot;, kwhen, kdur [, ip4] [, ip5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltapxw" '(:template "deltapxw ain, adel, iwsize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablewa" '(:template "vtablewa andx, kfn, ixmode, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgbrass" '(:template "ares wgbrass kamp, kfreq, ktens, iatt, kvibf, kvamp [, ifn] [, iminfreq]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nstrnum" '(:template "insno nstrnum quot;namequot;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tab2pvs" '(:template "fsig tab2pvs tvar|karr[][,ihopsize, iwinsize, iwintype]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "endin" '(:template "endin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablecopy" '(:template "tablecopy kdft, ksft" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loscil3" '(:template "ar1 [,ar2] loscil3 xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "looptseg" '(:template "ksig looptseg kfreq, ktrig, iphase, kvalue0, ktype0, ktime0, [, kvalue1] [,ktype1] [, ktime1] [, kvalue2] [,ktype2] [, ktime2] [...] [, kvalueN] [,ktypeN] [, ktimeN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "osciliktp" '(:template "ares osciliktp kcps, kfn, kphs [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidCCi" '(:template "fluidCCi iEngineNumber, iChannelNumber, iControllerNumber, iValue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "system" '(:template "ires system_i itrig, Scmd, [inowait]
kres system ktrig, Scmd, [knowait]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fof" '(:template "ares fof xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur [, iphs] [, ifmode] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expseg" '(:template "ares expseg ia, idur1, ib [, idur2] [, ic] [...]
kres expseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vpow_i" '(:template "vpow_i ifn, ival, ielements [, idstoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscil1" '(:template "kres oscil1 idel, kamp, idur [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outq4" '(:template "outq4 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nstance" '(:template "iHandle nstance insnum, iwhen, idur [, ip4] [, ip5] [...]
iHandle nstance quot;insnamequot;, iwhen, idur [, ip4] [, ip5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsavesnap" '(:template "FLsavesnap quot;filenamequot; [, igroup]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trmix" '(:template "fsig trmix fin1, fin2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpsmidi" '(:template "icps cpsmidi" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "p5gdata" '(:template "kres p5gdata kcontrol" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outic" '(:template "outic ichn, inum, ivalue, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cossegr" '(:template "ares cossegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres cossegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "delayr" '(:template "ares delayr idlt [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "genarray" '(:template "karray genarray kstart, kens[, inc]
iarray genarray istart, iens[, inc]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ntrpol" '(:template "ares ntrpol asig1, asig2, kpoint [, imin] [, imax]
ires ntrpol isig1, isig2, ipoint [, imin] [, imax]
kres ntrpol ksig1, ksig2, kpoint [, imin] [, imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "alwayson" '(:template "alwayson Tinstrument [p4, ..., pn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltap3" '(:template "ares deltap3 xdlt" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBlowBotl" '(:template "asignal STKBlowBotl ifrequency, iamplitude, [knoise, kv1[, klfo, kv2[, klfodepth, kv3[, kvol, kv4]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "setksmps" '(:template "setksmps iksmps" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "seqtime" '(:template "ktrig_out seqtime ktime_unit, kstart, kloop, kinitndx, kfn_times" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "plltrack" '(:template "acps, alock plltrack asig, kd [, kloopf, kloopq, klf, khf, kthresh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "instr" '(:template "instr i, j, ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsinit" '(:template "fsig pvsinit isize[, iolap, iwinsize, iwintype, iformat]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablekt" '(:template "ares tablekt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
kres tablekt kndx, kfn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "harmon" '(:template "ares harmon asig, kestfrq, kmaxvar, kgenfreq1, kgenfreq2, imode, iminfrq, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnset" '(:template "chnset ival, Sname
chnset kval, Sname
chnset aval, Sname
chnset Sval, Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midinoteoncps" '(:template "midinoteoncps xcps, xvelocity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmb3" '(:template "ares fmb3 kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "phaser2" '(:template "ares phaser2 asig, kfreq, kq, kord, kmode, ksep, kfeedback" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "shaker" '(:template "ares shaker kamp, kfreq, kbeans, kdamp, ktimes [, idecay]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "qinf" '(:template "qinf(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vactrol" '(:template "ares vactrol asig [iup, idown]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trhighest" '(:template "fsig, kfr, kamp trhighest fin1, kscal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vbap4move" '(:template "ar1, ar2, ar3, ar4 vbap4move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "faustaudio" '(:template "ihandle,a1[,a2,...] faustaudio ifac[,ain1,...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readclock" '(:template "ir readclock inum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strupperk" '(:template "Sdst strupperk Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wiidata" '(:template "kres wiidata kcontrol[, knum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scanhammer" '(:template "scanhammer isrc, idst, ipos, imult" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outq2" '(:template "outq2 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "betarand" '(:template "ares betarand krange, kalpha, kbeta
ires betarand krange, kalpha, kbeta
kres betarand krange, kalpha, kbeta" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKShakers" '(:template "asignal STKShakers ifrequency, iamplitude, [kenerg, kv1[, kdecay, kv2[, kshake, kv3[, knum, kv4[, kres, kv5[, kinstr, kv6]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabsum" '(:template "kr tabsum ifn[[, kmin] [, kmax]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftload" '(:template "ftload quot;filenamequot;, iflag, ifn1 [, ifn2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpreson" '(:template "ares lpreson asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLbutBank" '(:template "kout, ihandle FLbutBank itype, inumx, inumy, iwidth, iheight, ix, iy, iopcode [, kp1] [, kp2] [, kp3] [, kp4] [, kp5] [....] [, kpN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvinterp" '(:template "ares pvinterp ktimpnt, kfmod, ifile, kfreqscale1, kfreqscale2, kampscale1, kampscale2, kfreqinterp, kampinterp" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cosinv" '(:template "cosinv(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "foutk" '(:template "foutk ifilename, iformat, kout1 [, kout2, kout3,....,koutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spechist" '(:template "wsig spechist wsigin" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "linseg" '(:template "ares linseg ia, idur1, ib [, idur2] [, ic] [...]
kres linseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trscale" '(:template "fsig trscale fin, kpitch[, kgain]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsfilter" '(:template "fsig pvsfilter fsigin, fsigfil, kdepth[, igain]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider64f" '(:template "k1,...,k64 slider64f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1,..., ictlnum64, imin64, imax64, init64, ifn64, icutoff64" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpstun" '(:template "kcps cpstun ktrig, kindex, kfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bamboo" '(:template "ares bamboo kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vosim" '(:template "ar vosim kamp, kFund, kForm, kDecay, kPulseCount, kPulseFactor, ifn [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsftw" '(:template "kflag pvsftw fsrc, ifna [, ifnf]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lua_opcall" '(:template "lua_iopcall Sname, ...
lua_ikopcall Sname, ...
lua_iaopcall Sname, ...
lua_iopcall_off Sname, ...
lua_ikopcall_off Sname, ...
lua_iaopcall_off Sname, ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "doppler" '(:template "ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayxws" '(:template "aout1, aout2 vdelayxws ain1, ain2, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnk" '(:template "FLslidBnk quot;namesquot;, inumsliders [, ioutable] [, iwidth] [, iheight] [, ix] [, iy] [, itypetable] [, iexptable] [, istart_index] [, iminmaxtable]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opbitand" '(:template "a amp; b (bitwise AND)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dumpk2" '(:template "dumpk2 ksig1, ksig2, ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midictrl" '(:template "ival midictrl inum [, imin] [, imax]
kval midictrl inum [, imin] [, imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "compress2" '(:template "ar compress2 aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tanh" '(:template "tanh(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtabi" '(:template "vtabi indx, ifn, iout1 [, iout2, iout3, .... , ioutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "taninv2" '(:template "ares taninv2 ay, ax
ires taninv2 iy, ix
kres taninv2 ky, kx
...taninv2(ky, kx)... (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnmix" '(:template "chnmix aval, Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "abs" '(:template "abs(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayx" '(:template "aout vdelayx ain, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "polynomial" '(:template "aout polynomial ain, k0 [, k1 [, k2 [...]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strget" '(:template "Sdst strget indx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscilikt" '(:template "ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
kres oscilikt kamp, kcps, kfn [, iphs] [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "locsig" '(:template "a1, a2 locsig asig, kdegree, kdistance, kreverbsend
a1, a2, a3, a4 locsig asig, kdegree, kdistance, kreverbsend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "subinstr" '(:template "a1, [...] [, a8] subinstr instrnum [, p4] [, p5] [...]
a1, [...] [, a8] subinstr quot;insnamequot; [, p4] [, p5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbandr" '(:template "fsig pvsbandr fsigin, xlowcut, xlowfull, xhighfull, xhighcut[, ktype]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkat" '(:template "outkat kchn, kvalue, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mandol" '(:template "ares mandol kamp, kfreq, kpluck, kdetune, kgain, ksize [, ifn] [, iminfreq]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "printk" '(:template "printk itime, kval [, ispace]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSsinnoi" '(:template "ar ATSsinnoi ktimepnt, ksinlev, knzlev, kfmod, iatsfile, ipartials [, ipartialoffset, ipartialincr]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scantable" '(:template "aout scantable kamp, kpch, ipos, imass, istiff, idamp, ivel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sandpaper" '(:template "ares sandpaper iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trandom" '(:template "kout trandom ktrig, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "return" '(:template "return ival" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "getcfg" '(:template "Svalue getcfg iopt" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "random" '(:template "ares random kmin, kmax
ires random imin, imax
kres random kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hdf5write" '(:template "hdf5write ifilename, xout1[, xout2, xout3, ..., xoutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sndload" '(:template "sndload Sfname[, ifmt[, ichns[, isr[, ibas[, iamp[, istrt [, ilpmod[, ilps[, ilpe]]]]]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "times" '(:template "ires times
kres times" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "paulstretch" '(:template "asig paulstretch istretch, iwindowsize, ift" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opk" '(:template "k(x) (i-rate args only)
k(x) (a-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfinstr3" '(:template "ar1, ar2 sfinstr3 ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "p5gconnect" '(:template "p5gconnect" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lposcil3" '(:template "ares lposcil3 kamp, kfreqratio, kloop, kend, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "monitor" '(:template "aout1 [,aout2 ... aoutX] monitor
aarra monitor" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLscroll" '(:template "FLscroll iwidth, iheight [, ix] [, iy]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dumpk" '(:template "dumpk ksig, ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "clockoff" '(:template "clockoff inum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "readfi" '(:template "Sres, iline readfi ifilname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider32table" '(:template "kflag slider32table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum32, imin32, imax32, init32, ifn32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcmpk" '(:template "kres strcmpk S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "clfilt" '(:template "ares clfilt asig, kfreq, itype, inpol [, ikind] [, ipbr] [, isba] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfplay" '(:template "ar1, ar2 sfplay ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSread" '(:template "kfreq, kamp ATSread ktimepnt, iatsfile, ipartial" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vexp_i" '(:template "vexp_i ifn, ival, ielements[, idstoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mvclpf2" '(:template "asig mvclpf2 ain, xcf, xres[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vmultv" '(:template "vmultv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "0dbfs" '(:template "0dbfs = iarg
0dbfs" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableimix" '(:template "tableimix idft, idoff, ilen, is1ft, is1off, is1g, is2ft, is2off, is2g" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "copya2ftab" '(:template "copya2ftab kftbl, tab" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialFlush" '(:template "serialFlush iPort" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fareyleni" '(:template "ifl fareyleni ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opbitnot" '(:template "~ a (bitwise NOT)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdivv_i" '(:template "vdivv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "clockon" '(:template "clockon inum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLtext" '(:template "kout, ihandle FLtext quot;labelquot;, imin, imax, istep, itype, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pyrun" '(:template "pyrun "statement"
pyruni "statement"
pylrun "statement"
pylruni "statement"
pyrunt ktrigger, "statement"
pylrunt ktrigger, "statement"" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider16table" '(:template "kflag slider16table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum16, imin16, imax16, init16, ifn16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "newopcodename" '(:template "outarg1, outarg2 newopcodename inarg1, inarg2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imageload" '(:template "iimagenum imageload filename" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "flooper" '(:template "asig1[, asig2] flooper kamp, kpitch, istart, idur, ifad, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "while" '(:template "while condition do ... od" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "buthp" '(:template "ares buthp asig, kfreq [, iskip]
ares buthp asig, afreq [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltapx" '(:template "aout deltapx adel, iwsize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "compress" '(:template "ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ifdef" '(:template "num;ifdef NAME
....
num;else#160;
....
num;end#160;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "biquad" '(:template "ares biquad asig, kb0, kb1, kb2, ka0, ka1, ka2 [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagegetpixel" '(:template "ared, agreen, ablue imagegetpixel iimagenum, ax, ay
kred, kgreen, kblue imagegetpixel iimagenum, kx, ky" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outs1" '(:template "outs1 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cross2" '(:template "ares cross2 ain1, ain2, isize, ioverlap, iwin, kbias" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lorisread" '(:template "lorisread ktimpnt, ifilcod, istoreidx, kfreqenv, kampenv, kbwenv[, ifadetime]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "unwrap" '(:template "kout[] unwrap kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "powershape" '(:template "aout powershape ain, kShapeAmount [, ifullscale]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLgroupEnd" '(:template "FLgroupEnd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "initc21" '(:template "initc21 ichan, ictlno1, ictlno2, ictlno3, ivalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "initc7" '(:template "initc7 ichan, ictlno, ivalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "poscil" '(:template "ares poscil aamp, acps [, ifn, iphs]
ares poscil aamp, kcps [, ifn, iphs]
ares poscil kamp, acps [, ifn, iphs]
ares poscil kamp, kcps [, ifn, iphs]
ires poscil kamp, kcps [, ifn, iphs]
kres poscil kamp, kcps [, ifn, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "c2r" '(:template "kout[] c2r kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strsubk" '(:template "Sdst strsubk Ssrc, kstart, kend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outiat" '(:template "outiat ichn, ivalue, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maca" '(:template "ares maca asig1 , asig2 [, asig3] [, asig4] [, asig5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "poisson" '(:template "ares poisson klambda
ires poisson klambda
kres poisson klambda" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgflute" '(:template "ares wgflute kamp, kfreq, kjet, iatt, idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq] [, ijetrf] [, iendrf]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "adds" '(:template "plus;a (no rate restriction)
a plus; b (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "directory" '(:template "SFiles[] directory SDirectory[, SExtention]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mdelay" '(:template "mdelay kstatus, kchan, kd1, kd2, kdelay" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLpanelEnd" '(:template "FLpanelEnd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loop_gt" '(:template "loop_gt indx, idecr, imin, label
loop_gt kndx, kdecr, kmin, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wguide2" '(:template "ares wguide2 asig, xfreq1, xfreq2, kcutoff1, kcutoff2, kfeedback1, kfeedback2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ctrlinit" '(:template "ctrlinit ichnl, ictlno1, ival1 [, ictlno2] [, ival2] [, ictlno3] [, ival3] [,...ival32]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablemix" '(:template "tablemix kdft, kdoff, klen, ks1ft, ks1off, ks1g, ks2ft, ks2off, ks2g" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "prepiano" '(:template "ares prepiano ifreq, iNS, iD, iK, iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, isspread[, irattles, irubbers]
al,ar prepiano ifreq, iNS, iD, iK, iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, isspread[, irattles, irubbers]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rifft" '(:template "kout[] rifft kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnk2Set" '(:template "FLslidBnk2Set ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moscil" '(:template "moscil kchn, knum, kvel, kdur, kpause" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "marimba" '(:template "ares marimba kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec [, idoubles] [, itriples]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expsegr" '(:template "ares expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midinoteonpch" '(:template "midinoteonpch xpch, xvelocity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "expsega" '(:template "ares expsega ia, idur1, ib [, idur2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "phasor" '(:template "ares phasor xcps [, iphs]
kres phasor kcps [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pchmidi" '(:template "ipch pchmidi" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoAudioOut" '(:template "JackoAudioOut ScsoundPortName, asignal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "phs" '(:template "kout[] phs kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fareylen" '(:template "kfl fareylen kfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLgetsnap" '(:template "inumsnap FLgetsnap index [, igroup]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strlen" '(:template "ilen strlen Sstr" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dcblock" '(:template "ares dcblock ain [, igain]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablewa" '(:template "kstart tablewa kfn, asig, koff" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outvalue" '(:template "outvalue quot;channel namequot;, ivalue
outvalue quot;channel namequot;, kvalue
outvalue quot;channel namequot;, quot;stringquot;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midic21" '(:template "idest midic21 ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
kdest midic21 ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sockrecv" '(:template "asig sockrecv iport, ilength
ksig sockrecv iport, ilength
asigl, asigr sockrecvs iport, ilength
asig strecv Sipaddr, iport" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "product" '(:template "ares product asig1, asig2 [, asig3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkpat" '(:template "outkpat kchn, knotenum, kvalue, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strcpy" '(:template "Sdst strcpy Ssrc
Sdst = Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLbox" '(:template "ihandle FLbox quot;labelquot;, itype, ifont, isize, iwidth, iheight, ix, iy [, image]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ATSinterpread" '(:template "kamp ATSinterpread kfreq" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftgenonce" '(:template "ifno ftgenonce ip1, ip2dummy, isize, igen, iarga, iargb, ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outipb" '(:template "outipb ichn, ivalue, imin, imax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpumeter" '(:template "ktot[,kcpu1, kcpu2,...]cpumeter ifreq" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loscilx" '(:template "ar1 [, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16] loscilx xamp, kcps, ifn [, iwsize, ibas, istrt, imod, ibeg, iend]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maparray" '(:template "karray maparray kinarray, String
karray maparray_i kinarray, String" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "timedseq" '(:template "ktrig timedseq ktimpnt, ifn, kp1 [,kp2, kp3, ...,kpN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstinfo" '(:template "vstinfo instance" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "changed" '(:template "ktrig changed kvar1 [, kvar2,..., kvarN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "metro" '(:template "ktrig metro kfreq [, initphase]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBowed" '(:template "asignal STKBowed ifrequency, iamplitude, [kpress, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opor" '(:template "a verbar;verbar; b (logical OR; not audio-rate)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ctrl21" '(:template "idest ctrl21 ichan, ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
kdest ctrl21 ichan, ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscil" '(:template "ares oscil xamp, xcps [, ifn, iphs]
kres oscil kamp, kcps [, ifn, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dumpk4" '(:template "dumpk4 ksig1, ksig2, ksig3, ksig4, ifilname, iformat, iprd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "subtracts" '(:template "minus;a (no rate restriction)
a minus; b (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnexport" '(:template "gival chnexport Sname, imode[, itype, idflt, imin, imax]
gkval chnexport Sname, imode[, itype, idflt, imin, imax]
gaval chnexport Sname, imode
gSval chnexport Sname, imode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pchmidib" '(:template "ipch pchmidib [irange]
kpch pchmidib [irange]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLhvsBox" '(:template "ihandle FLhvsBox inumlinesX, inumlinesY, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidNote" '(:template "fluidNote ienginenum, ichannelnum, imidikey, imidivel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsanal" '(:template "fsig pvsanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midipolyaftertouch" '(:template "midipolyaftertouch xpolyaftertouch, xcontrollervalue [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "polyaft" '(:template "ires polyaft inote [, ilow] [, ihigh]
kres polyaft inote [, ilow] [, ihigh]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chnget" '(:template "ival chnget Sname
kval chnget Sname
aval chnget Sname
Sval chnget Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "partikkelsync" '(:template "async [,aphase] partikkelsync iopcode_id" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mpulse" '(:template "ares mpulse kamp, kintvl [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "envlpx" '(:template "ares envlpx xamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]
kres envlpx kamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "semitone" '(:template "semitone(x)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lposcilsa" '(:template "ar1, ar2 lposcilsa aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "randomi" '(:template "ares randomi kmin, kmax, xcps [,imode] [,ifirstval]
kres randomi kmin, kmax, kcps [,imode] [,ifirstval]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstparamset" '(:template "vstparamset instance, kparam, kvalue
kvalue vstparamget instance, kparam" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "integ" '(:template "ares integ asig [, iskip]
kres integ ksig [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "eqfil" '(:template "asig eqfil ain, kcf, kbw, kgain[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wrap" '(:template "ares wrap asig, klow, khigh
ires wrap isig, ilow, ihigh
kres wrap ksig, klow, khigh" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spat3di" '(:template "aW, aX, aY, aZ spat3di ain, iX, iY, iZ, idist, ift, imode [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dbfsamp" '(:template "dbfsamp(x) (init-rate or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "delay1" '(:template "ares delay1 asig [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inx" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16 inx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pyeval" '(:template "kresult pyeval "expression"
iresult pyevali "expression"
kresult pyleval "expression"
iresult pylevali "expression"
kresult pyevalt ktrigger, "expression"
kresult pylevalt ktrigger, "expression"" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "schedwhen" '(:template "schedwhen ktrigger, kinsnum, kwhen, kdur [, ip4] [, ip5] [...]
schedwhen ktrigger, quot;insnamequot;, kwhen, kdur [, ip4] [, ip5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKDrummer" '(:template "asignal STKDrummer ifrequency, iamplitude" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnk2Setk" '(:template "FLslidBnk2Setk ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLprintk" '(:template "FLprintk itime, kval, idisp" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "foscili" '(:template "ares foscili xamp, kcps, xcar, xmod, kndx, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outrg" '(:template "outrg kstart, aout1 [,aout2, aout3, ..., aoutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLvkeybd" '(:template "FLvkeybd quot;keyboard.mapquot;, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtabk" '(:template "vtabk kndx, ifn, kout1 [, kout2, kout3, .... , koutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "noise" '(:template "ares noise xamp, kbeta" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLslidBnk2" '(:template "FLslidBnk2 quot;namesquot;, inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]
FLslidBnk2 istring, inumsliders, ioutable, iconfigtable [,iwidth, iheight, ix, iy, istart_index]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetTextType" '(:template "FLsetTextType itype, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sin" '(:template "sin(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pdhalf" '(:template "aout pdhalf ain, kShapeAmount [, ibipolar [, ifullscale]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loop_ge" '(:template "loop_ge indx, idecr, imin, label
loop_ge kndx, kdecr, kmin, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "logcurve" '(:template "kout logcurve kindex, ksteepness" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "minabsaccum" '(:template "minabsaccum aAccumulator, aInput" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opi" '(:template "i(x) (control-rate or init-rate arg)
i(karray,index1, ...) (k-array with indices)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablei" '(:template "ares tablei andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires tablei indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres tablei kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsblur" '(:template "fsig pvsblur fsigin, kblurtime, imaxdel" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midinoteonkey" '(:template "midinoteonkey xkey, xvelocity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetTextColor" '(:template "FLsetTextColor ired, iblue, igreen, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscil3" '(:template "ares oscil3 xamp, xcps [, ifn, iphs]
kres oscil3 kamp, kcps [, ifn, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsynth" '(:template "ares pvsynth fsrc, [iinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmmetal" '(:template "ares fmmetal kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgclar" '(:template "ares wgclar kamp, kfreq, kstiff, iatt, idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resonxk" '(:template "kres resonxk ksig, kcf, kbw[, inumlayer, iscl, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mvclpf3" '(:template "asig mvclpf3 ain, xcf, xres[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialWrite" '(:template "serialWrite iPort, iByte
serialWrite iPort, kByte
serialWrite iPort, SBytes" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fouti" '(:template "fouti ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider32f" '(:template "k1,...,k32 slider32f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, ..., ictlnum32, imin32, imax32, init32, ifn32, icutoff32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tb" '(:template "tb0_init ifn
tb1_init ifn
tb2_init ifn
tb3_init ifn
tb4_init ifn
tb5_init ifn
tb6_init ifn
tb7_init ifn
tb8_init ifn
tb9_init ifn
tb10_init ifn
tb11_init ifn
tb12_init ifn
tb13_init ifn
tb14_init ifn
tb15_init ifn
iout = tb0(iIndex)
kout = tb0(kIndex)
iout = tb1(iIndex)
kout = tb1(kIndex)
iout = tb2(iIndex)
kout = tb2(kIndex)
iout = tb3(iIndex)
kout = tb3(kIndex)
iout = tb4(iIndex)
kout = tb4(kIndex)
iout = tb5(iIndex)
kout = tb5(kIndex)
iout = tb6(iIndex)
kout = tb6(kIndex)
iout = tb7(iIndex)
kout = tb7(kIndex)
iout = tb8(iIndex)
kout = tb8(kIndex)
iout = tb9(iIndex)
kout = tb9(kIndex)
iout = tb10(iIndex)
kout = tb10(kIndex)
iout = tb11(iIndex)
kout = tb11(kIndex)
iout = tb12(iIndex)
kout = tb12(kIndex)
iout = tb13(iIndex)
kout = tb13(kIndex)
iout = tb14(iIndex)
kout = tb14(kIndex)
iout = tb15(iIndex)
kout = tb15(kIndex)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "birnd" '(:template "birnd(x) (init- or control-rate only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltap" '(:template "ares deltap kdlt" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "select" '(:template "aout select a1, a2, aless, aequal, amore" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "barmodel" '(:template "ares barmodel kbcL, kbcR, iK, ib, kscan, iT30, ipos, ivel, iwid" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strset" '(:template "strset iarg, istring" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "delayk" '(:template "kr delayk ksig, idel[, imode]
kr vdel_k ksig, kdel, imdel[, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ctrl7" '(:template "idest ctrl7 ichan, ictlno, imin, imax [, ifn]
kdest ctrl7 ichan, ictlno, kmin, kmax [, ifn]
adest ctrl7 ichan, ictlno, kmin, kmax [, ifn] [, icutoff]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strupper" '(:template "Sdst strupper Ssrc" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "out32" '(:template "out32 asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, asig10, asig11, asig12, asig13, asig14, asig15, asig16, asig17, asig18, asig19, asig20, asig21, asig22, asig23, asig24, asig25, asig26, asig27, asig28, asig29, asig30, asig31, asig32" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ftcps" '(:template "ftcps(x) (init-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstinit" '(:template "instance vstinit ilibrarypath [,iverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "butbr" '(:template "ares butbr asig, kfreq, kband [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialPrint" '(:template "serialPrint iPort" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "getcol" '(:template "kout[] getcolkin[],kcol" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLupdate" '(:template "FLupdate" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablek" '(:template "vtablek kndx, kfn, kinterp, ixmode, kout1 [, kout2, kout3, .... , koutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xyin" '(:template "kx, ky xyin iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strchar" '(:template "ichr strchar Sstr[, ipos]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkc14" '(:template "outkc14 kchn, kmsb, klsb, kvalue, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbufread2" '(:template "fsig pvsbufread2 ktime, khandle, ift1, ift2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pan" '(:template "a1, a2, a3, a4 pan asig, kx, ky, ifn [, imode] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "randi" '(:template "ares randi xamp, xcps [, iseed] [, isize] [, ioffset]
kres randi kamp, kcps [, iseed] [, isize] [, ioffset]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvscent" '(:template "kcent pvscent fsig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inrg" '(:template "inrg kstart, ain1 [,ain2, ain3, ..., ainN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dates" '(:template "Sir dates [ itime]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "adsr" '(:template "ares adsr iatt, idec, islev, irel [, idel]
kres adsr iatt, idec, islev, irel [, idel]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "stix" '(:template "ares stix iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "duserrnd" '(:template "aout duserrnd ktableNum
iout duserrnd itableNum
kout duserrnd ktableNum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wgbow" '(:template "ares wgbow kamp, kfreq, kpres, krat, kvibf, kvamp [, ifn] [, iminfreq]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayxw" '(:template "aout vdelayxw ain, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slicearray" '(:template "karray slicearray kinarray, istart, iend" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "else" '(:template "else" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ptrack" '(:template "kcps, kamp ptrack asig, ihopsize[,ipeaks]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dssiactivate" '(:template "dssiactivate ihandle, ktoggle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vdelayxq" '(:template "aout1, aout2, aout3, aout4 vdelayxq ain1, ain2, ain3, ain4, adl, imd, iws [, ist]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lphasor" '(:template "ares lphasor xtrns [, ilps] [, ilpe] [, imode] [, istrt] [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cosseg" '(:template "ares cosseg ia, idur1, ib [, idur2] [, ic] [...]
kres cosseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dct" '(:template "kout[] dct kin[]
iout[] dct iin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtabwa" '(:template "vtabwa andx, ifn, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outq1" '(:template "outq1 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLscrollEnd" '(:template "FLscrollEnd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpslot" '(:template "lpslot islot" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "push" '(:template "push xval1, [xval2, ... , xval31]
push ival1, [ival2, ... , ival31]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outkpb" '(:template "outkpb kchn, kvalue, kmin, kmax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "deltapi" '(:template "ares deltapi xdlt" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLexecButton" '(:template "ihandle FLexecButton quot;commandquot;, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfload" '(:template "ir sfload quot;filenamequot;" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nestedap" '(:template "ares nestedap asig, imode, imaxdel, idel1, igain1 [, idel2] [, igain2] [, idel3] [, igain3] [, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLprintk2" '(:template "FLprintk2 kval, idisp" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bformenc1" '(:template "aw, ax, ay, az bformenc1 asig, kalpha, kbeta
aw, ax, ay, az, ar, as, at, au, av bformenc1 asig, kalpha, kbeta
aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq bformenc1 asig, kalpha, kbeta
aarray[] bformenc1 asig, kalpha, kbeta" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvoc" '(:template "ares pvoc ktimpnt, kfmod, ifilcod [, ispecwp] [, iextractmode] [, ifreqlim] [, igatefn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "log" '(:template "log(x) (no rate restriction)
kout[]log kin[][,ibas]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsadsyn" '(:template "ares pvsadsyn fsrc, inoscs, kfmod [, ibinoffset] [, ibinincr] [, iinit]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opbitshl" '(:template "a lt;lt; b (bitshift left)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsin" '(:template "fsig pvsin kchan[, isize, iolap, iwinsize, iwintype, iformat]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nchnls_i" '(:template "nchnls_i = iarg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strrindex" '(:template "ipos strrindex S1, S2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pindex" '(:template "ivalue pindex ipfieldIndex" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "assign" '(:template "ares = xarg
ires = iarg
kres = karg
ires, ... = iarg, ...
kres, ... = karg, ...
table [ kval] = karg" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "filebit" '(:template "ir filebit ifilcod [, iallowraw]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xscansmap" '(:template "xscansmap kpos, kvel, iscan, kamp, kvamp [, iwhich]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetPosition" '(:template "FLsetPosition ix, iy, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoMidiInConnect" '(:template "JackoMidiInConnect SexternalPortName, ScsoundPortName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chebyshevpoly" '(:template "aout chebyshevpoly ain, k0 [, k1 [, k2 [...]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "floor" '(:template "floor(x) (init-, control-, or audio-rate arg allowed)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dssilist" '(:template "dssilist" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "imagesave" '(:template "imagesave iimagenum, filename" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outx" '(:template "outx asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, asig9, asig10, asig11, asig12, asig13, asig14, asig15, asig16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "int" '(:template "int(x) (init-rate or control-rate; also works at audio rate in Csound5)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "trirand" '(:template "ares trirand krange
ires trirand krange
kres trirand krange" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "alpass" '(:template "ares alpass asig, xrvt, ilpt [, iskip] [, insmps]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLtabs" '(:template "FLtabs iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sndwarp" '(:template "ares [, ac] sndwarp xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, irandw, ioverlap, ifn2, itimemode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maxaccum" '(:template "maxaccum aAccumulator, aInput" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "nrpn" '(:template "nrpn kchan, kparmnum, kparmvalue" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rspline" '(:template "ares rspline xrangeMin, xrangeMax, kcpsMin, kcpsMax
kres rspline krangeMin, krangeMax, kcpsMin, kcpsMax" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKHevyMetl" '(:template "asignal STKHevyMetl ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outs2" '(:template "outs2 asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "zar" '(:template "ares zar kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ptablei" '(:template "ares ptablei andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptablei indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptablei kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "transegb" '(:template "ares transegb ia, itim, itype, ib [, itim2] [, itype] [, ic] ...
kres transegb ia, itim, itype, ib [, itim2] [, itype] [, ic] ..." :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "release" '(:template "kflag release" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bformenc" '(:template "aw, ax, ay, az bformenc asig, kalpha, kbeta, kord0, kord1
aw, ax, ay, az, ar, as, at, au, av bformenc asig, kalpha, kbeta, kord0, kord1 , kord2
aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq bformenc asig, kalpha, kbeta, kord0, kord1, kord2, kord3" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midiprogramchange" '(:template "midiprogramchange xprogram" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "printks" '(:template "printks quot;stringquot;, itime [, kval1] [, kval2] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "subinstrinit" '(:template "subinstrinit instrnum [, p4] [, p5] [...]
subinstrinit quot;insnamequot; [, p4] [, p5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pchbend" '(:template "ibend pchbend [imin] [, imax]
kbend pchbend [imin] [, imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLrun" '(:template "FLrun" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opcode" '(:template "opcode name, outtypes, intypes" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "OSCinit" '(:template "ihandle OSCinit iport" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "grain3" '(:template "ares grain3 kcps, kphs, kfmd, kpmd, kgdur, kdens, imaxovr, kfn, iwfn, kfrpow, kprpow [, iseed] [, imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsftr" '(:template "pvsftr fsrc, ifna [, ifnf]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maxarray" '(:template "kmax [,kindx] maxarray karray" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "maxabsaccum" '(:template "maxabsaccum aAccumulator, aInput" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabmorph" '(:template "kout tabmorph kindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ...,ifnN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tableseg" '(:template "tableseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resonk" '(:template "kres resonk ksig, kcf, kbw [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsbufread" '(:template "fsig pvsbufread ktime, khandle[, ilo, ihi, iclear]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "printf" '(:template "printf_i Sfmt, itrig, [iarg1[, iarg2[, ... ]]]
printf Sfmt, ktrig, [xarg1[, xarg2[, ... ]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "init" '(:template "ares init iarg
ires init iarg
kres init iarg
ares, ... init iarg, ...
ires, ... init iarg, ...
kres, ... init iarg, ...
tab init isize[, ival]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "qnan" '(:template "qnan(x) (no rate restriction)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hilbert" '(:template "ar1, ar2 hilbert asig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstaudio" '(:template "aout1,aout2 vstaudio instance, [ain1, ain2]
aout1,aout2 vstaudiog instance, [ain1, ain2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strfromurl" '(:template "Sdst strfromurl StringURL" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lessequal" '(:template "(a lt;= b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKWhistle" '(:template "asignal STKWhistle ifrequency, iamplitude, [kmod, kv1[, knoise, kv2[, kfipfreq, kv3[, kfipgain, kv4[, kvol, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rfft" '(:template "kout[] rfft kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lpinterp" '(:template "lpinterp islot1, islot2, kmix" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpsmidinn" '(:template "cpsmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "buzz" '(:template "ares buzz xamp, xcps, knh, ifn [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "olabuffer" '(:template "aout olabuffer kin, ioverlap" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "button" '(:template "kres button knum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "serialEnd" '(:template "serialEnd iPort" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octcps" '(:template "octcps (cps) (init- or control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "resony" '(:template "ares resony asig, kbf, kbw, inum, ksep [, isepmode] [, iscl] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fmwurlie" '(:template "ares fmwurlie kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vaget" '(:template "kval vaget kndx, avar" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vsubv" '(:template "vsubv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "schedule" '(:template "schedule insnum, iwhen, idur [, ip4] [, ip5] [...]
schedule quot;insnamequot;, iwhen, idur [, ip4] [, ip5] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "oscbnk" '(:template "ares oscbnk kcps, kamd, kfmd, kpmd, iovrlap, iseed, kl1minf, kl1maxf, kl2minf, kl2maxf, ilfomode, keqminf, keqmaxf, keqminl, keqmaxl, keqminq, keqmaxq, ieqmode, kfn [, il1fn] [, il2fn] [, ieqffn] [, ieqlfn] [, ieqqfn] [, itabl] [, ioutfn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetVal_i" '(:template "FLsetVal_i ivalue, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "specaddm" '(:template "wsig specaddm wsig1, wsig2 [, imul2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dcblock2" '(:template "ares dcblock2 ain [, iorder] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "MixerSetLevel" '(:template "MixerSetLevel isend, ibuss, kgain" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rireturn" '(:template "rireturn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoNoteOut" '(:template "JackoNoteOut ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "timek" '(:template "ires timek
kres timek" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "chuap" '(:template "aI3, aV2, aV1 chuap kL, kR0, kC1, kG, kGa, kGb, kE, kC2, iI3, iV2, iV1, ktime_step" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pconvolve" '(:template "ar1 [, ar2] [, ar3] [, ar4] pconvolve ain, ifilcod [, ipartitionsize, ichannel]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLmouse" '(:template "kx, ky, kb1, kb2, kb3 FLmouse [imode]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLcloseButton" '(:template "ihandle FLcloseButton quot;labelquot;, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fluidProgramSelect" '(:template "fluidProgramSelect ienginenum, ichannelnum, isfnum, ibanknum, ipresetnum" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "getftargs" '(:template "Sdst getftargs iftno, ktrig" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoMidiOutConnect" '(:template "JackoMidiOutConnect ScsoundPortName, SexternalPortName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "array" '(:template "karray[] array ival1, ival2,.....ivaln" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetSnapGroup" '(:template "FLsetSnapGroup igroup" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pol2rect" '(:template "kout[] pol2rect kin[]
kout[] pol2rect kmags[], kphs[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvsgain" '(:template "fsig pvsgain fsigin, kgain" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "strtodk" '(:template "kr strtodk Sstr
kr strtodk kndx" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vaddv" '(:template "vaddv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "scanu" '(:template "scanu init, irate, ifnvel, ifnmass, ifnstif, ifncentr, ifndamp, kmass, kstif, kcentr, kdamp, ileft, iright, kpos, kstrngth, ain, idisp, id" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "spat3dt" '(:template "spat3dt ioutft, iX, iY, iZ, idist, ift, imode, irlen [, iftnocl]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sleighbells" '(:template "ares sleighbells kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "lua_opdef" '(:template "lua_opdef Sname, Sluacode" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hrtfreverb" '(:template "aleft, aright, idel hrtfreverb asrc, ilowrt60, ihighrt60, ifilel, ifiler [,isr, imfp, iorder]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "out" '(:template "out asig1[, asig2,....]
out aarray" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "moogladder" '(:template "asig moogladder ain, kcf, kres[, istor]
asig moogladder ain, acf, kres[, istor]
asig moogladder ain, kcf, ares[, istor]
asig moogladder ain, acf, ares[, istor]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "opa" '(:template "a(x) (control-rate args only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "outletkid" '(:template "outletkid Sname, SinstanceID, ksignal" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "bbcuts" '(:template "a1,a2 bbcuts asource1, asource2, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLsetVal" '(:template "FLsetVal ktrig, kvalue, ihandle" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "mags" '(:template "kout[] mags kin[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "equals" '(:template "(a == b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vecdelay" '(:template "vecdelay ifn, ifnIn, ifnDel, ielements, imaxdel [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "foutir" '(:template "foutir ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "flanger" '(:template "ares flanger asig, adel, kfeedback [, imaxd]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "modmatrix" '(:template "modmatrix iresfn, isrcmodfn, isrcparmfn, imodscale, inum_mod, inum_parm, kupdate" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "reinit" '(:template "reinit label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cell" '(:template "cell ktrig, kreinit, ioutFunc, initStateFunc, iRuleFunc, ielements" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "midinoteonoct" '(:template "midinoteonoct xoct, xvelocity" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "loscil" '(:template "ar1 [,ar2] loscil xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "rnd" '(:template "rnd(x) (init- or control-rate only)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dconv" '(:template "ares dconv asig, isize, ifn" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "wiisend" '(:template "kres wiisend kcontrol, kvalue[, knum]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "centroid" '(:template "kcent centroid asig, ktrig, ifftsize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "STKBeeThree" '(:template "asignal STKBeeThree ifrequency, iamplitude, [kop4, kv1[, kop3, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoOn" '(:template "JackoOn [iactive]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "hsboscil" '(:template "ares hsboscil kamp, ktone, kbrite, ibasfreq, iwfn, ioctfn [, ioctcnt] [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xout" '(:template "xout xoutarg1 [, xoutarg2] ... [, xoutargN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "JackoInit" '(:template "JackoInit ServerName, SclientName" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ptable" '(:template "ares ptable andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptable indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptable kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cudasliding" '(:template "asig cudasliding ain, amod, iwinsize" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLknob" '(:template "kout, ihandle FLknob quot;labelquot;, imin, imax, iexp, itype, idisp, iwidth, ix, iy [, icursorsize]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "adsynt2" '(:template "ar adsynt2 kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "madsr" '(:template "ares madsr iatt, idec, islev, irel [, idel] [, ireltim]
kres madsr iatt, idec, islev, irel [, idel] [, ireltim]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tabmorphi" '(:template "kout tabmorphi kindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ..., ifnN]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "inletv" '(:template "array inletv Sname" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pvbufread" '(:template "pvbufread ktimpnt, ifile" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ctrl14" '(:template "idest ctrl14 ichan, ictlno1, ictlno2, imin, imax [, ifn]
kdest ctrl14 ichan, ictlno1, ictlno2, kmin, kmax [, ifn]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "crossfm" '(:template "a1, a2 crossfm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crosspm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crosspmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmpm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmpmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "pareq" '(:template "ares pareq asig, kc, kv, kq [, imode] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "reverb" '(:template "ares reverb asig, krvt [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "s16b14" '(:template "i1,...,i16 s16b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb16, ictlno_lsb16, imin16, imax16, initvalue16, ifn16
k1,...,k16 s16b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb16, ictlno_lsb16, imin16, imax16, initvalue16, ifn16" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vstmidiout" '(:template "vstmidiout instance, kstatus, kchan, kdata1, kdata2" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLvalue" '(:template "ihandle FLvalue quot;labelquot;, iwidth, iheight, ix, iy" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "octmidi" '(:template "ioct octmidi" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "flashtxt" '(:template "flashtxt iwhich, String" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "greaterthan" '(:template "(a gt; b ? v1 : v2)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ins" '(:template "ar1, ar2 ins" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "dispfft" '(:template "dispfft xsig, iprd, iwsiz [, iwtyp] [, idbout] [, iwtflg] [,imin] [,imax]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "sfplay3" '(:template "ar1, ar2 sfplay3 ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ceil" '(:template "ceil(x) (init-, control-, or audio-rate arg allowed)" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "balance" '(:template "ares balance asig, acomp [, ihp] [, iskip]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLtabsEnd" '(:template "FLtabsEnd" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "cpuprc" '(:template "cpuprc insnum, ipercent
cpuprc Sinsname, ipercent" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fout" '(:template "fout ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
fout ifilename, iformat, array[]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "slider8tablef" '(:template "kflag slider8tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum8, imin8, imax8, init8, ifn8, icutoff8" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vco2init" '(:template "ifn vco2init iwave [, ibasfn] [, ipmul] [, iminsiz] [, imaxsiz] [, isrcft]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "fini" '(:template "fini ifilename, iskipframes, iformat, in1 [, in2] [, in3] [, ...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "ckgoto" '(:template "ckgoto condition, label" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "xadsr" '(:template "ares xadsr iatt, idec, islev, irel [, idel]
kres xadsr iatt, idec, islev, irel [, idel]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "vtablea" '(:template "vtablea andx, kfn, kinterp, ixmode, aout1 [, aout2, aout3, .... , aoutN ]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "tablexseg" '(:template "tablexseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "doc" :html "seinna") csdoc-opcode-database)
(puthash "FLloadsnap" '(:template "FLloadsnap quot;filenamequot; [, igroup]" :doc "doc" :html "seinna") csdoc-opcode-database)

(provide 'csound-opcodes)
