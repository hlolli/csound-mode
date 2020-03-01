;;; csound-opcodes.el --- A major mode for interacting and coding Csound
;;  Copyright (C) 2017  Hlöðver Sigurðsson

;; Author: Hlöðver Sigurðsson <hlolli@gmail.com>
;; Version: 0.2.2
;; Package-Requires: ((emacs "25") (shut-up "0.3.2") (multi "2.0.1") (dash "2.16.0") (highlight "0"))
;; URL: https://github.com/hlolli/csound-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Auto generated database of opcodes extraced from the manual
;;; Code:

(setq csdoc-opcode-database (make-hash-table :test 'equal))
(puthash "FLslidBnkSet" '(:template "FLslidBnkSet ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "modify the values of a slider bank.") csdoc-opcode-database)
(puthash "slicearray" '(:template "karray slicearray kinarray, istart, iend [,istride]" :doc "Take a slice of a vector.") csdoc-opcode-database)
(puthash "vexpv_i" '(:template "vexpv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs exponential operations between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "gauss" '(:template "ares gauss krange
ires gauss krange
kres gauss krange" :doc "WTF: ") csdoc-opcode-database)
(puthash "tabmorpha" '(:template "aout tabmorpha aindex, aweightpoint, atabnum1, atabnum2, ifn1, ifn2 [, ifn3, ifn4, ... ifnN]" :doc "Allow morphing between a set of tables at audio rate with interpolation.") csdoc-opcode-database)
(puthash "k35_hpf" '(:template "asig K35_hpf ain, xcf, xQ [, inlp, isaturation, istor]" :doc "Zero-delay feedback implementation of Korg35 resonant high-pass filter.") csdoc-opcode-database)
(puthash "JackoMidiOut" '(:template "JackoMidiOut ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]" :doc "Sends a MIDI channel message to a Jack port.") csdoc-opcode-database)
(puthash "gain" '(:template "ares gain asig, krms [, ihp] [, iskip]" :doc "Adjusts the amplitude audio signal according to a root-mean-square value.") csdoc-opcode-database)
(puthash "octcps" '(:template "octcps (cps) (init- or control-rate args only)" :doc "Converts a cycles-per-second value to octave-point-decimal.") csdoc-opcode-database)
(puthash "midic21" '(:template "idest midic21 ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
kdest midic21 ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]" :doc "Allows a floating-point 21-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "chebyshevpoly" '(:template "aout chebyshevpoly ain, k0 [, k1 [, k2 [...]]]" :doc "Efficiently evaluates the sum of Chebyshev polynomials of arbitrary order.") csdoc-opcode-database)
(puthash "dumpk" '(:template "dumpk ksig, ifilname, iformat, iprd" :doc "Periodically writes an orchestra control-signal value to an external file.") csdoc-opcode-database)
(puthash "vdelay" '(:template "ares vdelay asig, adel, imaxdel [, iskip]" :doc "An interpolating variable time delay.") csdoc-opcode-database)
(puthash "phaser2" '(:template "ares phaser2 asig, kfreq, kq, kord, kmode, ksep, kfeedback" :doc "Second-order allpass filters arranged in a series.") csdoc-opcode-database)
(puthash "lphasor" '(:template "ares lphasor xtrns [, ilps] [, ilpe] [, imode] [, istrt] [, istor]" :doc "Generates a table index for sample playback") csdoc-opcode-database)
(puthash "strtolk" '(:template "kr strtolk Sstr
kr strtolk kndx" :doc "Converts a string to a signed integer (k-rate).") csdoc-opcode-database)
(puthash "pchmidinn" '(:template "pchmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "Converts a Midi note number value to octave point pitch-class units.") csdoc-opcode-database)
(puthash "shiftout" '(:template "asig shiftout kIn[][, ioff]" :doc "Shifts the contents of a 1-dimensional array into an audio variable.") csdoc-opcode-database)
(puthash "dam" '(:template "ares dam asig, kthreshold, icomp1, icomp2, irtime, iftime" :doc "A dynamic compressor/expander.") csdoc-opcode-database)
(puthash "butbp" '(:template "ares butbp asig, kfreq, kband [, iskip]" :doc "Same as the butterbp opcode.") csdoc-opcode-database)
(puthash "STKSimple" '(:template "asignal STKSimple ifrequency, iamplitude, [kpos, kv1[, kcross, kv2[, kenv, kv3[, kgain, kv4]]]]" :doc "STKSimple is a wavetable/noise instrument.") csdoc-opcode-database)
(puthash "moogladder2" '(:template "asig moogladder2 ain, kcf, kres[, istor]
asig moogladder2 ain, acf, kres[, istor]
asig moogladder2 ain, kcf, ares[, istor]
asig moogladder2 ain, acf, ares[, istor]" :doc "Moog ladder lowpass filter.") csdoc-opcode-database)
(puthash "tabrec" '(:template "tabrec ktrig_start, ktrig_stop, knumtics, kfn, kin1 [,kin2,...,kinN]" :doc "Recording of control signals.") csdoc-opcode-database)
(puthash "expsegr" '(:template "ares expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres expsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "Trace a series of exponential segments between specified points including a release segment.") csdoc-opcode-database)
(puthash "hvs2" '(:template "hvs2 kx, ky, inumParms, inumPointsX, inumPointsY, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "Allows two-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.") csdoc-opcode-database)
(puthash "fmin" '(:template "ires[] fmin iarg1[], iarg2[]
kres[] fmin karg1[], karg2[]
ires[] fmin iarg1[], iarg2
kres[] fmin karg[], karg2" :doc "Minimum value function.") csdoc-opcode-database)
(puthash "system" '(:template "ires system_i itrig, Scmd, [inowait]
kres system ktrig, Scmd, [knowait]" :doc "WTF: ") csdoc-opcode-database)
(puthash "cigoto" '(:template "cigoto condition, label" :doc "Conditionally transfer control during the i-time pass.") csdoc-opcode-database)
(puthash "maparray" '(:template "karray maparray kinarray, String
karray maparray_i kinarray, String" :doc "Apply a function to every element of a vector.") csdoc-opcode-database)
(puthash "insglobal" '(:template "insglobal isource, instrnum [,instrnum...]" :doc "An opcode which can be used to implement a remote
      orchestra. This opcode will send note events from a source
      machine to many destinations.") csdoc-opcode-database)
(puthash "pinkish" '(:template "ares pinkish xin [, imethod] [, inumbands] [, iseed] [, iskip]" :doc "Generates approximate pink noise.") csdoc-opcode-database)
(puthash "dssiactivate" '(:template "dssiactivate ihandle, ktoggle" :doc "Activates or deactivates a DSSI or LADSPA plugin.") csdoc-opcode-database)
(puthash "hrtfreverb" '(:template "aleft, aright, idel hrtfreverb asrc, ilowrt60, ihighrt60, ifilel, ifiler [,isr, imfp, iorder]" :doc "A binaural, dynamic FDN based diffuse-field reverberator. The opcode works independently as an efficient, flexible reverberator.") csdoc-opcode-database)
(puthash "in32" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16, ar17, ar18, ar19, ar20, ar21, ar22, ar23, ar24, ar25, ar26, ar27, ar28, ar29, ar30, ar31, ar32 in32" :doc "Reads a 32-channel audio signal from an external device or stream.") csdoc-opcode-database)
(puthash "oscilikt" '(:template "ares oscilikt xamp, xcps, kfn [, iphs] [, istor]
kres oscilikt kamp, kcps, kfn [, iphs] [, istor]" :doc "A linearly interpolated oscillator that allows changing the table number at k-rate.") csdoc-opcode-database)
(puthash "pvsfreeze" '(:template "fsig pvsfreeze fsigin, kfreeza, kfreezf" :doc "Freeze the amplitude and frequency time functions of a pv stream according to a control-rate
      trigger.") csdoc-opcode-database)
(puthash "fout" '(:template "fout ifilename, iformat, aout1 [, aout2, aout3,...,aoutN]
fout ifilename, iformat, array[]" :doc "Outputs a-rate signals to an arbitrary number of channels.") csdoc-opcode-database)
(puthash "pvsbufread" '(:template "fsig pvsbufread ktime, khandle[, ilo, ihi, iclear]" :doc "This opcode reads a circular buffer of f-signals (streaming PV signals).") csdoc-opcode-database)
(puthash "cauchyi" '(:template "ares cauchyi klambda, xamp, xcps
ires cauchyi klambda, xamp, xcps
kres cauchyi klambda, xamp, xcps" :doc "WTF: ") csdoc-opcode-database)
(puthash "sfpassign" '(:template "sfpassign istartindex, ifilhandle[, imsgs]" :doc "Assigns all presets of a SoundFont2 (SF2) sample file to a sequence of progressive index numbers.") csdoc-opcode-database)
(puthash "nlfilt" '(:template "ares nlfilt ain, ka, kb, kd, kC, kL" :doc "A filter with a non-linear effect.") csdoc-opcode-database)
(puthash "trandom" '(:template "kout trandom ktrig, kmin, kmax" :doc "Generates a controlled pseudo-random number series between min and max values according to a trigger.") csdoc-opcode-database)
(puthash "vpow_i" '(:template "vpow_i ifn, ival, ielements [, idstoffset]" :doc "Raises each element of a vector to a scalar power") csdoc-opcode-database)
(puthash "syncphasor" '(:template "aphase, asyncout syncphasor xcps, asyncin, [, iphs]" :doc "Produces a normalized moving phase value with sync input and output.") csdoc-opcode-database)
(puthash "midion2" '(:template "midion2 kchn, knum, kvel, ktrig" :doc "Sends noteon and noteoff messages to the MIDI OUT port.") csdoc-opcode-database)
(puthash "specdisp" '(:template "specdisp wsig, iprd [, iwtflg]" :doc "Displays the magnitude values of the spectrum.") csdoc-opcode-database)
(puthash "spdist" '(:template "k1 spdist ifn, ktime, kx, ky" :doc "Calculates distance values from xy coordinates.") csdoc-opcode-database)
(puthash "butlp" '(:template "ares butlp asig, kfreq [, iskip]
ares butlp asig, afreq [, iskip]" :doc "Same as the butterlp opcode.") csdoc-opcode-database)
(puthash "fareylen" '(:template "kfl fareylen kfn" :doc "returns the length of a Farey Sequence.") csdoc-opcode-database)
(puthash "opk" '(:template "k(x) (i-rate args only)
k(x) (a-rate args only)" :doc "Converts a i-rate parameter to an k-rate value.
      Or converts an a-rate value to a k-rate value by down-sampling.") csdoc-opcode-database)
(puthash "gainslider" '(:template "kout gainslider kindex" :doc "An implementation of a logarithmic gain curve which is similar to the gainslider~ object from Cycling 74 Max / MSP.") csdoc-opcode-database)
(puthash "deinterleave" '(:template "kout1[], kout2[] deinterleave kin[]" :doc "Deinterleaves arrays by picking alternate data from its input.") csdoc-opcode-database)
(puthash "ftconv" '(:template "a1[, a2[, a3[, ... a8]]] ftconv ain, ift, iplen[, iskipsamples [, iirlen[, iskipinit]]]" :doc "Low latency multichannel convolution, using a function table as impulse
	response source.") csdoc-opcode-database)
(puthash "grain" '(:template "ares grain xamp, xpitch, xdens, kampoff, kpitchoff, kgdur, igfn, iwfn, imgdur [, igrnd]" :doc "Generates granular synthesis textures.") csdoc-opcode-database)
(puthash "dust" '(:template "ares dust kamp, kdensity
kres dust kamp, kdensity" :doc "Random impulses.") csdoc-opcode-database)
(puthash "zdf_1pole" '(:template "asig zdf_1pole ain, xcf [, kmode, istor]" :doc "Zero-delay feedback implementation of 1 pole filter.") csdoc-opcode-database)
(puthash "tablefilteri" '(:template "inumpassed tablefilteri iouttable, iintatble, imode, iparam" :doc "Filters a source table and writes result into a destination table.") csdoc-opcode-database)
(puthash "ATScross" '(:template "ar ATScross ktimepnt, kfmod, iatsfile, ifn, kmylev, kbuflev, ipartials [, ipartialoffset, ipartialincr]" :doc "perform cross synthesis from ATS analysis files.") csdoc-opcode-database)
(puthash "adsyn" '(:template "ares adsyn kamod, kfmod, ksmod, ifilcod" :doc "Output is an additive set of individually controlled sinusoids, using an oscillator bank.") csdoc-opcode-database)
(puthash "tableicopy" '(:template "tableicopy idft, isft" :doc "Simple, fast table copy opcode.") csdoc-opcode-database)
(puthash "lpfreson" '(:template "ares lpfreson asig, kfrqratio" :doc "Resynthesises a signal from the data passed internally by a previous lpread, applying formant shifting.") csdoc-opcode-database)
(puthash "alwayson" '(:template "alwayson Tinstrument [p4, ..., pn]" :doc "Activates the indicated instrument in the orchestra header,
      without need for an i statement.") csdoc-opcode-database)
(puthash "resonr" '(:template "ares resonr asig, xcf, xbw [, iscl] [, iskip]" :doc "A bandpass filter with variable frequency response.") csdoc-opcode-database)
(puthash "valpass" '(:template "ares valpass asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]" :doc "Variably reverberates an input signal with a flat frequency response.") csdoc-opcode-database)
(puthash "0dbfs" '(:template "0dbfs = iarg
0dbfs" :doc "Sets the value of 0 decibels using full scale amplitude.") csdoc-opcode-database)
(puthash "strchar" '(:template "ichr strchar Sstr[, ipos]" :doc "Return the ASCII code of a character in a string") csdoc-opcode-database)
(puthash "A4" '(:template "A4 = iarg" :doc "Sets the base frequency for pitch A4.") csdoc-opcode-database)
(puthash "wgbrass" '(:template "ares wgbrass kamp, kfreq, ktens, iatt, kvibf, kvamp [, ifn] [, iminfreq]" :doc "Creates a tone related to a brass instrument.") csdoc-opcode-database)
(puthash "FLsetPosition" '(:template "FLsetPosition ix, iy, ihandle" :doc "Sets the position of a FLTK widget.") csdoc-opcode-database)
(puthash "tablemix" '(:template "tablemix kdft, kdoff, klen, ks1ft, ks1off, ks1g, ks2ft, ks2off, ks2g" :doc "Mixes two tables.") csdoc-opcode-database)
(puthash "pvscale" '(:template "fsig pvscale fsigin, kscal[, kkeepform, kgain, kcoefs]" :doc "Scale the frequency components of a pv stream.") csdoc-opcode-database)
(puthash "pol2rect" '(:template "kout[] pol2rect kin[]
kout[] pol2rect kmags[], kphs[]" :doc "Polar to rectangular format conversion.") csdoc-opcode-database)
(puthash "strget" '(:template "Sdst strget indx" :doc "Set string variable to value from strset table or string p-field") csdoc-opcode-database)
(puthash "mvclpf1" '(:template "asig mvclpf1 ain, xcf, xres[,istor]" :doc "Moog voltage-controlled lowpass filter emulation.") csdoc-opcode-database)
(puthash "binit" '(:template "fsig binit fin, isize" :doc "PVS tracks to amplitude+frequency conversion.") csdoc-opcode-database)
(puthash "xin" '(:template "xinarg1 [, xinarg2] ... [xinargN] xin" :doc "Passes variables to a user-defined opcode block,") csdoc-opcode-database)
(puthash "turnon" '(:template "turnon insnum [, itime]" :doc "Activate an instrument for an indefinite time.") csdoc-opcode-database)
(puthash "modmatrix" '(:template "modmatrix iresfn, isrcmodfn, isrcparmfn, imodscale, inum_mod, inum_parm, kupdate" :doc "Modulation matrix opcode with optimizations for sparse matrices.") csdoc-opcode-database)
(puthash "trmix" '(:template "fsig trmix fin1, fin2" :doc "Streaming partial track mixing.") csdoc-opcode-database)
(puthash "OSCinitM" '(:template "ihandle OSCinitM Sgroup, iport" :doc "Start a listening process for multicast OSC messages to a particular port.") csdoc-opcode-database)
(puthash "timek" '(:template "ires timek
kres timek" :doc "Read absolute time in k-rate cycles.") csdoc-opcode-database)
(puthash "sleighbells" '(:template "ares sleighbells kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "Semi-physical model of a sleighbell sound.") csdoc-opcode-database)
(puthash "envlpxr" '(:template "ares envlpxr xamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]
kres envlpxr kamp, irise, idec, ifn, iatss, iatdec [, ixmod] [,irind]" :doc "The") csdoc-opcode-database)
(puthash "cmplxprod" '(:template "kout[] cmplxprod kin1[], kin2[]" :doc "Complex product of two arrays.") csdoc-opcode-database)
(puthash "diskin2" '(:template "a1[, a2[, ... aN]] diskin2 ifilcod[, kpitch[, iskiptim [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]
ar1[] diskin2 ifilcod[, kpitch[, iskiptim [, iwrap[, iformat[, iwsize[, ibufsize[, iskipinit]]]]]]]" :doc "Reads audio data from a file, and can alter its pitch using one of several
      available interpolation types, as well as convert the sample rate to match
      the orchestra sr setting.") csdoc-opcode-database)
(puthash "tabrowlin" '(:template "tabrowlin krow, ifnsrc, ifndest, inumcols [, ioffset, istart, iend, istep ]" :doc "Copy a row from an f-table to another, interpolating between rows") csdoc-opcode-database)
(puthash "maxalloc" '(:template "maxalloc insnum, icount
maxalloc Sinsname, icount" :doc "Limits the number of allocations of an instrument.") csdoc-opcode-database)
(puthash "vincr" '(:template "vincr accum, aincr" :doc "Accumulates audio signals.") csdoc-opcode-database)
(puthash "vco2ft" '(:template "kfn vco2ft kcps, iwave [, inyx]" :doc "Returns a table number at k-time for a given oscillator frequency and wavform.") csdoc-opcode-database)
(puthash "JackoFreewheel" '(:template "JackoFreewheel [ienabled]" :doc "Turns Jack's freewheeling mode on or off.") csdoc-opcode-database)
(puthash "date" '(:template "ir[, inano] date
kr[, knano] date" :doc "Returns the number seconds since a base date.") csdoc-opcode-database)
(puthash "pvstanal" '(:template "fsig pvstanal ktimescal, kamp, kpitch, ktab, [kdetect, kwrap, ioffset,ifftsize, ihop, idbthresh]" :doc "Phase vocoder analysis processing with onset detection/processing.") csdoc-opcode-database)
(puthash "getrowlin" '(:template "kOut[] getrowlin kMtx[], krow [, kstart, kend, kstep ]
kOut[] getrowlin krow, ifn, inumcols [, iskip, start, iend, istep ]" :doc "Copy a row from a 2D array or table, with interpolation between rows") csdoc-opcode-database)
(puthash "logcurve" '(:template "kout logcurve kindex, ksteepness" :doc "This opcode implements a formula for generating a normalised logarithmic curve in range 0 - 1. It is based on the Max / MSP work of Eric Singer (c) 1994.") csdoc-opcode-database)
(puthash "pvsfwrite" '(:template "pvsfwrite fsig, ifile" :doc "Write a fsig to a PVOCEX file.") csdoc-opcode-database)
(puthash "max_k" '(:template "knumkout max_k asig, ktrig, itype" :doc "Local maximum (or minimum) value of an incoming asig signal") csdoc-opcode-database)
(puthash "sorta" '(:template "k/i[]sorta k/i[] (k- or i-arrays )" :doc "Sorts an array in ascending order.") csdoc-opcode-database)
(puthash "outletv" '(:template "outletv Sname, array" :doc "Sends an arate array signal out from an instrument to a named port.") csdoc-opcode-database)
(puthash "zaw" '(:template "zaw asig, kndx" :doc "Writes to a za variable at a-rate without mixing.") csdoc-opcode-database)
(puthash "setctrl" '(:template "setctrl inum, ival, itype" :doc "Configurable slider controls for realtime user input.") csdoc-opcode-database)
(puthash "scantable" '(:template "aout scantable kamp, kpch, ipos, imass, istiff, idamp, ivel" :doc "A simpler scanned synthesis implementation.") csdoc-opcode-database)
(puthash "pvspitch" '(:template "kfr, kamp pvspitch fsig, kthresh" :doc "Track the pitch and amplitude of a PVS signal.") csdoc-opcode-database)
(puthash "vcomb" '(:template "ares vcomb asig, krvt, xlpt, imaxlpt [, iskip] [, insmps]" :doc "Variably reverberates an input signal with a") csdoc-opcode-database)
(puthash "bexprnd" '(:template "ares bexprnd krange
ires bexprnd krange
kres bexprnd krange" :doc "WTF: ") csdoc-opcode-database)
(puthash "sndwarpst" '(:template "ar1, ar2 [,ac1] [, ac2] sndwarpst xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, irandw, ioverlap, ifn2, itimemode" :doc "Reads a stereo sound sample from a table and applies time-stretching and/or pitch modification.") csdoc-opcode-database)
(puthash "ihold" '(:template "ihold" :doc "Creates a held note.") csdoc-opcode-database)
(puthash "outq1" '(:template "outq1 asig" :doc "Writes samples to quad channel 1 of an external device or stream.") csdoc-opcode-database)
(puthash "cossegb" '(:template "ares cossegb ia, itim1, ib [, itim2] [, ic] [...]
kres cossegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "Trace a series of line segments between specified absolute points with
      cosine interpolation.") csdoc-opcode-database)
(puthash "link_beat_get" '(:template "k_beat_number, k_phase, k_current_time_seconds link_beat_get i_peer [, k_quantum]" :doc "Returns the beat, phase with respect to the local quantum, and current time for the session.") csdoc-opcode-database)
(puthash "nchnls" '(:template "nchnls = iarg" :doc "Sets the number of channels of audio output.") csdoc-opcode-database)
(puthash "follow" '(:template "ares follow asig, idt" :doc "Envelope follower unit generator.") csdoc-opcode-database)
(puthash "lpsholdp" '(:template "ksig lpsholdp kphase, kvalue0, ktime0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]" :doc "Control signals based on held segments.") csdoc-opcode-database)
(puthash "flooper" '(:template "asig1[, asig2] flooper kamp, kpitch, istart, idur, ifad, ifn" :doc "Function-table-based crossfading looper.") csdoc-opcode-database)
(puthash "strcatk" '(:template "Sdst strcatk Ssrc1, Ssrc2" :doc "Concatenate strings (k-rate)") csdoc-opcode-database)
(puthash "spat3d" '(:template "aW, aX, aY, aZ spat3d ain, kX, kY, kZ, idist, ift, imode, imdel, iovr [, istor]" :doc "Positions the input sound in a 3D space and allows moving the sound at k-rate.") csdoc-opcode-database)
(puthash "vmap" '(:template "vmap ifn1, ifn2, ielements [,idstoffset, isrcoffset]" :doc "Maps elements from a vector according to indexes contained in another vector.") csdoc-opcode-database)
(puthash "dssictls" '(:template "dssictls ihandle, iport, kvalue, ktrigger" :doc "Send control information to a LADSPA or DSSI plugin.") csdoc-opcode-database)
(puthash "init" '(:template "ares init iarg
ires init iarg
kres init iarg
ares, ... init iarg, ...
ires, ... init iarg, ...
kres, ... init iarg, ...
tab init isize[, ival]" :doc "Puts the value of the i-time expression into a k-, a-rate or t- variable.") csdoc-opcode-database)
(puthash "liveconv" '(:template "ares liveconv ain, ift, iplen, kupdate, kclear" :doc "Partitioned convolution with dynamically reloadable impulse response") csdoc-opcode-database)
(puthash "hvs3" '(:template "hvs3 kx, ky, kz, inumParms, inumPointsX, inumPointsY, inumPointsZ, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "Allows three-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.") csdoc-opcode-database)
(puthash "tablekt" '(:template "ares tablekt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
kres tablekt kndx, kfn [, ixmode] [, ixoff] [, iwrap]" :doc "Provides k-rate control over table numbers.") csdoc-opcode-database)
(puthash "fluidOut" '(:template "aleft, aright fluidOut ienginenum" :doc "Outputs sound from a given fluidEngine") csdoc-opcode-database)
(puthash "tabifd" '(:template "ffr,fphs tabifd ktimpt, kamp, kpitch, ifftsize, ihopsize, iwintype,ifn" :doc "Instantaneous Frequency Distribution, magnitude and phase analysis.") csdoc-opcode-database)
(puthash "cpuprc" '(:template "cpuprc insnum, ipercent
cpuprc Sinsname, ipercent" :doc "Control allocation of cpu resources on a per-instrument basis, to optimize realtime output.") csdoc-opcode-database)
(puthash "atone" '(:template "ares atone asig, khp [, iskip]" :doc "A hi-pass filter whose transfer functions are the complements of the") csdoc-opcode-database)
(puthash "return" '(:template "return ival" :doc "Returns a value from an instrument.") csdoc-opcode-database)
(puthash "lpf18" '(:template "ares lpf18 asig, xfco, xres, xdist [, iskip]" :doc "A 3-pole sweepable resonant lowpass filter.") csdoc-opcode-database)
(puthash "miditempo" '(:template "ksig miditempo" :doc "Returns the current tempo at k-rate, of either the MIDI file (if
      available) or the score.") csdoc-opcode-database)
(puthash "maxarray" '(:template "kmax [,kindx] maxarray karray" :doc "returns the maximum value in an array.") csdoc-opcode-database)
(puthash "kr" '(:template "kr = iarg" :doc "Sets the control rate.") csdoc-opcode-database)
(puthash "oscil1i" '(:template "kres oscil1i idel, kamp, idur [, ifn]" :doc "Accesses table values by incremental sampling with linear interpolation.") csdoc-opcode-database)
(puthash "pvsbuffer" '(:template "ihandle, ktime pvsbuffer fsig, ilen" :doc "This opcode creates and writes to a circular buffer for f-signals (streaming PV signals).") csdoc-opcode-database)
(puthash "limit1" '(:template "ires[] limit1 iarg
kres[] limit1 karg" :doc "Limiting function") csdoc-opcode-database)
(puthash "wiisend" '(:template "kres wiisend kcontrol, kvalue[, knum]" :doc "Sends data to one of a number of external Nintendo Wiimote controllers.") csdoc-opcode-database)
(puthash "bamboo" '(:template "ares bamboo kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "Semi-physical model of a bamboo sound.") csdoc-opcode-database)
(puthash "tonek" '(:template "kres tonek ksig, khp [, iskip]" :doc "A first-order recursive low-pass filter with variable frequency response.") csdoc-opcode-database)
(puthash "jitter" '(:template "kout jitter kamp, kcpsMin, kcpsMax" :doc "Generates a segmented line whose segments are randomly generated.") csdoc-opcode-database)
(puthash "foscili" '(:template "ares foscili xamp, kcps, xcar, xmod, kndx, ifn [, iphs]" :doc "Basic frequency modulated oscillator with linear interpolation.") csdoc-opcode-database)
(puthash "soundouts" '(:template "soundouts asigl, asigr, ifilcod [, iformat]" :doc "WTF: ") csdoc-opcode-database)
(puthash "ATSbufread" '(:template "ATSbufread ktimepnt, kfmod, iatsfile, ipartials[, ipartialoffset, ipartialincr]" :doc "reads data from and ATS data file and stores it in an internal data table of frequency, amplitude pairs.") csdoc-opcode-database)
(puthash "lincos" '(:template "ky lincos kx, ky0, ky1 [, kx0, kx1 ]
iy lincos ix, iy0, iy1 [, ix0, ix1 ]" :doc "Linear to cosine interpolation") csdoc-opcode-database)
(puthash "barmodel" '(:template "ares barmodel kbcL, kbcR, iK, ib, kscan, iT30, ipos, ivel, iwid" :doc "Creates a tone similar to a struck metal bar.") csdoc-opcode-database)
(puthash "pvsgain" '(:template "fsig pvsgain fsigin, kgain" :doc "Scale the amplitude of a pv stream.") csdoc-opcode-database)
(puthash "tab2pvs" '(:template "fsig tab2pvs tvar|karr[][,ihopsize, iwinsize, iwintype]
fsig tab2pvs kmags[], kfreqs[][,ihopsize, iwinsize, iwintype]" :doc "Copies spectral data from k-rate arrays (or t-variables.). Also known as pvsfromarray.") csdoc-opcode-database)
(puthash "STKBrass" '(:template "asignal STKBrass ifrequency, iamplitude, [klip, kv1[, kslide, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "STKBrass is a simple brass instrument.") csdoc-opcode-database)
(puthash "FLsetVal_i" '(:template "FLsetVal_i ivalue, ihandle" :doc "Sets the value of a FLTK valuator to a number provided by the user.") csdoc-opcode-database)
(puthash "newopcodename" '(:template "outarg1, outarg2 newopcodename inarg1, inarg2" :doc "Short description. Single line for opcode listing.") csdoc-opcode-database)
(puthash "OSCbundle" '(:template "OSCbundle kwhen, ihost, iport, Sdest[], Stype[],kArgs[][][,isize]" :doc "Sends data to other processes using the OSC protocol by packing
      messages in a bundle.") csdoc-opcode-database)
(puthash "mvclpf4" '(:template "asig1,asig2,asig3,asig4 mvclpf4 ain, xcf, xres[, istor]" :doc "Moog voltage-controlled lowpass filter emulation.") csdoc-opcode-database)
(puthash "tablewkt" '(:template "tablewkt asig, andx, kfn [, ixmode] [, ixoff] [, iwgmode]
tablewkt ksig, kndx, kfn [, ixmode] [, ixoff] [, iwgmode]" :doc "Change the contents of existing function tables.") csdoc-opcode-database)
(puthash "bqrez" '(:template "ares bqrez asig, xfco, xres [, imode] [, iskip]" :doc "A second-order multi-mode filter.") csdoc-opcode-database)
(puthash "sandpaper" '(:template "ares sandpaper iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "Semi-physical model of a sandpaper sound.") csdoc-opcode-database)
(puthash "zfilter2" '(:template "ares zfilter2 asig, kdamp, kfreq, iM, iN, ib0, ib1, ..., ibM, ia1,ia2, ..., iaN" :doc "Performs filtering using a transposed form-II digital filter lattice with radial pole-shearing and angular pole-warping.") csdoc-opcode-database)
(puthash "opi" '(:template "i(x) (control-rate or init-rate arg)
i(karray,index1, ...) (k-array with indices)" :doc "Returns an init-type equivalent of a k-rate argument, or directly returns an i-rate argument.") csdoc-opcode-database)
(puthash "FLsetFont" '(:template "FLsetFont ifont, ihandle" :doc "Sets the font type of a FLTK widget.") csdoc-opcode-database)
(puthash "scale" '(:template "kscl scale kinput, kmax, kmin" :doc "Arbitrary signal scaling.") csdoc-opcode-database)
(puthash "outrg" '(:template "outrg kstart, aout1 [,aout2, aout3, ..., aoutN]" :doc "Allow output to a range of adjacent audio channels on the audio output device") csdoc-opcode-database)
(puthash "resonz" '(:template "ares resonz asig, xcf, xbw [, iscl] [, iskip]" :doc "A bandpass filter with variable frequency response.") csdoc-opcode-database)
(puthash "hypot" '(:template "ires[] hypot iarg1[], iarg2[]
kres[] hypot karg1[], karg2[]" :doc "Euclidean distance function.") csdoc-opcode-database)
(puthash "vosim" '(:template "ar vosim kamp, kFund, kForm, kDecay, kPulseCount, kPulseFactor, ifn [, iskip]" :doc "Simple vocal simulation based on glottal pulses with formant characteristics.") csdoc-opcode-database)
(puthash "nsamp" '(:template "nsamp(x) (init-rate args only)" :doc "Returns the number of samples loaded into a stored function table number.") csdoc-opcode-database)
(puthash "buzz" '(:template "ares buzz xamp, xcps, knh, ifn [, iphs]" :doc "Output is a set of harmonically related sine partials.") csdoc-opcode-database)
(puthash "fin" '(:template "fin ifilename, iskipframes, iformat, ain1 [, ain2] [, ain3] [,...]
fin ifilename, iskipframes, iformat, arr[]" :doc "Read signals from a file at a-rate.") csdoc-opcode-database)
(puthash "fft" '(:template "kout[] fft kin[]" :doc "Complex-to-complex Fast Fourier Transform.") csdoc-opcode-database)
(puthash "readk4" '(:template "kr1, kr2, kr3, kr4 readk4 ifilname, iformat, iprd" :doc "Periodically reads four orchestra control-signal values from an external file.") csdoc-opcode-database)
(puthash "vco" '(:template "ares vco xamp, xcps, iwave, kpw [, ifn] [, imaxd] [, ileak] [, inyx] [, iphs] [, iskip]" :doc "Implementation of a band limited, analog modeled oscillator.") csdoc-opcode-database)
(puthash "FLhvsBox" '(:template "ihandle FLhvsBox inumlinesX, inumlinesY, iwidth, iheight, ix, iy" :doc "Displays a box with a grid useful for visualizing two-dimensional Hyper Vectorial Synthesis.") csdoc-opcode-database)
(puthash "push" '(:template "push xval1, [xval2, ... , xval31]
push ival1, [ival2, ... , ival31]" :doc "WTF: ") csdoc-opcode-database)
(puthash "xadsr" '(:template "ares xadsr iatt, idec, islev, irel [, idel]
kres xadsr iatt, idec, islev, irel [, idel]" :doc "Calculates the classical ADSR envelope.") csdoc-opcode-database)
(puthash "strsub" '(:template "Sdst strsub Ssrc[, istart[, iend]]" :doc "Extract a substring") csdoc-opcode-database)
(puthash "spechist" '(:template "wsig spechist wsigin" :doc "Accumulates the values of successive spectral frames.") csdoc-opcode-database)
(puthash "veloc" '(:template "ival veloc [ilow] [, ihigh]" :doc "Get the velocity from a MIDI event.") csdoc-opcode-database)
(puthash "freeverb" '(:template "aoutL, aoutR freeverb ainL, ainR, kRoomSize, kHFDamp[, iSRate[, iSkip]]" :doc "Opcode version of Jezar's Freeverb") csdoc-opcode-database)
(puthash "gaussi" '(:template "ares gaussi krange, xamp, xcps
ires gaussi krange, xamp, xcps
kres gaussi krange, xamp, xcps" :doc "WTF: ") csdoc-opcode-database)
(puthash "delay" '(:template "ares delay asig, idlt [, iskip]" :doc "Delays an input signal by some time interval.") csdoc-opcode-database)
(puthash "zkwm" '(:template "zkwm ksig, kndx [, imix]" :doc "Writes to a zk variable at k-rate with mixing.") csdoc-opcode-database)
(puthash "FLsetVal" '(:template "FLsetVal ktrig, kvalue, ihandle" :doc "Sets the value of a FLTK valuator at control-rate.") csdoc-opcode-database)
(puthash "ftload" '(:template "ftload Sfilename, iflag, ifn1 [, ifn2] [...]" :doc "Load a set of previously-allocated tables from a file.") csdoc-opcode-database)
(puthash "betarand" '(:template "ares betarand krange, kalpha, kbeta
ires betarand krange, kalpha, kbeta
kres betarand krange, kalpha, kbeta" :doc "WTF: ") csdoc-opcode-database)
(puthash "product" '(:template "ares product asig1, asig2 [, asig3] [...]" :doc "Multiplies any number of a-rate signals.") csdoc-opcode-database)
(puthash "octpch" '(:template "octpch (pch) (init- or control-rate args only)" :doc "Converts a pitch-class value to octave-point-decimal.") csdoc-opcode-database)
(puthash "vtablewi" '(:template "vtablewi indx, ifn, ixmode, inarg1 [, inarg2, inarg3 , .... , inargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "STKFlute" '(:template "asignal STKFlute ifrequency, iamplitude, [kjet, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]" :doc "STKFlute uses a simple flute physical model.") csdoc-opcode-database)
(puthash "clfilt" '(:template "ares clfilt asig, kfreq, itype, inpol [, ikind] [, ipbr] [, isba] [, iskip]" :doc "Implements low-pass and high-pass filters of different styles.") csdoc-opcode-database)
(puthash "outz" '(:template "outz ksig1" :doc "Writes multi-channel audio data from a ZAK array to an external device or stream.") csdoc-opcode-database)
(puthash "buthp" '(:template "ares buthp asig, kfreq [, iskip]
ares buthp asig, afreq [, iskip]" :doc "Same as the butterhp opcode.") csdoc-opcode-database)
(puthash "dssilist" '(:template "dssilist" :doc "Lists all available DSSI and LADSPA plugins.") csdoc-opcode-database)
(puthash "pinker" '(:template "ares pinker" :doc "Generates pink noise.") csdoc-opcode-database)
(puthash "mandel" '(:template "kiter, koutrig mandel ktrig, kx, ky, kmaxIter" :doc "Mandelbrot set") csdoc-opcode-database)
(puthash "FLsetTextType" '(:template "FLsetTextType itype, ihandle" :doc "Sets some font attributes of the text label of a FLTK widget.") csdoc-opcode-database)
(puthash "sfplay3m" '(:template "ares sfplay3m ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "Plays a SoundFont2 (SF2) sample preset, generating a mono sound with cubic interpolation.") csdoc-opcode-database)
(puthash "cross2" '(:template "ares cross2 ain1, ain2, isize, ioverlap, iwin, kbias" :doc "Cross synthesis using FFT's.") csdoc-opcode-database)
(puthash "vmultv" '(:template "vmultv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs mutiplication between two vectorial control signals") csdoc-opcode-database)
(puthash "evalstr" '(:template "ires evalstr Scode
kres evalstr Scode, ktrig" :doc "Evalstrs evaluates a string containing Csound code, returning a value.") csdoc-opcode-database)
(puthash "serialWrite" '(:template "serialWrite iPort, iByte
serialWrite iPort, kByte
serialWrite iPort, SBytes" :doc "Write data to a serial port.") csdoc-opcode-database)
(puthash "JackoAudioIn" '(:template "asignal JackoAudioIn ScsoundPortName" :doc "Receives an audio signal from a Jack port.") csdoc-opcode-database)
(puthash "timeinstk" '(:template "kres timeinstk" :doc "Read absolute time in k-rate cycles.") csdoc-opcode-database)
(puthash "pyrun" '(:template "pyrun "statement"
pyruni "statement"
pylrun "statement"
pylruni "statement"
pyrunt ktrigger, "statement"
pylrunt ktrigger, "statement"" :doc "Run a Python statement or block of statements.") csdoc-opcode-database)
(puthash "butterlp" '(:template "ares butterlp asig, kfreq [, iskip]
ares butterlp asig, afreq [, iskip]" :doc "A low-pass Butterworth filter.") csdoc-opcode-database)
(puthash "productarray" '(:template "kres/iresproduct karr[]/iarr[] (k- or i-arrays )" :doc "Calculates the product of an array.") csdoc-opcode-database)
(puthash "opa" '(:template "a(x) (control-rate args only)" :doc "Converts a k-rate parameter to an a-rate value with interpolation.") csdoc-opcode-database)
(puthash "rect2pol" '(:template "kout[] rect2pol kin[]" :doc "Rectangular to polar format conversion.") csdoc-opcode-database)
(puthash "xscans" '(:template "ares xscans kamp, kfreq, ifntraj, id [, iorder]" :doc "Fast scanned synthesis waveform and the wavetable generator.") csdoc-opcode-database)
(puthash "ptrack" '(:template "kcps, kamp ptrack asig, ihopsize[,ipeaks]" :doc "Tracks the pitch of a signal.") csdoc-opcode-database)
(puthash "svfilter" '(:template "alow, ahigh, aband svfilter asig, kcf, kq [, iscl] [, iskip]" :doc "A resonant second order filter, with simultaneous lowpass, highpass and bandpass outputs.") csdoc-opcode-database)
(puthash "moogvcf" '(:template "ares moogvcf asig, xfco, xres [,iscale, iskip]" :doc "A digital emulation of the Moog diode ladder filter configuration.") csdoc-opcode-database)
(puthash "STKVoicForm" '(:template "asignal STKVoicForm ifrequency, iamplitude, [kmix, kv1[, ksel, kv2[, klfo, kv3[, klfodepth, kv4[, kloud, kv5]]]]]" :doc "STKVoicForm is a  four formant synthesis instrument.") csdoc-opcode-database)
(puthash "rtclock" '(:template "ires rtclock
kres rtclock" :doc "Read the real time clock from the operating system.") csdoc-opcode-database)
(puthash "FLsetSize" '(:template "FLsetSize iwidth, iheight, ihandle" :doc "Resizes a FLTK widget.") csdoc-opcode-database)
(puthash "noteoff" '(:template "noteoff ichn, inum, ivel" :doc "Send a noteoff message to the MIDI OUT port.") csdoc-opcode-database)
(puthash "midglobal" '(:template "midglobal isource, instrnum [,instrnum...]" :doc "An opcode which can be used to implement a remote midi orchestra. This opcode will broadcast the midi events to all the machines involved in the remote concert.") csdoc-opcode-database)
(puthash "pvbufread" '(:template "pvbufread ktimpnt, ifile" :doc "Reads from a phase vocoder analysis file and makes the retrieved data available.") csdoc-opcode-database)
(puthash "FLbutBank" '(:template "kout, ihandle FLbutBank itype, inumx, inumy, iwidth, iheight, ix, iy, iopcode [, kp1] [, kp2] [, kp3] [, kp4] [, kp5] [....] [, kpN]" :doc "A FLTK widget opcode that creates a bank of buttons.") csdoc-opcode-database)
(puthash "fofilter" '(:template "asig fofilter ain, xcf, xris, xdec[, istor]" :doc "Formant filter.") csdoc-opcode-database)
(puthash "pvsdisp" '(:template "pvsdisp fsig[, ibins, iwtflg]" :doc "Displays a PVS signal as an amplitude vs. freq graph.") csdoc-opcode-database)
(puthash "lowres" '(:template "ares lowres asig, xcutoff, xresonance [, iskip]" :doc "Another resonant lowpass filter.") csdoc-opcode-database)
(puthash "cngoto" '(:template "cngoto condition, label" :doc "Transfers control on every pass when a condition is not true.") csdoc-opcode-database)
(puthash "sr" '(:template "sr = iarg" :doc "Sets the audio sampling rate.") csdoc-opcode-database)
(puthash "pyassign" '(:template "pyassign "variable", kvalue
pyassigni "variable", ivalue
pylassign "variable", kvalue
pylassigni "variable", ivalue
pyassignt ktrigger, "variable", kvalue
pylassignt ktrigger, "variable", kvalue" :doc "Assign the value of the given Csound variable to a Python variable possibly destroying its previous content.") csdoc-opcode-database)
(puthash "lorisplay" '(:template "ar lorisplay ireadidx, kfreqenv, kampenv, kbwenv" :doc "renders a stored set of bandwidth-enhanced partials using the method of Bandwidth-Enhanced Additive Synthesis implemented in the Loris software, applying control-rate frequency, amplitude, and bandwidth scaling envelopes.") csdoc-opcode-database)
(puthash "pchmidib" '(:template "ipch pchmidib [irange]
kpch pchmidib [irange]" :doc "Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in pitch-class units.") csdoc-opcode-database)
(puthash "midipolyaftertouch" '(:template "midipolyaftertouch xpolyaftertouch, xcontrollervalue [, ilow] [, ihigh]" :doc "Gets a MIDI polyphonic aftertouch value.") csdoc-opcode-database)
(puthash "FLrun" '(:template "FLrun" :doc "Starts the FLTK widget thread.") csdoc-opcode-database)
(puthash "printf" '(:template "printf_i Sfmt, itrig, [iarg1[, iarg2[, ... ]]]
printf Sfmt, ktrig, [xarg1[, xarg2[, ... ]]]" :doc "WTF: ") csdoc-opcode-database)
(puthash "STKMoog" '(:template "asignal STKMoog ifrequency, iamplitude, [kq, kv1[, krate, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "STKMoog produces moog-like swept filter sounds.") csdoc-opcode-database)
(puthash "vcella" '(:template "vcella ktrig, kreinit, ioutFunc, initStateFunc, iRuleFunc, ielements, irulelen [, iradius]" :doc "Cellular Automata") csdoc-opcode-database)
(puthash "xscansmap" '(:template "xscansmap kpos, kvel, iscan, kamp, kvamp [, iwhich]" :doc "Allows the position and velocity of a node in a scanned process to be read.") csdoc-opcode-database)
(puthash "xyin" '(:template "kx, ky xyin iprd, ixmin, ixmax, iymin, iymax [, ixinit] [, iyinit]" :doc "Sense the cursor position in an output window") csdoc-opcode-database)
(puthash "vaddv_i" '(:template "vaddv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs addition between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "vtable1k" '(:template "vtable1k kfn,kout1 [, kout2, kout3, .... , koutN ]" :doc "Read a vector (several scalars simultaneously) from a table.") csdoc-opcode-database)
(puthash "portk" '(:template "kres portk ksig, khtim [, isig]" :doc "Applies portamento to a step-valued control signal.") csdoc-opcode-database)
(puthash "control" '(:template "kres control knum" :doc "Configurable slider controls for realtime user input.") csdoc-opcode-database)
(puthash "loopseg" '(:template "ksig loopseg kfreq, ktrig, iphase, kvalue0, ktime0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2][...]" :doc "Generate control signal consisting of linear segments delimited by two or more specified points.") csdoc-opcode-database)
(puthash "fillarray" '(:template "karray[] fillarray ival1, ival2,.....ivaln
karray fillarray ival1, ival2,.....ivaln
karray fillarray kval1, kval2,.....kvaln" :doc "Generate a vector with initial values.") csdoc-opcode-database)
(puthash "shiftin" '(:template "kout[] shiftin asig" :doc "Shifts the contents of an audio variable into a 1-dimensional array.") csdoc-opcode-database)
(puthash "pvsdiskin" '(:template "fsig pvsdiskin SFname,ktscal,kgain[,ioffset, ichan]" :doc "Read a selected channel from a PVOC-EX analysis file.") csdoc-opcode-database)
(puthash "butterhp" '(:template "ares butterhp asig, kfreq [, iskip]
ares butterhp asig, afreq [, iskip]" :doc "A high-pass Butterworth filter.") csdoc-opcode-database)
(puthash "STKWurley" '(:template "asignal STKWurley ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STKWurley simulates a Wurlitzer electric piano FM synthesis instrument.") csdoc-opcode-database)
(puthash "multitap" '(:template "ares multitap asig [, itime1, igain1] [, itime2, igain2] [...]" :doc "Multitap delay line implementation.") csdoc-opcode-database)
(puthash "timedseq" '(:template "ktrig timedseq ktimpnt, ifn, kp1 [,kp2, kp3, ...,kpN]" :doc "Time Variant Sequencer") csdoc-opcode-database)
(puthash "sc_lag" '(:template "aout sc_lag ain, klagtime [, initialvalue=0]
kout sc_lag kin, klagtime [, initialvalue=0]" :doc "Exponential Lag") csdoc-opcode-database)
(puthash "specfilt" '(:template "wsig specfilt wsigin, ifhtim" :doc "Filters each channel of an input spectrum.") csdoc-opcode-database)
(puthash "hrtfmove2" '(:template "aleft, aright hrtfmove2 asrc, kAz, kElev, ifilel, ifiler [,ioverlap, iradius, isr]" :doc "Generates dynamic 3d binaural audio for headphones using a Woodworth based spherical head model
      with improved low frequency phase accuracy.") csdoc-opcode-database)
(puthash "outiat" '(:template "outiat ichn, ivalue, imin, imax" :doc "Sends MIDI aftertouch messages at i-rate.") csdoc-opcode-database)
(puthash "cps2pch" '(:template "icps cps2pch ipch, iequal" :doc "Converts a pitch-class value into cycles-per-second (Hz) for equal divisions of the octave.") csdoc-opcode-database)
(puthash "setscorepos" '(:template "setscorepos ipos" :doc "Sets the playback position of the current score performance to a given position.") csdoc-opcode-database)
(puthash "ftslice" '(:template "ftslice ifnsource, ifndest [, kstart, kend, kstep ]" :doc "Copy a slice from an f-table to another f-table") csdoc-opcode-database)
(puthash "outic14" '(:template "outic14 ichn, imsb, ilsb, ivalue, imin, imax" :doc "Sends 14-bit MIDI controller output at i-rate.") csdoc-opcode-database)
(puthash "vdelayxwq" '(:template "aout1, aout2, aout3, aout4 vdelayxwq ain1, ain2, ain3, ain4, adl, imd, iws [, ist]" :doc "Variable delay opcodes with high quality interpolation.") csdoc-opcode-database)
(puthash "readk" '(:template "kres readk ifilname, iformat, iprd" :doc "Periodically reads an orchestra control-signal value from an external file.") csdoc-opcode-database)
(puthash "p" '(:template "p(x)" :doc "Show the value in a given p-field.") csdoc-opcode-database)
(puthash "instr" '(:template "instr i, j, ..." :doc "Starts an instrument block.") csdoc-opcode-database)
(puthash "chani" '(:template "kval chani kchan
aval chani kchan" :doc "Reads data from the software bus") csdoc-opcode-database)
(puthash "active" '(:template "ir active insnum [,iopt [,inorel]]
ir active Sinsname [,iopt [,inorel]]
kres active kinsnum [,iopt [,inorel]]" :doc "Returns the number of active instances of an instrument.") csdoc-opcode-database)
(puthash "tempo" '(:template "tempo ktempo, istartempo" :doc "Apply tempo control to an uninterpreted score.") csdoc-opcode-database)
(puthash "JackoTransport" '(:template "JackoTransport kcommand, [kposition]" :doc "Control the Jack transport.") csdoc-opcode-database)
(puthash "vbapzmove" '(:template "vbapzmove inumchnls, istartndx, asig, idur, ispread, ifldnum, ifld1, ifld2, [...]" :doc "Writes a multi-channel audio signal to a ZAK array with moving virtual sources.") csdoc-opcode-database)
(puthash "loopsegp" '(:template "ksig loopsegp kphase, kvalue0, kdur0, kvalue1 [, kdur1, ... , kdurN-1, kvalueN]" :doc "Control signals based on linear segments.") csdoc-opcode-database)
(puthash "midictrl" '(:template "ival midictrl inum [, imin] [, imax]
kval midictrl inum [, imin] [, imax]" :doc "Get the current value (0-127) of a specified MIDI controller.") csdoc-opcode-database)
(puthash "urandom" '(:template "ax urandom [imin, imax]
ix urandom [imin, imax]
kx urandom [imin, imax]" :doc "truly random opcodes with controllable range.") csdoc-opcode-database)
(puthash "fold" '(:template "ares fold asig, kincr" :doc "Adds artificial foldover to an audio signal.") csdoc-opcode-database)
(puthash "strcmp" '(:template "ires strcmp S1, S2" :doc "Compare strings") csdoc-opcode-database)
(puthash "min" '(:template "amin min ain1, ain2 [, ain3] [, ain4] [...]
kmin min kin1, kin2 [, kin3] [, kin4] [...]
imin min iin1, iin2 [, iin3] [, iin4] [...]" :doc "Produces a signal that is the minimum of any number of input signals.") csdoc-opcode-database)
(puthash "dconv" '(:template "ares dconv asig, isize, ifn" :doc "A direct convolution opcode.") csdoc-opcode-database)
(puthash "babo" '(:template "a1, a2 babo asig, ksrcx, ksrcy, ksrcz, irx, iry, irz [, idiff] [, ifno]" :doc "A physical model reverberator.") csdoc-opcode-database)
(puthash "logbtwo" '(:template "logbtwo(x) (init-rate or control-rate args only)" :doc "Performs a logarithmic base two calculation.") csdoc-opcode-database)
(puthash "ops" '(:template "S(x) (control-rate or init-rate arg)" :doc "Returns an S-type equivalent of an init-time or k-rate argument.") csdoc-opcode-database)
(puthash "pvsmooth" '(:template "fsig pvsmooth fsigin, kacf, kfcf" :doc "Smooth the amplitude and frequency time functions of a pv stream using parallel 1st order
      lowpass IIR filters with time-varying cutoff frequency.") csdoc-opcode-database)
(puthash "slider32f" '(:template "k1,...,k32 slider32f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, ..., ictlnum32, imin32, imax32, init32, ifn32, icutoff32" :doc "Creates a bank of 32 different MIDI control message numbers, filtered before output.") csdoc-opcode-database)
(puthash "changed" '(:template "ktrig changed kvar1 [, kvar2,..., kvarN]" :doc "k-rate signal change detector.") csdoc-opcode-database)
(puthash "biquad" '(:template "ares biquad asig, kb0, kb1, kb2, ka0, ka1, ka2 [, iskip]" :doc "A sweepable general purpose biquadratic digital filter.") csdoc-opcode-database)
(puthash "vbap4" '(:template "ar1, ar2, ar3, ar4 vbap4 asig, kazim [, kelev] [, kspread]" :doc "Distributes an audio signal among 4 channels.") csdoc-opcode-database)
(puthash "resyn" '(:template "asig resyn fin, kscal, kpitch, kmaxtracks, ifn" :doc "Streaming partial track additive synthesis with cubic phase interpolation with
pitch control and support for timescale-modified input") csdoc-opcode-database)
(puthash "pwd" '(:template "Sres pwd" :doc "Asks the underlying operating system for the current directory
      name as a string.") csdoc-opcode-database)
(puthash "pvoc" '(:template "ares pvoc ktimpnt, kfmod, ifilcod [, ispecwp] [, iextractmode] [, ifreqlim] [, igatefn]" :doc "Implements signal reconstruction using an fft-based phase vocoder.") csdoc-opcode-database)
(puthash "vibes" '(:template "ares vibes kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec" :doc "Physical model related to the striking of a metal block.") csdoc-opcode-database)
(puthash "endop" '(:template "endop" :doc "Marks the end of an user-defined opcode block.") csdoc-opcode-database)
(puthash "poscil" '(:template "ares poscil aamp, acps [, ifn, iphs]
ares poscil aamp, kcps [, ifn, iphs]
ares poscil kamp, acps [, ifn, iphs]
ares poscil kamp, kcps [, ifn, iphs]
ires poscil kamp, kcps [, ifn, iphs]
kres poscil kamp, kcps [, ifn, iphs]" :doc "High precision oscillator.") csdoc-opcode-database)
(puthash "dumpk3" '(:template "dumpk3 ksig1, ksig2, ksig3, ifilname, iformat, iprd" :doc "Periodically writes three orchestra control-signal values to an external file.") csdoc-opcode-database)
(puthash "strindexk" '(:template "kpos strindexk S1, S2" :doc "Return the position of the first occurence of a string in another string") csdoc-opcode-database)
(puthash "bpfcos" '(:template "ky bpfcos kx, kx1, ky1, kx2, ..., kxn, kyn
kys[] bpfcos kxs[], kx1, ky1, kx2, ..., kxn, kyn" :doc "Break point function with cosine (easy-in/easy-out) interpolation") csdoc-opcode-database)
(puthash "imagesave" '(:template "imagesave iimagenum, filename" :doc "Save a previously created image.") csdoc-opcode-database)
(puthash "dates" '(:template "Sir dates [ itime]" :doc "Returns as a string the date and time specified.") csdoc-opcode-database)
(puthash "seqtime" '(:template "ktrig_out seqtime ktime_unit, kstart, kloop, kinitndx, kfn_times" :doc "Generates a trigger signal according to the values stored in a table.") csdoc-opcode-database)
(puthash "cbrt" '(:template "ires[] cbrt iarg
kres[] cbrt karg" :doc "Cubic root function.") csdoc-opcode-database)
(puthash "vbapmove" '(:template "ar1[, ar2...] vbapmove asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]
aarray[] vbapmove asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "Distributes an audio signal among many channels with moving virtual sources.") csdoc-opcode-database)
(puthash "dcblock2" '(:template "ares dcblock2 ain [, iorder] [, iskip]" :doc "A DC blocking filter.") csdoc-opcode-database)
(puthash "outch" '(:template "outch kchan1, asig1 [, kchan2] [, asig2] [...]" :doc "Writes multi-channel audio data, with user-controllable channels, to an external device or stream.") csdoc-opcode-database)
(puthash "MixerClear" '(:template "MixerClear" :doc "Resets all channels of a buss to 0.") csdoc-opcode-database)
(puthash "readk2" '(:template "kr1, kr2 readk2 ifilname, iformat, iprd" :doc "Periodically reads two orchestra control-signal values from an external file.") csdoc-opcode-database)
(puthash "strlen" '(:template "ilen strlen Sstr" :doc "Return the length of a string") csdoc-opcode-database)
(puthash "chuap" '(:template "aI3, aV2, aV1 chuap kL, kR0, kC1, kG, kGa, kGb, kE, kC2, iI3, iV2, iV1, ktime_step" :doc "Simulates Chua's oscillator, an LRC oscillator with an active resistor, proved capable of bifurcation and chaotic attractors, with k-rate control of circuit elements.") csdoc-opcode-database)
(puthash "midipitchbend" '(:template "midipitchbend xpitchbend [, ilow] [, ihigh]" :doc "Gets a MIDI pitchbend value.") csdoc-opcode-database)
(puthash "aftouch" '(:template "kaft aftouch [imin] [, imax]" :doc "Get the current after-touch value for this channel.") csdoc-opcode-database)
(puthash "splitrig" '(:template "splitrig ktrig, kndx, imaxtics, ifn, kout1 [,kout2,...,koutN]" :doc "Split a trigger signal") csdoc-opcode-database)
(puthash "wgflute" '(:template "ares wgflute kamp, kfreq, kjet, iatt, idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq] [, ijetrf] [, iendrf]" :doc "Creates a tone similar to a flute.") csdoc-opcode-database)
(puthash "vtablewa" '(:template "vtablewa andx, kfn, ixmode, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "ins" '(:template "ar1, ar2 ins" :doc "Reads stereo audio data from an external device or stream.") csdoc-opcode-database)
(puthash "madsr" '(:template "ares madsr iatt, idec, islev, irel [, idel] [, ireltim]
kres madsr iatt, idec, islev, irel [, idel] [, ireltim]" :doc "Calculates the classical ADSR envelope using the") csdoc-opcode-database)
(puthash "oscils" '(:template "ares oscils iamp, icps, iphs [, iflg]" :doc "A simple, fast sine oscillator") csdoc-opcode-database)
(puthash "FLsetColor2" '(:template "FLsetColor2 ired, igreen, iblue, ihandle" :doc "Sets the secondary (or selection) color of a FLTK widget.") csdoc-opcode-database)
(puthash "guiro" '(:template "ares guiro kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1]" :doc "Semi-physical model of a guiro sound.") csdoc-opcode-database)
(puthash "outkat" '(:template "outkat kchn, kvalue, kmin, kmax" :doc "Sends MIDI aftertouch messages at k-rate.") csdoc-opcode-database)
(puthash "seed" '(:template "seed ival" :doc "Sets the global seed value.") csdoc-opcode-database)
(puthash "inletkid" '(:template "ksignal inletkid Sname, SinstanceID" :doc "Receives a krate signal into an instrument from a named port.") csdoc-opcode-database)
(puthash "loopxseg" '(:template "ksig loopxseg kfreq, ktrig, iphase, ktime0, kvalue0 [, ktime1] [, kvalue1] [, ktime2] [, kvalue2] [...]" :doc "Generate control signal consisting of exponential segments delimited by two or more specified points.") csdoc-opcode-database)
(puthash "outq" '(:template "outq asig1, asig2, asig3, asig4" :doc "Writes 4-channel audio data to an external device or stream.") csdoc-opcode-database)
(puthash "FLtabs" '(:template "FLtabs iwidth, iheight, ix, iy" :doc "Creates a tabbed FLTK interface.") csdoc-opcode-database)
(puthash "adsr" '(:template "ares adsr iatt, idec, islev, irel [, idel]
kres adsr iatt, idec, islev, irel [, idel]" :doc "Calculates the classical ADSR envelope using linear segments.") csdoc-opcode-database)
(puthash "printk" '(:template "printk itime, kval [, ispace] [, inamed]" :doc "Prints one k-rate value at specified intervals.") csdoc-opcode-database)
(puthash "pvsout" '(:template "pvsout fsig, kchan" :doc "Write a fsig to the pvs output bus.") csdoc-opcode-database)
(puthash "dripwater" '(:template "ares dripwater kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "Semi-physical model of a water drop.") csdoc-opcode-database)
(puthash "voice" '(:template "ares voice kamp, kfreq, kphoneme, kform, kvibf, kvamp, ifn, ivfn" :doc "An emulation of a human voice.") csdoc-opcode-database)
(puthash "taninv2" '(:template "ares taninv2 ay, ax
ires taninv2 iy, ix
kres taninv2 ky, kx
...taninv2(ky, kx)... (no rate restriction)" :doc "Returns an arctangent.") csdoc-opcode-database)
(puthash "butterbp" '(:template "ares butterbp asig, xfreq, xband [, iskip]" :doc "A band-pass Butterworth filter.") csdoc-opcode-database)
(puthash "limit" '(:template "ares limit asig, klow, khigh
ires limit isig, ilow, ihigh
kres limit ksig, klow, khigh
ires[] limit isig[], ilow, ihigh
kres[] limit ksig[], klow, khigh" :doc "Sets the lower and upper limits of the value it processes.") csdoc-opcode-database)
(puthash "rnd" '(:template "rnd(x) (init- or control-rate only)" :doc "Returns a random number in a unipolar range at the rate given by the input argument.") csdoc-opcode-database)
(puthash "tablei" '(:template "ares tablei andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires tablei indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres tablei kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing with linear interpolation.") csdoc-opcode-database)
(puthash "cabasa" '(:template "ares cabasa iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "Semi-physical model of a cabasa sound.") csdoc-opcode-database)
(puthash "fog" '(:template "ares fog xamp, xdens, xtrans, aspd, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur [, iphs] [, itmode] [, iskip]" :doc "Audio output is a succession of grains derived from data in a stored function table") csdoc-opcode-database)
(puthash "wguide1" '(:template "ares wguide1 asig, xfreq, kcutoff, kfeedback" :doc "A simple waveguide model consisting of one delay-line and one first-order lowpass filter.") csdoc-opcode-database)
(puthash "vcopy" '(:template "vcopy ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [, kverbose]" :doc "Copies between two vectorial control signals") csdoc-opcode-database)
(puthash "faustcompile" '(:template "ihandle faustcompile Scode, Sargs[,iasync, istacksize]" :doc "Invokes the just-in-time compiler to produce a instantiable DSP process from a Faust program.") csdoc-opcode-database)
(puthash "pvsosc" '(:template "fsig pvsosc kamp, kfreq, ktype, isize [,ioverlap] [, iwinsize] [, iwintype] [, iformat]" :doc "PVS-based oscillator simulator.") csdoc-opcode-database)
(puthash "getftargs" '(:template "Sdst getftargs iftno, ktrig" :doc "Fill a string variable with the arguments used to create a function table at k-rate.") csdoc-opcode-database)
(puthash "balance" '(:template "ares balance asig, acomp [, ihp] [, iskip]" :doc "Adjust one audio signal according to the values of another.") csdoc-opcode-database)
(puthash "resony" '(:template "ares resony asig, kbf, kbw, inum, ksep [, isepmode] [, iscl] [, iskip]" :doc "A bank of second-order bandpass filters, connected in parallel.") csdoc-opcode-database)
(puthash "linsegr" '(:template "ares linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres linsegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "Trace a series of line segments between specified points including a release segment.") csdoc-opcode-database)
(puthash "pdclip" '(:template "aout pdclip ain, kWidth, kCenter [, ibipolar [, ifullscale]]" :doc "Performs linear clipping on an audio signal or a phasor.") csdoc-opcode-database)
(puthash "vbapgmove" '(:template "kr1[, kr2...] vbapgmove idur, ispread, ifldnum, ifld1 [, ifld2] [...]
karray[] vbapgmove idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "Calculates the gains for a sound location between multiple
      channels with moving virtual sources.") csdoc-opcode-database)
(puthash "JackoAudioOut" '(:template "JackoAudioOut ScsoundPortName, asignal" :doc "Sends an audio signal to a Jack port.") csdoc-opcode-database)
(puthash "linrand" '(:template "ares linrand krange
ires linrand krange
kres linrand krange" :doc "WTF: ") csdoc-opcode-database)
(puthash "OSCsend" '(:template "OSCsend kwhen, ihost, iport, idestination[, itype , xdata1, xdata2, ...]" :doc "Sends data to other processes using the OSC protocol") csdoc-opcode-database)
(puthash "ctrl7" '(:template "idest ctrl7 ichan, ictlno, imin, imax [, ifn]
kdest ctrl7 ichan, ictlno, kmin, kmax [, ifn]
adest ctrl7 ichan, ictlno, kmin, kmax [, ifn] [, icutoff]" :doc "Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "ftgen" '(:template "gir ftgen ifn, itime, isize, igen, iarga [, iargb ] [...]
gir ftgen ifn, itime, isize, igen, iarray" :doc "Generate a score function table from within the orchestra.") csdoc-opcode-database)
(puthash "tambourine" '(:template "ares tambourine kamp, idettack [, inum] [, idamp] [, imaxshake] [, ifreq] [, ifreq1] [, ifreq2]" :doc "Semi-physical model of a tambourine sound.") csdoc-opcode-database)
(puthash "diskin" '(:template "ar1 [, ar2 [, ar3 [, ... arN]]] diskin ifilcod[, kpitch[, iskiptim [, iwraparound[, iformat[, iskipinit]]]]]
ar1[] diskin ifilcod[, kpitch[, iskiptim [, iwraparound[, iformat[, iskipinit]]]]]" :doc "Reads audio data from an external device or stream and can alter its pitch.") csdoc-opcode-database)
(puthash "setksmps" '(:template "setksmps iksmps" :doc "Sets the local ksmps value in an instrument or user-defined opcode block") csdoc-opcode-database)
(puthash "flanger" '(:template "ares flanger asig, adel, kfeedback [, imaxd]" :doc "A user controlled flanger.") csdoc-opcode-database)
(puthash "sndload" '(:template "sndload Sfname[, ifmt[, ichns[, isr[, ibas[, iamp[, istrt [, ilpmod[, ilps[, ilpe]]]]]]]]]" :doc "Loads a sound file into memory for use by") csdoc-opcode-database)
(puthash "spsend" '(:template "a1, a2, a3, a4 spsend" :doc "Generates output signals based on a previously defined") csdoc-opcode-database)
(puthash "moogladder" '(:template "asig moogladder ain, kcf, kres[, istor]
asig moogladder ain, acf, kres[, istor]
asig moogladder ain, kcf, ares[, istor]
asig moogladder ain, acf, ares[, istor]" :doc "Moog ladder lowpass filter.") csdoc-opcode-database)
(puthash "ATSsinnoi" '(:template "ar ATSsinnoi ktimepnt, ksinlev, knzlev, kfmod, iatsfile, ipartials [, ipartialoffset, ipartialincr]" :doc "uses the data from an ATS analysis file to perform resynthesis.") csdoc-opcode-database)
(puthash "FLpanelEnd" '(:template "FLpanelEnd" :doc "Marks the end of a group of FLTK widgets contained inside of a window (panel).") csdoc-opcode-database)
(puthash "printarray" '(:template "printarray ixs[] [, Smft, Slabel ]
printarray kxs[] [, ktrig, Sfmt, Slabel ]" :doc "Print the contents of an array") csdoc-opcode-database)
(puthash "sininv" '(:template "sininv(x) (no rate restriction)
sininv(k/i[]) (k- or i-arrays)" :doc "Performs an arcsine function.") csdoc-opcode-database)
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
pylcallni "callable", nresults, iresult1, ..., iresultn, iarg1, ..." :doc "Invoke the specified Python callable at
      k-time and i-time (i suffix), passing the given arguments. The call is
      perfomed in the global environment, and the result (the returning
      value) is copied into the Csound output variables specified.") csdoc-opcode-database)
(puthash "OSCraw" '(:template "Smess[],klen OSCraw iport" :doc "Listen for all OSC messages at a given port.") csdoc-opcode-database)
(puthash "ftgentmp" '(:template "ifno ftgentmp ip1, ip2dummy, isize, igen, iarga, iargb, ..." :doc "Generate a score function table from within the orchestra, which is deleted at the end of the note.") csdoc-opcode-database)
(puthash "crunch" '(:template "ares crunch iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "Semi-physical model of a crunch sound.") csdoc-opcode-database)
(puthash "lineto" '(:template "kres lineto ksig, ktime" :doc "Generate glissandos starting from a control signal.") csdoc-opcode-database)
(puthash "fink" '(:template "fink ifilename, iskipframes, iformat, kin1 [, kin2] [, kin3] [,...]" :doc "Read signals from a file at k-rate.") csdoc-opcode-database)
(puthash "ATSreadnz" '(:template "kenergy ATSreadnz ktimepnt, iatsfile, iband" :doc "reads data from an ATS file.") csdoc-opcode-database)
(puthash "midiin" '(:template "kstatus, kchan, kdata1, kdata2 midiin" :doc "Returns a generic MIDI message received by the MIDI IN port.") csdoc-opcode-database)
(puthash "pvsfilter" '(:template "fsig pvsfilter fsigin, fsigfil, kdepth[, igain]" :doc "Multiply amplitudes of a pvoc stream by those of a second
pvoc stream, with dynamic scaling.") csdoc-opcode-database)
(puthash "outq4" '(:template "outq4 asig" :doc "Writes samples to quad channel 4 of an external device or stream.") csdoc-opcode-database)
(puthash "chnget" '(:template "ival chnget Sname
kval chnget Sname
aval chnget Sname
Sval chnget Sname
Sval chngetks Sname" :doc "Reads data from the software bus.") csdoc-opcode-database)
(puthash "outkpb" '(:template "outkpb kchn, kvalue, kmin, kmax" :doc "Sends MIDI pitch-bend messages at k-rate.") csdoc-opcode-database)
(puthash "vtablei" '(:template "vtablei indx, ifn, interp, ixmode, iout1 [, iout2, iout3, .... , ioutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "slider16f" '(:template "k1,...,k16 slider16f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1,..., ictlnum16, imin16, imax16, init16, ifn16, icutoff16" :doc "Creates a bank of 16 different MIDI control message numbers, filtered before output.") csdoc-opcode-database)
(puthash "cudanal" '(:template "fsig cudanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]" :doc "Generate an fsig from a mono audio source ain, using phase
      vocoder overlap-add analysis and GPU hardware. Experimental and
      only available as source code at the moment.") csdoc-opcode-database)
(puthash "igoto" '(:template "igoto label" :doc "Transfer control during the i-time pass.") csdoc-opcode-database)
(puthash "tlineto" '(:template "kres tlineto ksig, ktime, ktrig" :doc "Generate glissandos starting from a control signal.") csdoc-opcode-database)
(puthash "reverb2" '(:template "ares reverb2 asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] [, inumAlpas] [, ifnAlpas]" :doc "Same as the nreverb opcode.") csdoc-opcode-database)
(puthash "ftloadk" '(:template "ftloadk Sfilename, ktrig, iflag, ifn1 [, ifn2] [...]" :doc "Load a set of previously-allocated tables from a file.") csdoc-opcode-database)
(puthash "pitch" '(:template "koct, kamp pitch asig, iupdte, ilo, ihi, idbthresh [, ifrqs] [, iconf] [, istrt] [, iocts] [, iq] [, inptls] [, irolloff] [, iskip]" :doc "Tracks the pitch of a signal.") csdoc-opcode-database)
(puthash "sfplist" '(:template "sfplist ifilhandle" :doc "Prints a list of all presets of a SoundFont2 (SF2) sample file.") csdoc-opcode-database)
(puthash "genarray" '(:template "karray genarray kstart, kens[, inc]
iarray genarray istart, iens[, inc]" :doc "Generate a vector with an arithmetic sequence.") csdoc-opcode-database)
(puthash "shaker" '(:template "ares shaker kamp, kfreq, kbeans, kdamp, ktimes [, idecay]" :doc "Sounds like the shaking of a maraca or similar gourd instrument.") csdoc-opcode-database)
(puthash "serialPrint" '(:template "serialPrint iPort" :doc "Print data from a serial port.") csdoc-opcode-database)
(puthash "strchark" '(:template "kchr strchark Sstr[, kpos]" :doc "Return the ASCII code of a character in a string") csdoc-opcode-database)
(puthash "cent" '(:template "cent(x)" :doc "Calculates a factor to raise/lower a frequency by a given amount of cents.") csdoc-opcode-database)
(puthash "ampdbfs" '(:template "ampdbfs(x) (no rate restriction)" :doc "Returns the amplitude equivalent (in 16-bit signed integer scale) of the full scale decibel (dB FS) value") csdoc-opcode-database)
(puthash "dispfft" '(:template "dispfft xsig, iprd, iwsiz [, iwtyp] [, idbout] [, iwtflg] [,imin] [,imax]" :doc "Displays the Fourier Transform of an audio or control signal.") csdoc-opcode-database)
(puthash "zdf_1pole_mode" '(:template "alp, ahp zdf_1pole_mode ain, xcf [, istor]" :doc "Zero-delay feedback implementation of 1 pole filter with multimode output.") csdoc-opcode-database)
(puthash "printk2" '(:template "printk2 kvar [, inumspaces] [, inamed]" :doc "Prints a new value every time a control variable changes.") csdoc-opcode-database)
(puthash "ftcps" '(:template "ftcps(x) (init-rate args only)" :doc "Returns the base frequency of a stored function table in Hz.") csdoc-opcode-database)
(puthash "rezzy" '(:template "ares rezzy asig, xfco, xres [, imode, iskip]" :doc "A resonant low-pass filter.") csdoc-opcode-database)
(puthash "trcross" '(:template "fsig trcross fin1, fin2, ksearch, kdepth [, kmode]" :doc "Streaming partial track cross-synthesis.") csdoc-opcode-database)
(puthash "pvsbufread2" '(:template "fsig pvsbufread2 ktime, khandle, ift1, ift2" :doc "This opcode reads a circular buffer of f-signals (streaming PV signals), with binwise additional delays.") csdoc-opcode-database)
(puthash "vexpv" '(:template "vexpv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs exponential operations between two vectorial control signals") csdoc-opcode-database)
(puthash "link_beat_request" '(:template "link_beat_request i_peer, k_beat [, k_at_time_seconds [, k_quantum ]]" :doc "Requests the global network Ableton Link session to adopt a specific beat number and time.") csdoc-opcode-database)
(puthash "mp3scal" '(:template "asig, asig2, ktime mp3scal Sfile, ktimescal, kpitch, kamp[, iskip, ifftsize, idecim, ilock]" :doc "Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.") csdoc-opcode-database)
(puthash "serialEnd" '(:template "serialEnd iPort" :doc "Close a serial port.") csdoc-opcode-database)
(puthash "syncloop" '(:template "asig syncloop kamp, kfreq, kpitch, kgrsize, kprate, klstart, klend, ifun1, ifun2, iolaps[,istart, iskip]" :doc "Synchronous granular synthesis.") csdoc-opcode-database)
(puthash "inletk" '(:template "ksignal inletk Sname" :doc "Receives a krate signal into an instrument from a named port.") csdoc-opcode-database)
(puthash "FLhvsBoxSetValue" '(:template "FLhvsBox kx, ky, ihandle" :doc "Sets the cursor position of a previously-declared FLhvsBox widget.") csdoc-opcode-database)
(puthash "filelen" '(:template "ir filelen ifilcod, [iallowraw]" :doc "Returns the length of a sound file.") csdoc-opcode-database)
(puthash "hdf5read" '(:template "xout1[, xout2, xout3, ..., xoutN] hdf5read ifilename, ivariablename1[, ivariablename2, ivariablename3, ..., ivariablenameN]" :doc "Read signals and arrays from an hdf5 file.") csdoc-opcode-database)
(puthash "outs" '(:template "outs asig1, asig2" :doc "Writes stereo audio data to an external device or stream.") csdoc-opcode-database)
(puthash "pvstrace" '(:template "fsig pvstrace fsigin, kn
fsig, kBins[] pvstrace fsigin, kn[, isort]" :doc "Retain only the N loudest bins.") csdoc-opcode-database)
(puthash "select" '(:template "aout select a1, a2, aless, aequal, amore" :doc "Select sample value based on audio-rate comparisons.") csdoc-opcode-database)
(puthash "zamod" '(:template "ares zamod asig, kzamod" :doc "Modulates one a-rate signal by a second one.") csdoc-opcode-database)
(puthash "tempest" '(:template "ktemp tempest kin, iprd, imindur, imemdur, ihp, ithresh, ihtim, ixfdbak, istartempo, ifn [, idisprd] [, itweek]" :doc "Estimate the tempo of beat patterns in a control signal.") csdoc-opcode-database)
(puthash "oscilikts" '(:template "ares oscilikts xamp, xcps, kfn, async, kphs [, istor]" :doc "A linearly interpolated oscillator with sync status that allows changing the table number at k-rate.") csdoc-opcode-database)
(puthash "dbfsamp" '(:template "dbfsamp(x) (init-rate or control-rate args only)" :doc "Returns the decibel equivalent of the raw amplitude") csdoc-opcode-database)
(puthash "ftlen" '(:template "ftlen(x) (init-rate args only)" :doc "Returns the size of a stored function table.") csdoc-opcode-database)
(puthash "faustctl" '(:template "faustctl idsp,Scontrol,kval" :doc "Adjusts a given control in a Faust DSP instance.") csdoc-opcode-database)
(puthash "waveset" '(:template "ares waveset ain, krep [, ilen]" :doc "A simple time stretch by repeating cycles.") csdoc-opcode-database)
(puthash "STKShakers" '(:template "asignal STKShakers ifrequency, iamplitude, [kenerg, kv1[, kdecay, kv2[, kshake, kv3[, knum, kv4[, kres, kv5[, kinstr, kv6]]]]]]" :doc "STKShakers is an instrument that simulates environmental sounds or collisions of multiple independent sound producing objects.") csdoc-opcode-database)
(puthash "FLcolor" '(:template "FLcolor ired, igreen, iblue [, ired2, igreen2, iblue2]" :doc "A FLTK opcode that sets the primary colors.") csdoc-opcode-database)
(puthash "tone" '(:template "ares tone asig, khp [, iskip]" :doc "A first-order recursive low-pass filter with variable frequency response.") csdoc-opcode-database)
(puthash "slider32tablef" '(:template "kflag slider32tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum32, imin32, imax32, init32, ifn32, icutoff32" :doc "Stores a bank of 32 different MIDI control messages to a table, filtered before output.") csdoc-opcode-database)
(puthash "ntrpol" '(:template "ares ntrpol asig1, asig2, kpoint [, imin] [, imax]
ires ntrpol isig1, isig2, ipoint [, imin] [, imax]
kres ntrpol ksig1, ksig2, kpoint [, imin] [, imax]" :doc "Calculates the weighted mean value of two input signals.") csdoc-opcode-database)
(puthash "cudasynth" '(:template "asig cudasynth kamp, kfreq, itab, iftab, iatab[, inum]
asig cudasynth fsig, kamp, kfreq[, inum]
asig cudasynth fsig" :doc "Synthesis by additive synthesis and inverse FFT. Experimental and
      only available as source code at the moment.") csdoc-opcode-database)
(puthash "times" '(:template "ires times
kres times" :doc "Read absolute time in seconds.") csdoc-opcode-database)
(puthash "pvsanal" '(:template "fsig pvsanal ain, ifftsize, ioverlap, iwinsize, iwintype [, iformat] [, iinit]" :doc "Generate an fsig from a mono audio source ain, using phase vocoder overlap-add analysis.") csdoc-opcode-database)
(puthash "outipat" '(:template "outipat ichn, inotenum, ivalue, imin, imax" :doc "Sends polyphonic MIDI aftertouch messages at i-rate.") csdoc-opcode-database)
(puthash "peak" '(:template "kres peak asig
kres peak ksig" :doc "Maintains the output equal to the highest absolute value received.") csdoc-opcode-database)
(puthash "cpsmidinn" '(:template "cpsmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "Converts a Midi note number value to cycles-per-second.") csdoc-opcode-database)
(puthash "fluidCCi" '(:template "fluidCCi iEngineNumber, iChannelNumber, iControllerNumber, iValue" :doc "Sends a MIDI controller data message to fluid.") csdoc-opcode-database)
(puthash "loop_ge" '(:template "loop_ge indx, idecr, imin, label
loop_ge kndx, kdecr, kmin, label" :doc "Looping constructions.") csdoc-opcode-database)
(puthash "setrow" '(:template "i/kout[] setrowi/kin[],i/krow" :doc "Sets a given row of a 2-dimensional array from a vector.") csdoc-opcode-database)
(puthash "delayk" '(:template "kr delayk ksig, idel[, imode]
kr vdel_k ksig, kdel, imdel[, imode]" :doc "Delays an input signal by some time interval.") csdoc-opcode-database)
(puthash "link_peers" '(:template "k_count link_peers i_peer" :doc "Returns the number of peers in the session.") csdoc-opcode-database)
(puthash "strupperk" '(:template "Sdst strupperk Ssrc" :doc "Convert a string to upper case") csdoc-opcode-database)
(puthash "fmrhode" '(:template "ares fmrhode kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "Uses FM synthesis to create a Fender Rhodes electric piano sound.") csdoc-opcode-database)
(puthash "lposcilsa2" '(:template "ar1, ar2 lposcilsa2 aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "Read stereo sampled sound from a table with looping and high precision.") csdoc-opcode-database)
(puthash "trigger" '(:template "kout trigger ksig, kthreshold, kmode" :doc "Informs when a krate signal crosses a threshold.") csdoc-opcode-database)
(puthash "outleta" '(:template "outleta Sname, asignal" :doc "Sends an arate signal out from an instrument to a named port.") csdoc-opcode-database)
(puthash "maxaccum" '(:template "maxaccum aAccumulator, aInput" :doc "Accumulates the maximum value of audio signals.") csdoc-opcode-database)
(puthash "zkr" '(:template "kres zkr kndx" :doc "Reads from a location in zk space at k-rate.") csdoc-opcode-database)
(puthash "diff" '(:template "ares diff asig [, iskip]
kres diff ksig [, iskip]" :doc "Modify a signal by differentiation.") csdoc-opcode-database)
(puthash "sndwarp" '(:template "ares [, ac] sndwarp xamp, xtimewarp, xresample, ifn1, ibeg, iwsize, irandw, ioverlap, ifn2, itimemode" :doc "Reads a mono sound sample from a table and applies time-stretching and/or pitch modification.") csdoc-opcode-database)
(puthash "p5gdata" '(:template "kres p5gdata kcontrol" :doc "Reads data fields from an external P5 Glove.") csdoc-opcode-database)
(puthash "beosc" '(:template "aout beosc xfreq, kbw [, ifn, iphs, inoisetype ]" :doc "Band-Enhanced Oscillator") csdoc-opcode-database)
(puthash "pvsmorph" '(:template "fsig pvsmorph fsig1, fsig2, kampint, kfrqint" :doc "Performs morphing (or interpolation) between two source fsigs.") csdoc-opcode-database)
(puthash "rewindscore" '(:template "rewindscore" :doc "Rewinds the playback position of the current score performance.") csdoc-opcode-database)
(puthash "trim" '(:template "trim_i iarray, ilen
trim xarray, klen" :doc "Adjust size o a one-dimensional array.") csdoc-opcode-database)
(puthash "trscale" '(:template "fsig trscale fin, kpitch[, kgain]" :doc "Streaming partial track frequency scaling.") csdoc-opcode-database)
(puthash "log2" '(:template "log2(x) (no rate restriction)
log2(k/i[]) (k- or i-arrays )" :doc "Returns a base 2 log.") csdoc-opcode-database)
(puthash "vdivv_i" '(:template "vdivv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs division between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "mxadsr" '(:template "ares mxadsr iatt, idec, islev, irel [, idel] [, ireltim]
kres mxadsr iatt, idec, islev, irel [, idel] [, ireltim]" :doc "Calculates the classical ADSR envelope using the") csdoc-opcode-database)
(puthash "linseg" '(:template "ares linseg ia, idur1, ib [, idur2] [, ic] [...]
kres linseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "Trace a series of line segments between specified points.") csdoc-opcode-database)
(puthash "foscil" '(:template "ares foscil xamp, kcps, xcar, xmod, kndx, ifn [, iphs]" :doc "A basic frequency modulated oscillator.") csdoc-opcode-database)
(puthash "scoreline" '(:template "scoreline Sin, ktrig" :doc "Issues one or more score line events from an instrument.") csdoc-opcode-database)
(puthash "vrandh" '(:template "vrandh ifn, krange, kcps, ielements [, idstoffset] [, iseed] [, isize] [, ioffset]" :doc "Generates a vector of random numbers stored into a table, holding the values for a period of time.") csdoc-opcode-database)
(puthash "strupper" '(:template "Sdst strupper Ssrc" :doc "Convert a string to upper case") csdoc-opcode-database)
(puthash "compilecsd" '(:template "ires compilecsd Sfilename" :doc "compiles a new orchestra from an ASCII file") csdoc-opcode-database)
(puthash "slider8" '(:template "i1,...,i8 slider8 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8
k1,...,k8 slider8 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8" :doc "Creates a bank of 8 different MIDI control message numbers.") csdoc-opcode-database)
(puthash "semitone" '(:template "semitone(x)" :doc "Calculates a factor to raise/lower a frequency by a given amount of semitones.") csdoc-opcode-database)
(puthash "noteondur2" '(:template "noteondur2 ichn, inum, ivel, idur" :doc "Sends a noteon and a noteoff MIDI message both with the same channel, number and velocity.") csdoc-opcode-database)
(puthash "interleave" '(:template "kout[] interleave kin1[], kin2[]" :doc "Interleaves arrays into a a single one by placing the input data
    in alternate positions.") csdoc-opcode-database)
(puthash "fluidCCk" '(:template "fluidCCk iEngineNumber, iChannelNumber, iControllerNumber, kValue" :doc "Sends a MIDI controller data message to fluid.") csdoc-opcode-database)
(puthash "sumarray" '(:template "ksum sumarray karray" :doc "returns the sum of the elements in an array.") csdoc-opcode-database)
(puthash "midinoteonoct" '(:template "midinoteonoct xoct, xvelocity" :doc "Gets a MIDI note number value as octave-point-decimal value.") csdoc-opcode-database)
(puthash "rspline" '(:template "ares rspline xrangeMin, xrangeMax, kcpsMin, kcpsMax
kres rspline krangeMin, krangeMax, kcpsMin, kcpsMax" :doc "Generate random spline curves.") csdoc-opcode-database)
(puthash "randomi" '(:template "ares randomi kmin, kmax, xcps [,imode] [,ifirstval]
kres randomi kmin, kmax, kcps [,imode] [,ifirstval]" :doc "Generates a user-controlled random number series with interpolation between each new number.") csdoc-opcode-database)
(puthash "tab" '(:template "ir tab_i indx, ifn[, ixmode]
kr tab kndx, ifn[, ixmode]
ar tab xndx, ifn[, ixmode]
tabw_i isig, indx, ifn [,ixmode]
tabw ksig, kndx, ifn [,ixmode]
tabw asig, andx, ifn [,ixmode]" :doc "WTF: ") csdoc-opcode-database)
(puthash "socksend" '(:template "socksend asig, Sipaddr, iport, ilength
socksend ksig, Sipaddr, iport, ilength
socksends asigl, asigr, Sipaddr, iport, ilength
stsend asig, Sipaddr, iport" :doc "Sends data to other processes using the low-level UDP or TCP protocols") csdoc-opcode-database)
(puthash "hvs1" '(:template "hvs1 kx, inumParms, inumPointsX, iOutTab, iPositionsTab, iSnapTab [, iConfigTab]" :doc "Allows one-dimensional Hyper Vectorial Synthesis (HVS) controlled by externally-updated k-variables.") csdoc-opcode-database)
(puthash "FLmouse" '(:template "kx, ky, kb1, kb2, kb3 FLmouse [imode]" :doc "Returns the mouse position and the state of the three mouse buttons.") csdoc-opcode-database)
(puthash "slider64" '(:template "i1,...,i64 slider64 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum64, imin64, imax64, init64, ifn64
k1,...,k64 slider64 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum64, imin64, imax64, init64, ifn64" :doc "Creates a bank of 64 different MIDI control message numbers.") csdoc-opcode-database)
(puthash "grain2" '(:template "ares grain2 kcps, kfmd, kgdur, iovrlp, kfn, iwfn [, irpow] [, iseed] [, imode]" :doc "Easy-to-use granular synthesis texture generator.") csdoc-opcode-database)
(puthash "FLslidBnkSetk" '(:template "FLslidBnkSetk ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "modify the values of a slider bank.") csdoc-opcode-database)
(puthash "bbcuts" '(:template "a1,a2 bbcuts asource1, asource2, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice]" :doc "Generates breakbeat-style cut-ups of a stereo audio stream.") csdoc-opcode-database)
(puthash "oscil3" '(:template "ares oscil3 xamp, xcps [, ifn, iphs]
kres oscil3 kamp, kcps [, ifn, iphs]" :doc "A simple oscillator with cubic interpolation.") csdoc-opcode-database)
(puthash "chanctrl" '(:template "ival chanctrl ichnl, ictlno [, ilow] [, ihigh]
kval chanctrl ichnl, ictlno [, ilow] [, ihigh]" :doc "Get the current value of a MIDI channel controller.") csdoc-opcode-database)
(puthash "FLkeyIn" '(:template "kascii FLkeyIn [ifn]" :doc "Reports keys pressed (on alphanumeric keyboard) when an FLTK panel has focus.") csdoc-opcode-database)
(puthash "vwrap" '(:template "vwrap ifn, kmin, kmax, ielements" :doc "Limiting and Wrapping Vectorial Signals") csdoc-opcode-database)
(puthash "pvsblur" '(:template "fsig pvsblur fsigin, kblurtime, imaxdel" :doc "Average the amp/freq time functions of each analysis channel for
    a specified time.") csdoc-opcode-database)
(puthash "FLslidBnkGetHandle" '(:template "ihandle FLslidBnkGetHandle" :doc "gets the handle of last slider bank created.") csdoc-opcode-database)
(puthash "delayw" '(:template "delayw asig" :doc "Writes the audio signal to a digital delay line.") csdoc-opcode-database)
(puthash "scoreline_i" '(:template "scoreline_i Sin" :doc "Issues one or more score line events from an instrument at i-time.") csdoc-opcode-database)
(puthash "scalearray" '(:template "scalearray tarray, kmin, kmax[, kleft, kright]" :doc "Scales the values in a range of a vector (one dimensional array).") csdoc-opcode-database)
(puthash "octave" '(:template "octave(x)" :doc "Calculates a factor to raise/lower a frequency by a given amount of octaves.") csdoc-opcode-database)
(puthash "squinewave" '(:template "aout [, asyncout] squinewave acps, aClip, aSkew, asyncin [, iMinSweep] [, iphase]
aout [, asyncout] squinewave acps, aClip, aSkew [, ksyncin] [, iMinSweep] [, iphase]" :doc "A mostly bandlimited shape-shifting square-pulse-saw-sinewave oscillator with hardsync.") csdoc-opcode-database)
(puthash "syncgrain" '(:template "asig syncgrain kamp, kfreq, kpitch, kgrsize, kprate, ifun1, ifun2, iolaps" :doc "Synchronous granular synthesis.") csdoc-opcode-database)
(puthash "FLupdate" '(:template "FLupdate" :doc "Same as the FLrun opcode.") csdoc-opcode-database)
(puthash "wterrain" '(:template "aout wterrain kamp, kpch, k_xcenter, k_ycenter, k_xradius, k_yradius, itabx, itaby" :doc "A simple wave-terrain synthesis opcode.") csdoc-opcode-database)
(puthash "noteon" '(:template "noteon ichn, inum, ivel" :doc "Send a noteon message to the MIDI OUT port.") csdoc-opcode-database)
(puthash "midic7" '(:template "idest midic7 ictlno, imin, imax [, ifn]
kdest midic7 ictlno, kmin, kmax [, ifn]" :doc "Allows a floating-point 7-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "mp3len" '(:template "ir mp3len ifilcod" :doc "Returns the length of an MP3 sound file.") csdoc-opcode-database)
(puthash "chn" '(:template "chn_k Sname, imode[, itype, idflt, imin, ima, ix, iy, iwidth, iheight, Sattributes]
chn_a Sname, imode
chn_S Sname, imode" :doc "Declare a channel of the named software bus.") csdoc-opcode-database)
(puthash "expsegb" '(:template "ares expsegb ia, itim1, ib [, itim2] [, ic] [...]
kres expsegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "Trace a series of exponential segments between specified
      absolute points.") csdoc-opcode-database)
(puthash "linlin" '(:template "ky linlin kx, ky0, ky1 [, kx0, kx1 ]
iy linlin ix, iy0, iy1 [, ix0, ix1 ]
kys[] linlin kxs[], ky0, ky1 [, kx0, kx1 ]
iys[] linlin ixs[], ky0, ky1, [ kx0, kx1 ]
kC[] linlin kx, kA[], kB[] [, kx0, kx1 ]" :doc "Linear to linear interpolation") csdoc-opcode-database)
(puthash "deltapn" '(:template "ares deltapn xnumsamps" :doc "Taps a delay line at variable offset times.") csdoc-opcode-database)
(puthash "JackoOn" '(:template "JackoOn [iactive]" :doc "Enables or disables all Jack ports.") csdoc-opcode-database)
(puthash "FLscroll" '(:template "FLscroll iwidth, iheight [, ix] [, iy]" :doc "A FLTK opcode that adds scroll bars to an area.") csdoc-opcode-database)
(puthash "slider32" '(:template "i1,...,i32 slider32 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum32, imin32, imax32, init32, ifn32
k1,...,k32 slider32 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum32, imin32, imax32, init32, ifn32" :doc "Creates a bank of 32 different MIDI control message numbers.") csdoc-opcode-database)
(puthash "pvscross" '(:template "fsig pvscross fsrc, fdest, kamp1, kamp2" :doc "Performs cross-synthesis between two source fsigs.") csdoc-opcode-database)
(puthash "outc" '(:template "outc asig1 [, asig2] [...]" :doc "Writes audio data with an arbitrary number of channels to an external device or stream.") csdoc-opcode-database)
(puthash "repluck" '(:template "ares repluck iplk, kamp, icps, kpick, krefl, axcite" :doc "Physical model of the plucked string.") csdoc-opcode-database)
(puthash "compress" '(:template "ar compress aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook" :doc "Compress, limit, expand, duck or gate an audio signal.") csdoc-opcode-database)
(puthash "slider64tablef" '(:template "kflag slider64tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum64, imin64, imax64, init64, ifn64, icutoff64" :doc "Stores a bank of 64 different MIDI control messages to a table, filtered before output.") csdoc-opcode-database)
(puthash "reshapearray" '(:template "reshapearray array[], isize0 [, isize1 ]" :doc "Reshape an array, maintaining its capacity") csdoc-opcode-database)
(puthash "qinf" '(:template "qinf(x) (no rate restriction)" :doc "Questions whether the argument is a infinite number") csdoc-opcode-database)
(puthash "inx" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16 inx" :doc "Reads a 16-channel audio signal from an external device or stream.") csdoc-opcode-database)
(puthash "tableimix" '(:template "tableimix idft, idoff, ilen, is1ft, is1off, is1g, is2ft, is2off, is2g" :doc "Mixes two tables.") csdoc-opcode-database)
(puthash "pvsceps" '(:template "keps[] pvsceps fsig[, icoefs]" :doc "Calculate the cepstrum of a pvs input, optionally liftering coefficients.") csdoc-opcode-database)
(puthash "vtaba" '(:template "vtaba andx, ifn, aout1 [, aout2, aout3, .... , aoutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "mfb" '(:template "kout[] mfb kin[],klow,khigh,ibands" :doc "Mel scale filterbank for spectral magnitudes.") csdoc-opcode-database)
(puthash "inletf" '(:template "fsignal inletf Sname" :doc "Receives an frate signal (fsig) into an instrument from a named port.") csdoc-opcode-database)
(puthash "signum" '(:template "signum(x) (no rate restriction)" :doc "Performs a signum function.") csdoc-opcode-database)
(puthash "zkw" '(:template "zkw kval, kndx" :doc "Writes to a zk variable at k-rate without mixing.") csdoc-opcode-database)
(puthash "midic14" '(:template "idest midic14 ictlno1, ictlno2, imin, imax [, ifn]
kdest midic14 ictlno1, ictlno2, kmin, kmax [, ifn]" :doc "Allows a floating-point 14-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "lposcil3" '(:template "ares lposcil3 kamp, kfreqratio, kloop, kend, ifn [, iphs]" :doc "Read sampled sound from a table with high precision and cubic interpolation.") csdoc-opcode-database)
(puthash "sin" '(:template "sin(x) (no rate restriction)
sin(k/i[]) (k- or i-arrays )" :doc "Performs a sine function.") csdoc-opcode-database)
(puthash "round" '(:template "round(x) (init-, control-, or audio-rate arg allowed)
round(k/i[]) (k- or i-arrays )" :doc "Returns the integer value nearest to") csdoc-opcode-database)
(puthash "comb" '(:template "ares comb asig, krvt, ilpt [, iskip] [, insmps]" :doc "Reverberates an input signal with a") csdoc-opcode-database)
(puthash "dot" '(:template "kres/iresdot karr1[]/iarr1[], karr2[]/iarr2[] (k- or i-arrays )" :doc "Calculates the dot product of two arrays.") csdoc-opcode-database)
(puthash "tableigpw" '(:template "tableigpw ifn" :doc "Writes a table's guard point.") csdoc-opcode-database)
(puthash "vsubv" '(:template "vsubv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs subtraction between two vectorial control signals") csdoc-opcode-database)
(puthash "mode" '(:template "aout mode ain, xfreq, xQ [, iskip]" :doc "A filter that simulates a mass-spring-damper system") csdoc-opcode-database)
(puthash "mrtmsg" '(:template "mrtmsg imsgtype" :doc "Send system real-time messages to the MIDI OUT port.") csdoc-opcode-database)
(puthash "specscal" '(:template "wsig specscal wsigin, ifscale, ifthresh" :doc "Scales an input spectral datablock with spectral envelopes.") csdoc-opcode-database)
(puthash "slider8f" '(:template "k1,...,k8 slider8f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, ..., ictlnum8, imin8, imax8, init8, ifn8, icutoff8" :doc "Creates a bank of 8 different MIDI control message numbers, filtered before output.") csdoc-opcode-database)
(puthash "balance2" '(:template "ares balance2 asig, acomp [, ihp] [, iskip]" :doc "Adjust one audio signal according to the values of another.") csdoc-opcode-database)
(puthash "seqtime2" '(:template "ktrig_out seqtime2 ktrig_in, ktime_unit, kstart, kloop, kinitndx, kfn_times" :doc "Generates a trigger signal according to the values stored in a table.") csdoc-opcode-database)
(puthash "flashtxt" '(:template "flashtxt iwhich, String" :doc "Allows text to be displayed from instruments like sliders") csdoc-opcode-database)
(puthash "FLgetsnap" '(:template "inumsnap FLgetsnap index [, igroup]" :doc "Retrieves a previously stored FLTK snapshot.") csdoc-opcode-database)
(puthash "maxabs" '(:template "amax maxabs ain1, ain2 [, ain3] [, ain4] [...]
kmax maxabs kin1, kin2 [, kin3] [, kin4] [...]" :doc "Produces a signal that is the maximum of the absolute values of any number of input signals.") csdoc-opcode-database)
(puthash "partikkel" '(:template "a1 [, a2, a3, a4, a5, a6, a7, a8] partikkel agrainfreq, kdistribution, idisttab, async, kenv2amt, ienv2tab, ienv_attack, ienv_decay, ksustain_amount, ka_d_ratio, kduration, kamp, igainmasks, kwavfreq, ksweepshape, iwavfreqstarttab, iwavfreqendtab, awavfm, ifmamptab, kfmenv, icosine, ktraincps, knumpartials, kchroma, ichannelmasks, krandommask, kwaveform1, kwaveform2, kwaveform3, kwaveform4, iwaveamptab, asamplepos1, asamplepos2, asamplepos3, asamplepos4, kwavekey1, kwavekey2, kwavekey3, kwavekey4, imax_grains [, iopcode_id, ipanlaws]" :doc "Granular synthesizer with 'per grain' control
      over many of its parameters.  Has a sync input to
      sychronize its internal grain scheduler clock to an external
      clock source.") csdoc-opcode-database)
(puthash "bbcutm" '(:template "a1 bbcutm asource, ibps, isubdiv, ibarlength, iphrasebars, inumrepeats [, istutterspeed] [, istutterchance] [, ienvchoice ]" :doc "Generates breakbeat-style cut-ups of a mono audio stream.") csdoc-opcode-database)
(puthash "slider64f" '(:template "k1,...,k64 slider64f ichan, ictlnum1, imin1, imax1, init1, ifn1, icutoff1,..., ictlnum64, imin64, imax64, init64, ifn64, icutoff64" :doc "Creates a bank of 64 different MIDI control message numbers, filtered before output.") csdoc-opcode-database)
(puthash "midinoteoncps" '(:template "midinoteoncps xcps, xvelocity" :doc "Gets a MIDI note number as a cycles-per-second frequency.") csdoc-opcode-database)
(puthash "median" '(:template "ares median asig, ksize, imaxsize [, iskip]" :doc "A median filter, a variant FIR lowpass filter.") csdoc-opcode-database)
(puthash "abs" '(:template "abs(x) (no rate restriction)
abs(k/i[]) (k- or i-arrays )" :doc "Returns an absolute value.") csdoc-opcode-database)
(puthash "pow" '(:template "ares pow aarg, kpow [, inorm]
ires pow iarg, ipow [, inorm]
kres pow karg, kpow [, inorm]
ires[] pow iarg[], ipow[]
kres[] pow karg[], kpow[]
ires[] pow iarg[], ipow
kres[] pow karg[], kpow" :doc "Computes one argument to the power of another argument.") csdoc-opcode-database)
(puthash "STKClarinet" '(:template "asignal STKClarinet ifrequency, iamplitude, [kstiff, kv1[, knoise, kv2[, klfo, kv3[, klfodepth, kv4[, kbreath, kv5]]]]]" :doc "STKClarinet uses a simple clarinet physical model.") csdoc-opcode-database)
(puthash "link_create" '(:template "i_peer link_create [i_bpm]" :doc "Creates a peer in an Ableton Link network session.") csdoc-opcode-database)
(puthash "loscil3" '(:template "ar1 [,ar2] loscil3 xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]
aph, ar1 [,ar2] loscil3phs xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]" :doc "Read sampled sound from a table using cubic interpolation. A version that outputs the exact
      table position (phase) corresponding to the output sample is
      provided as an alternative opcode.") csdoc-opcode-database)
(puthash "outkc" '(:template "outkc kchn, knum, kvalue, kmin, kmax" :doc "Sends MIDI controller messages at k-rate.") csdoc-opcode-database)
(puthash "ptablew" '(:template "ptablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
ptablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
ptablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "Change the contents of existing function tables of any length.") csdoc-opcode-database)
(puthash "strtol" '(:template "ir strtol Sstr
ir strtol indx" :doc "Converts a string to a signed integer (i-rate).") csdoc-opcode-database)
(puthash "strcmpk" '(:template "kres strcmpk S1, S2" :doc "Compare strings") csdoc-opcode-database)
(puthash "setcol" '(:template "i/kout[] setcoli/kin[],i/kcol" :doc "Sets a given column of a 2-dimensional array from a vector.") csdoc-opcode-database)
(puthash "tableikt" '(:template "ares tableikt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
kres tableikt kndx, kfn [, ixmode] [, ixoff] [, iwrap]" :doc "Provides k-rate control over table numbers.") csdoc-opcode-database)
(puthash "trirand" '(:template "ares trirand krange
ires trirand krange
kres trirand krange" :doc "WTF: ") csdoc-opcode-database)
(puthash "scans" '(:template "ares scans kamp, kfreq, ifn, id [, iorder]" :doc "Generate audio output using scanned synthesis.") csdoc-opcode-database)
(puthash "expsega" '(:template "ares expsega ia, idur1, ib [, idur2] [, ic] [...]" :doc "An exponential segment generator operating at a-rate.") csdoc-opcode-database)
(puthash "lpshold" '(:template "ksig lpshold kfreq, ktrig, iphase, ktime0, kvalue0 [, kvalue1] [, ktime1] [, kvalue2] [, ktime2] [...]" :doc "Generate control signal consisting of held segments.") csdoc-opcode-database)
(puthash "midichn" '(:template "ichn midichn" :doc "Returns the MIDI channel number from which the note was activated.") csdoc-opcode-database)
(puthash "pyexec" '(:template "pyexec "filename"
pyexeci "filename"
pylexec "filename"
pylexeci "filename"
pyexect ktrigger, "filename"
plyexect ktrigger, "filename"" :doc "Execute a script from a file at k-time or i-time (i suffix).") csdoc-opcode-database)
(puthash "distort" '(:template "ar distort asig, kdist, ifn[, ihp, istor]" :doc "Distort an audio signal via waveshaping and optional clipping.") csdoc-opcode-database)
(puthash "cosinv" '(:template "cosinv(x) (no rate restriction)
cosinv(k/i[]) (k- or i-arrays )" :doc "Performs a arccosine function.") csdoc-opcode-database)
(puthash "inrg" '(:template "inrg kstart, ain1 [,ain2, ain3, ..., ainN]" :doc "Allow input from a range of adjacent audio channels from the audio input device") csdoc-opcode-database)
(puthash "ftsr" '(:template "ftsr(x) (init-rate args only)" :doc "Returns the sampling-rate of a stored function table.") csdoc-opcode-database)
(puthash "pvread" '(:template "kfreq, kamp pvread ktimpnt, ifile, ibin" :doc "Reads from a phase vocoder analysis file and returns the frequency and amplitude from a single analysis channel or bin.") csdoc-opcode-database)
(puthash "randi" '(:template "ares randi xamp, xcps [, iseed] [, isize] [, ioffset]
kres randi kamp, kcps [, iseed] [, isize] [, ioffset]" :doc "Generates a controlled random number series with interpolation between each new number.") csdoc-opcode-database)
(puthash "vpowv_i" '(:template "vpowv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs power-of operations between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "sprintf" '(:template "Sdst sprintf Sfmt, xarg1[, xarg2[, ... ]]" :doc "printf-style formatted output to a string variable.") csdoc-opcode-database)
(puthash "vbapz" '(:template "vbapz inumchnls, istartndx, asig, kazim [, kelev] [, kspread]" :doc "Writes a multi-channel audio signal to a ZAK array.") csdoc-opcode-database)
(puthash "STKStifKarp" '(:template "asignal STKStifKarp ifrequency, iamplitude, [kpos, kv1[, ksus, kv2[, kstretch, kv3]]]" :doc "STKStifKarp is a plucked stiff string instrument.") csdoc-opcode-database)
(puthash "deltap" '(:template "ares deltap kdlt" :doc "Taps a delay line at variable offset times.") csdoc-opcode-database)
(puthash "jspline" '(:template "ares jspline xamp, kcpsMin, kcpsMax
kres jspline kamp, kcpsMin, kcpsMax" :doc "A jitter-spline generator.") csdoc-opcode-database)
(puthash "monitor" '(:template "aout1 [,aout2 ... aoutX] monitor
aarra monitor" :doc "Returns the audio spout frame.") csdoc-opcode-database)
(puthash "tigoto" '(:template "tigoto label" :doc "Transfer control at i-time when a new note is being tied onto a previously held note") csdoc-opcode-database)
(puthash "phs" '(:template "kout[] phs kin[]" :doc "Obtains the phases of a complex-number array") csdoc-opcode-database)
(puthash "cpsmidib" '(:template "icps cpsmidib [irange]
kcps cpsmidib [irange]" :doc "Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in cycles-per-second.") csdoc-opcode-database)
(puthash "areson" '(:template "ares areson asig, kcf, kbw [, iscl] [, iskip]
ares areson asig, acf, kbw [, iscl] [, iskip]
ares areson asig, kcf, abw [, iscl] [, iskip]
ares areson asig, acf, abw [, iscl] [, iskip]" :doc "A notch filter whose transfer functions are the complements of
      the reson opcode.") csdoc-opcode-database)
(puthash "out32" '(:template "out32 asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, asig10, asig11, asig12, asig13, asig14, asig15, asig16, asig17, asig18, asig19, asig20, asig21, asig22, asig23, asig24, asig25, asig26, asig27, asig28, asig29, asig30, asig31, asig32" :doc "Writes 32-channel audio data to an external device or stream.") csdoc-opcode-database)
(puthash "vport" '(:template "vport ifn, khtime, ielements [, ifnInit]" :doc "Vectorial Control-rate Delay Paths") csdoc-opcode-database)
(puthash "pvsdemix" '(:template "fsig pvsdemix fleft, fright, kpos, kwidth, ipoints" :doc "Spectral azimuth-based de-mixing of stereo sources.") csdoc-opcode-database)
(puthash "gendyc" '(:template "ares gendyc kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]
kres gendyc kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]" :doc "Dynamic stochastic approach to waveform synthesis using cubic interpolation.") csdoc-opcode-database)
(puthash "inq" '(:template "ar1, ar2, ar3, a4 inq" :doc "Reads quad audio data from an external device or stream.") csdoc-opcode-database)
(puthash "sc_trig" '(:template "aout sc_trig ain, kdur
kout sc_trig kin, kdur" :doc "Timed trigger") csdoc-opcode-database)
(puthash "vdelayx" '(:template "aout vdelayx ain, adl, imd, iws [, ist]" :doc "A variable delay opcode with high quality interpolation.") csdoc-opcode-database)
(puthash "lposcilsa" '(:template "ar1, ar2 lposcilsa aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "Read stereo sampled sound from a table with looping and high precision.") csdoc-opcode-database)
(puthash "tablexkt" '(:template "ares tablexkt xndx, kfn, kwarp, iwsize [, ixmode] [, ixoff] [, iwrap]" :doc "Reads function tables with linear, cubic, or sinc interpolation.") csdoc-opcode-database)
(puthash "FLsetsnap" '(:template "inumsnap, inumval FLsetsnap index [, ifn, igroup]" :doc "Stores the current status of all FLTK valuators into a snapshot location.") csdoc-opcode-database)
(puthash "outletf" '(:template "outletf Sname, fsignal" :doc "Sends a frate signal (fsig) out from an instrument to a named port.") csdoc-opcode-database)
(puthash "scanu" '(:template "scanu init, irate, ifnvel, ifnmass, ifnstif, ifncentr, ifndamp, kmass, kstif, kcentr, kdamp, ileft, iright, kpos, kstrngth, ain, idisp, id" :doc "Compute the waveform and the wavetable for use in scanned synthesis.") csdoc-opcode-database)
(puthash "sfinstr3" '(:template "ar1, ar2 sfinstr3 ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound with cubic interpolation.") csdoc-opcode-database)
(puthash "dbamp" '(:template "dbamp(x) (init-rate or control-rate args only)" :doc "Returns the decibel equivalent of the raw amplitude") csdoc-opcode-database)
(puthash "slider8table" '(:template "kflag slider8table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum8, imin8, imax8, init8, ifn8" :doc "Stores a bank of 8 different MIDI control messages to a table.") csdoc-opcode-database)
(puthash "STKFMVoices" '(:template "asignal STKFMVoices ifrequency, iamplitude, [kvowel, kv1[, kspec, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STKFMVoices is a singing FM synthesis instrument.") csdoc-opcode-database)
(puthash "nlfilt2" '(:template "ares nlfilt2 ain, ka, kb, kd, kC, kL" :doc "A filter with a non-linear effect and blowup protection.") csdoc-opcode-database)
(puthash "sum" '(:template "ares sum asig1 [, asig2] [, asig3] [...]
kres sum karr
ires sum iarr" :doc "Sums any number of a-rate signals, or array elements.") csdoc-opcode-database)
(puthash "polynomial" '(:template "aout polynomial ain, k0 [, k1 [, k2 [...]]]" :doc "Efficiently evaluates a polynomial of arbitrary order.") csdoc-opcode-database)
(puthash "resonk" '(:template "kres resonk ksig, kcf, kbw [, iscl] [, iskip]" :doc "A second-order resonant filter.") csdoc-opcode-database)
(puthash "rms" '(:template "kres rms asig [, ihp] [, iskip]" :doc "Determines the root-mean-square amplitude of an audio signal.") csdoc-opcode-database)
(puthash "loscil" '(:template "ar1 [,ar2] loscil xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]
aph, ar1 [,ar2] loscilphs xamp, kcps, ifn [, ibas] [, imod1] [, ibeg1] [, iend1] [, imod2] [, ibeg2] [, iend2]" :doc "Read sampled sound from a table.") csdoc-opcode-database)
(puthash "phaser1" '(:template "ares phaser1 asig, kfreq, kord, kfeedback [, iskip]" :doc "First-order allpass filters arranged in a series.") csdoc-opcode-database)
(puthash "fmb3" '(:template "ares fmb3 kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn]" :doc "Uses FM synthesis to create a Hammond B3 organ sound.") csdoc-opcode-database)
(puthash "ziw" '(:template "ziw isig, indx" :doc "Writes to a zk variable at i-rate without mixing.") csdoc-opcode-database)
(puthash "moog" '(:template "ares moog kamp, kfreq, kfiltq, kfiltrate, kvibf, kvamp, iafn, iwfn, ivfn" :doc "An emulation of a mini-Moog synthesizer.") csdoc-opcode-database)
(puthash "octmidi" '(:template "ioct octmidi" :doc "Get the note number, in octave-point-decimal units, of the current MIDI event.") csdoc-opcode-database)
(puthash "exprandi" '(:template "ares exprandi klambda, xamp, xcps
ires exprandi klambda, xamp, xcps
kres exprandi klambda, xamp, xcps" :doc "WTF: ") csdoc-opcode-database)
(puthash "delayr" '(:template "ares delayr idlt [, iskip]" :doc "Reads from an automatically established digital delay line.") csdoc-opcode-database)
(puthash "rifft" '(:template "kout[] rifft kin[]" :doc "Complex-to-real Inverse Fast Fourier Transform.") csdoc-opcode-database)
(puthash "zkcl" '(:template "zkcl kfirst, klast" :doc "Clears one or more variables in the zk space.") csdoc-opcode-database)
(puthash "max" '(:template "amax max ain1, ain2 [, ain3] [, ain4] [...]
kmax max kin1, kin2 [, kin3] [, kin4] [...]
imax max iin1, iin2 [, iin3] [, iin4] [...]" :doc "Produces a signal that is the maximum of any number of input signals.") csdoc-opcode-database)
(puthash "tablew" '(:template "tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "Change the contents of existing function tables.") csdoc-opcode-database)
(puthash "mpulse" '(:template "ares mpulse kamp, kintvl [, ioffset]" :doc "Generates a set of impulses.") csdoc-opcode-database)
(puthash "vco2" '(:template "ares vco2 kamp, kcps [, imode] [, kpw] [, kphs] [, inyx]" :doc "Implementation of a band-limited oscillator using pre-calculated tables.") csdoc-opcode-database)
(puthash "vibr" '(:template "kout vibr kAverageAmp, kAverageFreq, ifn" :doc "Easier-to-use user-controllable vibrato.") csdoc-opcode-database)
(puthash "reverb" '(:template "ares reverb asig, krvt [, iskip]" :doc "Reverberates an input signal with a") csdoc-opcode-database)
(puthash "fmmetal" '(:template "ares fmmetal kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "Uses FM synthesis to create a") csdoc-opcode-database)
(puthash "loop_le" '(:template "loop_le indx, incr, imax, label
loop_le kndx, kncr, kmax, label" :doc "Looping constructions.") csdoc-opcode-database)
(puthash "combinv" '(:template "ares combinv asig, krvt, ilpt [, iskip] [, insmps]" :doc "Reverberates an input signal with a") csdoc-opcode-database)
(puthash "outipb" '(:template "outipb ichn, ivalue, imin, imax" :doc "Sends MIDI pitch-bend messages at i-rate.") csdoc-opcode-database)
(puthash "jitter2" '(:template "kout jitter2 ktotamp, kamp1, kcps1, kamp2, kcps2, kamp3, kcps3[ , iopt]" :doc "Generates a segmented line with user-controllable random segments.") csdoc-opcode-database)
(puthash "midremot" '(:template "midremot idestination, isource, instrnum [,instrnum...]" :doc "An opcode which can be used to implement a remote midi orchestra. This opcode will send midi events from a source machine to one destination.") csdoc-opcode-database)
(puthash "readk3" '(:template "kr1, kr2, kr3 readk3 ifilname, iformat, iprd" :doc "Periodically reads three orchestra control-signal values from an external file.") csdoc-opcode-database)
(puthash "vtabi" '(:template "vtabi indx, ifn, iout1 [, iout2, iout3, .... , ioutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "sfinstr" '(:template "ar1, ar2 sfinstr ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "Plays a SoundFont2 (SF2) sample instrument, generating a stereo sound.") csdoc-opcode-database)
(puthash "sliderKawai" '(:template "k1, k2, ...., k16 sliderKawai imin1, imax1, init1, ifn1, imin2, imax2, init2, ifn2, ..., imin16, imax16, init16, ifn16" :doc "Creates a bank of 16 different MIDI control message numbers from a KAWAI MM-16 midi mixer.") csdoc-opcode-database)
(puthash "strtod" '(:template "ir strtod Sstr
ir strtod indx" :doc "Converts a string to a float (i-rate).") csdoc-opcode-database)
(puthash "trshift" '(:template "fsig trshift fin, kpshift[, kgain]" :doc "Streaming partial track frequency scaling.") csdoc-opcode-database)
(puthash "ptablei" '(:template "ares ptablei andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptablei indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptablei kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing with linear interpolation.") csdoc-opcode-database)
(puthash "harmon2" '(:template "ares harmon2 asig, koct, kfrq1, kfrq2, icpsmode, ilowest[, ipolarity]
ares harmon3 asig, koct, kfrq1, kfrq2, kfrq3, icpsmode, ilowest[, ipolarity]
ares harmon4 asig, koct, kfrq1, kfrq2, kfrq3, kfrq4, icpsmode, ilowest[, ipolarity]" :doc "Analyze an audio input and generate harmonizing voices in
      synchrony with formants preserved.") csdoc-opcode-database)
(puthash "partials" '(:template "ftrks partials ffr, fphs, kthresh, kminpts, kmaxgap, imaxtracks" :doc "Partial track spectral analysis.") csdoc-opcode-database)
(puthash "readscratch" '(:template "ival readscratch[index]" :doc "returns a value stored in the instance of an instrument.") csdoc-opcode-database)
(puthash "midiarp" '(:template "kMidiNoteNum, kCountermidiarp kRate[, kMode]" :doc "Generates arpeggios based on currently held MIDI notes.") csdoc-opcode-database)
(puthash "pvsarp" '(:template "fsig pvsarp fsigin, kbin, kdepth, kgain" :doc "Arpeggiate the spectral components of a streaming pv signal.") csdoc-opcode-database)
(puthash "joystick" '(:template "kres joystick kdevice ktab" :doc "Reads data from a joystick controller.") csdoc-opcode-database)
(puthash "pconvolve" '(:template "ar1 [, ar2] [, ar3] [, ar4] pconvolve ain, ifilcod [, ipartitionsize, ichannel]" :doc "Convolution based on a uniformly partitioned overlap-save algorithm") csdoc-opcode-database)
(puthash "pcount" '(:template "icount pcount" :doc "Returns the number of pfields belonging to a note event.") csdoc-opcode-database)
(puthash "space" '(:template "a1, a2, a3, a4 space asig, ifn, ktime, kreverbsend, kx, ky" :doc "Distributes an input signal among 4 channels using cartesian coordinates.") csdoc-opcode-database)
(puthash "ampdb" '(:template "ampdb(x) (no rate restriction)" :doc "Returns the amplitude equivalent of the decibel value x.") csdoc-opcode-database)
(puthash "vdelayk" '(:template "kout vdelayk ksig, kdel, imaxdel [, iskip, imode]" :doc "k-rate variable time delay.") csdoc-opcode-database)
(puthash "cosh" '(:template "cosh(x) (no rate restriction)
cosh(k/i[]) (k- or i-arrays )" :doc "Performs a hyperbolic cosine function.") csdoc-opcode-database)
(puthash "getrow" '(:template "i/kout[] getrowi/kin[],i/krow" :doc "Gets a given row from a 2-dimensional array as a vector.") csdoc-opcode-database)
(puthash "ATSinterpread" '(:template "kamp ATSinterpread kfreq" :doc "allows a user to determine the frequency envelope of any") csdoc-opcode-database)
(puthash "log10" '(:template "log10(x) (no rate restriction)
log10(k/i[]) (k- or i-arrays )" :doc "Returns a base 10 log.") csdoc-opcode-database)
(puthash "vdelay3" '(:template "ares vdelay3 asig, adel, imaxdel [, iskip]" :doc "A variable time delay with cubic interpolation.") csdoc-opcode-database)
(puthash "cuserrnd" '(:template "aout cuserrnd kmin, kmax, ktableNum
iout cuserrnd imin, imax, itableNum
kout cuserrnd kmin, kmax, ktableNum" :doc "Continuous USER-defined-distribution RaNDom generator.") csdoc-opcode-database)
(puthash "fareyleni" '(:template "ifl fareyleni ifn" :doc "returns the length of a Farey Sequence.") csdoc-opcode-database)
(puthash "opnonequiv" '(:template "a # b (bitwise NON EQUIVALENCE)" :doc "Bitwise NON EQUIVALENCE operator.") csdoc-opcode-database)
(puthash "STKBlowHole" '(:template "asignal STKBlowHole ifrequency, iamplitude, [kreed, kv1[, knoise, kv2[, khole, kv3[, kreg, kv4[, kbreath, kv5]]]]]" :doc "STK clarinet physical model with one register hole and one tonehole.") csdoc-opcode-database)
(puthash "serialBegin" '(:template "iPort serialBegin SPortName [, ibaudRate]" :doc "Open a serial port.") csdoc-opcode-database)
(puthash "trfilter" '(:template "fsig trfilter fin, kamnt, ifn" :doc "Streaming partial track filtering.") csdoc-opcode-database)
(puthash "faustaudio" '(:template "ihandle,a1[,a2,...] faustaudio ifac[,ain1,...]" :doc "Instantiates and runs a compiled Faust program.") csdoc-opcode-database)
(puthash "pvshift" '(:template "fsig pvshift fsigin, kshift, klowest[, kkeepform, igain, kcoefs]" :doc "Shift the frequency components of a pv stream, stretching/compressing
      its spectrum.") csdoc-opcode-database)
(puthash "mton" '(:template "Snote mton kmidi
Snote mton imidi" :doc "Convert midi note number to string note name") csdoc-opcode-database)
(puthash "cpstun" '(:template "kcps cpstun ktrig, kindex, kfn" :doc "Returns micro-tuning values at k-rate.") csdoc-opcode-database)
(puthash "checkbox" '(:template "kres checkbox knum" :doc "Sense on-screen controls.") csdoc-opcode-database)
(puthash "tablegpw" '(:template "tablegpw kfn" :doc "Writes a table's guard point.") csdoc-opcode-database)
(puthash "chnset" '(:template "chnset ival, Sname
chnset kval, Sname
chnset aval, Sname
chnset Sval, Sname
chnsetks Sval, Sname" :doc "Writes data to the named software bus.") csdoc-opcode-database)
(puthash "vtabwk" '(:template "vtabwk kndx, ifn, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "STKWhistle" '(:template "asignal STKWhistle ifrequency, iamplitude, [kmod, kv1[, knoise, kv2[, kfipfreq, kv3[, kfipgain, kv4[, kvol, kv5]]]]]" :doc "STKWhistle produces whistle sounds.") csdoc-opcode-database)
(puthash "spat3di" '(:template "aW, aX, aY, aZ spat3di ain, iX, iY, iZ, idist, ift, imode [, istor]" :doc "Positions the input sound in a 3D space with the sound source position set at i-time.") csdoc-opcode-database)
(puthash "cpumeter" '(:template "ktot[,kcpu1, kcpu2,...]cpumeter ifreq" :doc "Reports the usage of cpu either total or per core.") csdoc-opcode-database)
(puthash "cell" '(:template "cell ktrig, kreinit, ioutFunc, initStateFunc, iRuleFunc, ielements" :doc "Cellular Automaton") csdoc-opcode-database)
(puthash "ctrl21" '(:template "idest ctrl21 ichan, ictlno1, ictlno2, ictlno3, imin, imax [, ifn]
kdest ctrl21 ichan, ictlno1, ictlno2, ictlno3, kmin, kmax [, ifn]" :doc "Allows a floating-point 21-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "cpstmid" '(:template "icps cpstmid ifn" :doc "Get a MIDI note number (allows customized micro-tuning scales).") csdoc-opcode-database)
(puthash "oscbnk" '(:template "ares oscbnk kcps, kamd, kfmd, kpmd, iovrlap, iseed, kl1minf, kl1maxf, kl2minf, kl2maxf, ilfomode, keqminf, keqmaxf, keqminl, keqmaxl, keqminq, keqmaxq, ieqmode, kfn [, il1fn] [, il2fn] [, ieqffn] [, ieqlfn] [, ieqqfn] [, itabl] [, ioutfn]" :doc "Mixes the output of any number of oscillators.") csdoc-opcode-database)
(puthash "outo" '(:template "outo asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8" :doc "Writes 8-channel audio data to an external device or stream.") csdoc-opcode-database)
(puthash "directory" '(:template "SFiles[] directory SDirectory[, SExtention]" :doc "Reads a directory and outputs to a string array a list of file names.") csdoc-opcode-database)
(puthash "polyaft" '(:template "ires polyaft inote [, ilow] [, ihigh]
kres polyaft inote [, ilow] [, ihigh]" :doc "Returns the polyphonic after-touch pressure of the selected note number.") csdoc-opcode-database)
(puthash "ampmidid" '(:template "iamplitude ampmidid ivelocity, idecibels
kamplitude ampmidid kvelocity, idecibels" :doc "Musically map MIDI velocity to peak amplitude within a specified dynamic range in decibels.") csdoc-opcode-database)
(puthash "centroid" '(:template "kcent centroid asig, ktrig, ifftsize" :doc "Calculate the spectral centroid of a signal.") csdoc-opcode-database)
(puthash "FLslidBnk2Set" '(:template "FLslidBnk2Set ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "modify the values of a slider bank.") csdoc-opcode-database)
(puthash "changed2" '(:template "ktrig changed2 kvar1 [, kvar2,..., kvarN]
ktrig changed2 karr[]
ktrig changed2 aarr[]" :doc "k-rate signal change detector.") csdoc-opcode-database)
(puthash "FLshow" '(:template "FLshow ihandle" :doc "Restores the visibility of a previously hidden FLTK widget.") csdoc-opcode-database)
(puthash "faustplay" '(:template "a1[, a2,...] faustplay ihandle[, ain1,...]" :doc "Runs an instantiated Faust program.") csdoc-opcode-database)
(puthash "strtodk" '(:template "kr strtodk Sstr
kr strtodk kndx" :doc "Converts a string to a float (k-rate).") csdoc-opcode-database)
(puthash "clockoff" '(:template "clockoff inum" :doc "Stops one of a number of internal clocks.") csdoc-opcode-database)
(puthash "slider32table" '(:template "kflag slider32table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum32, imin32, imax32, init32, ifn32" :doc "Stores a bank of 32 different MIDI control messages to a table.") csdoc-opcode-database)
(puthash "lfo" '(:template "kres lfo kamp, kcps [, itype]
ares lfo kamp, kcps [, itype]" :doc "A low frequency oscillator of various shapes.") csdoc-opcode-database)
(puthash "strlower" '(:template "Sdst strlower Ssrc" :doc "Convert a string to lower case") csdoc-opcode-database)
(puthash "FLsetBox" '(:template "FLsetBox itype, ihandle" :doc "Sets the appearance of a box surrounding a FLTK widget.") csdoc-opcode-database)
(puthash "vdivv" '(:template "vdivv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs division between two vectorial control signals") csdoc-opcode-database)
(puthash "insremot" '(:template "insremot idestination, isource, instrnum [,instrnum...]" :doc "An opcode which can be used to implement a remote
      orchestra. This opcode will send note events from a source
      machine to one destination.") csdoc-opcode-database)
(puthash "chano" '(:template "chano kval, kchan
chano aval, kchan" :doc "Send data to the outwards software bus") csdoc-opcode-database)
(puthash "fmanal" '(:template "am, af fmanal are, aim" :doc "AM/FM analysis from quadrature signal.") csdoc-opcode-database)
(puthash "link_tempo_get" '(:template "k_bpm link_tempo_get i_peer" :doc "Returns the current tempo of the global network Ableton Link session.") csdoc-opcode-database)
(puthash "sinsyn" '(:template "asig sinsyn fin, kscal, kmaxtracks, ifn" :doc "Streaming partial track additive synthesis with cubic phase interpolation") csdoc-opcode-database)
(puthash "print" '(:template "print iarg [, iarg1] [, iarg2] [...]" :doc "Displays the values init (i-rate) variables.") csdoc-opcode-database)
(puthash "STKBowed" '(:template "asignal STKBowed ifrequency, iamplitude, [kpress, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kvol, kv5]]]]]" :doc "STKBowed is a bowed string instrument.") csdoc-opcode-database)
(puthash "cos" '(:template "cos(x) (no rate restriction)
cos(k/i[]) (k- or i-arrays )" :doc "Performs a cosine function.") csdoc-opcode-database)
(puthash "ftom" '(:template "imidi ftom ifreq [,irnd]
kmidi ftom kfreq [,irnd]
imidis[] ftom ifreqs[] [,irnd]
kmidis[] ftom kfreqs[] [,irnd]" :doc "Convert frequency to midi") csdoc-opcode-database)
(puthash "fftinv" '(:template "kout[] fftinv kin[]" :doc "Complex-to-complex Inverse Fast Fourier Transform.") csdoc-opcode-database)
(puthash "tablera" '(:template "ares tablera kfn, kstart, koff" :doc "Reads tables in sequential locations.") csdoc-opcode-database)
(puthash "lorismorph" '(:template "lorismorph isrcidx, itgtidx, istoreidx, kfreqmorphenv, kampmorphenv, kbwmorphenv" :doc "Morphs two stored sets of bandwidth-enhanced partials
    and stores a new set of partials representing the morphed
    sound. The morph is performed by linearly interpolating the
    parameter envelopes (frequency, amplitude, and bandwidth, or
    noisiness) of the bandwidth-enhanced partials according to
    control-rate frequency, amplitude, and bandwidth morphing
    functions.") csdoc-opcode-database)
(puthash "mdelay" '(:template "mdelay kstatus, kchan, kd1, kd2, kdelay" :doc "A MIDI delay opcode.") csdoc-opcode-database)
(puthash "spat3dt" '(:template "spat3dt ioutft, iX, iY, iZ, idist, ift, imode, irlen [, iftnocl]" :doc "Can be used to render an impulse response for a 3D space at i-time.") csdoc-opcode-database)
(puthash "vaset" '(:template "vaset kval, kndx, avar" :doc "Write value of into the current buffer of an a-rate variable by index.") csdoc-opcode-database)
(puthash "link_is_enabled" '(:template "k_is_enabled link_is_enabled i_peer" :doc "Returns whether or not this peer is synchronized with the global network Ableton Link session.") csdoc-opcode-database)
(puthash "filescal" '(:template "asig[,asig2] filescal ktimescal, kamp, kpitch, Sfile, klock [,ifftsize, idecim, ithresh]" :doc "Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.") csdoc-opcode-database)
(puthash "line" '(:template "ares line ia, idur, ib
kres line ia, idur, ib" :doc "Trace a straight line between specified points.") csdoc-opcode-database)
(puthash "filevalid" '(:template "ir filevalid ifilcod" :doc "Checks that a file can be read.") csdoc-opcode-database)
(puthash "loop_lt" '(:template "loop_lt indx, incr, imax, label
loop_lt kndx, kncr, kmax, label" :doc "Looping constructions.") csdoc-opcode-database)
(puthash "vtabwa" '(:template "vtabwa andx, ifn, ainarg1 [, ainarg2, ainarg3 , .... , ainargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "partikkelget" '(:template "kindex partikkelget kparameterindex, iopcode_id" :doc "Get mask index for a specific mask parameter of a running") csdoc-opcode-database)
(puthash "midinoteonpch" '(:template "midinoteonpch xpch, xvelocity" :doc "Gets a MIDI note number as a pitch-class value.") csdoc-opcode-database)
(puthash "FLsetTextSize" '(:template "FLsetTextSize isize, ihandle" :doc "Sets the size of the text label of a FLTK widget.") csdoc-opcode-database)
(puthash "vdelayxw" '(:template "aout vdelayxw ain, adl, imd, iws [, ist]" :doc "Variable delay opcodes with high quality interpolation.") csdoc-opcode-database)
(puthash "granule" '(:template "ares granule xamp, ivoice, iratio, imode, ithd, ifn, ipshift, igskip, igskip_os, ilength, kgap, igap_os, kgsize, igsize_os, iatt, idec [, iseed] [, ipitch1] [, ipitch2] [, ipitch3] [, ipitch4] [, ifnenv]" :doc "A more complex granular synthesis texture generator.") csdoc-opcode-database)
(puthash "vco2init" '(:template "ifn vco2init iwave [, ibasfn] [, ipmul] [, iminsiz] [, imaxsiz] [, isrcft]" :doc "Calculates tables for use by vco2 opcode.") csdoc-opcode-database)
(puthash "vsubv_i" '(:template "vsubv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs subtraction between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "gendyx" '(:template "ares gendyx kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]
kres gendyx kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl, kcurveup, kcurvedown [, initcps] [, knum]" :doc "Variation of the dynamic stochastic approach to waveform
      synthesis conceived by Iannis Xenakis.") csdoc-opcode-database)
(puthash "ampmidi" '(:template "iamp ampmidi iscal [, ifn]" :doc "Get the velocity of the current MIDI event.") csdoc-opcode-database)
(puthash "cpsmidi" '(:template "icps cpsmidi" :doc "Get the note number of the current MIDI event, expressed in cycles-per-second.") csdoc-opcode-database)
(puthash "mediank" '(:template "kres mediank kin, ksize, imaxsize [, iskip]" :doc "A median filter, a variant FIR lowpass filter.") csdoc-opcode-database)
(puthash "FLsetAlign" '(:template "FLsetAlign ialign, ihandle" :doc "Sets the text alignment of a label of a FLTK widget.") csdoc-opcode-database)
(puthash "mac" '(:template "ares mac ksig1, asig1 [, ksig2] [, asig2] [, ksig3] [, asig3] [...]" :doc "Multiplies and accumulates a- and k-rate signals.") csdoc-opcode-database)
(puthash "vbaplsinit" '(:template "vbaplsinit idim, ilsnum [, idir1] [, idir2] [...] [, idir32]
vbaplsinit idim, ilsnum, ilsarray" :doc "Configures VBAP output according to loudspeaker parameters.") csdoc-opcode-database)
(puthash "JackoAudioInConnect" '(:template "JackoAudioInConnect SexternalPortName, ScsoundPortName" :doc "Creates an audio connection from a Jack port to Csound.") csdoc-opcode-database)
(puthash "STKSitar" '(:template "asignal STKSitar ifrequency, iamplitude" :doc "STKSitar uses a plucked string physical model.") csdoc-opcode-database)
(puthash "pvsinit" '(:template "fsig pvsinit isize[, iolap, iwinsize, iwintype, iformat]" :doc "Initialise a spectral (f) variable to zero.") csdoc-opcode-database)
(puthash "fluidAllOut" '(:template "aleft, aright fluidAllOut" :doc "Collects all audio from all Fluidsynth engines in a performance") csdoc-opcode-database)
(puthash "foutir" '(:template "foutir ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]" :doc "Outputs i-rate signals from an arbitrary number of channels to a specified file.") csdoc-opcode-database)
(puthash "tableng" '(:template "ires tableng ifn
kres tableng kfn" :doc "Interrogates a function table for length.") csdoc-opcode-database)
(puthash "clear" '(:template "clear avar1 [, avar2] [, avar3] [...]" :doc "Zeroes a list of audio signals.") csdoc-opcode-database)
(puthash "specptrk" '(:template "koct, kamp specptrk wsig, kvar, ilo, ihi, istr, idbthresh, inptls, irolloff [, iodd] [, iconfs] [, interp] [, ifprd] [, iwtflg]" :doc "Estimates the pitch of the most prominent complex tone in the spectrum.") csdoc-opcode-database)
(puthash "exitnow" '(:template "exitnow [ivalue]" :doc "Exit Csound as fast as possible, with no cleaning up.") csdoc-opcode-database)
(puthash "random" '(:template "ares random kmin, kmax
ires random imin, imax
kres random kmin, kmax" :doc "Generates a controlled pseudo-random number series between min and max values.") csdoc-opcode-database)
(puthash "diode_ladder" '(:template "asig diode_ladder ain, xcf, xk [, inlp, isaturation, istor]" :doc "Zero-delay feedback implementation of 4 pole diode ladder filter.") csdoc-opcode-database)
(puthash "hilbert" '(:template "ar1, ar2 hilbert asig" :doc "A Hilbert transformer.") csdoc-opcode-database)
(puthash "pindex" '(:template "ivalue pindex ipfieldIndex" :doc "Returns the value of a specified pfield.") csdoc-opcode-database)
(puthash "xscanu" '(:template "xscanu init, irate, ifnvel, ifnmass, ifnstif, ifncentr, ifndamp, kmass, kstif, kcentr, kdamp, ileft, iright, kpos, kstrngth, ain, idisp, id" :doc "Compute the waveform and the wavetable for use in scanned synthesis.") csdoc-opcode-database)
(puthash "inletv" '(:template "array inletv Sname" :doc "Receives an arate array signal into an instrument through a named port.") csdoc-opcode-database)
(puthash "plusbecomes" '(:template "ares += xarg
ires += iarg
kres += karg
table [ kval] += karg" :doc "Performs add and assignment.") csdoc-opcode-database)
(puthash "sfinstrm" '(:template "ares sfinstrm ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "Plays a SoundFont2 (SF2) sample instrument, generating a mono sound.") csdoc-opcode-database)
(puthash "pvstencil" '(:template "fsig pvstencil fsigin, kgain, klevel, iftable" :doc "Transforms a pvoc stream according to a masking function table.") csdoc-opcode-database)
(puthash "poisson" '(:template "ares poisson klambda
ires poisson klambda
kres poisson klambda" :doc "WTF: ") csdoc-opcode-database)
(puthash "convolve" '(:template "ar1 [, ar2] [, ar3] [, ar4] convolve ain, ifilcod [, ichannel]" :doc "Convolves a signal and an impulse response.") csdoc-opcode-database)
(puthash "reinit" '(:template "reinit label" :doc "Suspends a performance while a special initialization pass is executed.") csdoc-opcode-database)
(puthash "zarg" '(:template "ares zarg kndx, kgain" :doc "Reads from a location in za space at a-rate, adds some gain.") csdoc-opcode-database)
(puthash "reverbsc" '(:template "aoutL, aoutR reverbsc ainL, ainR, kfblvl, kfco[, israte[, ipitchm[, iskip]]]" :doc "8 delay line stereo FDN reverb, based on work by Sean Costello") csdoc-opcode-database)
(puthash "pvsbandr" '(:template "fsig pvsbandr fsigin, xlowcut, xlowfull, xhighfull, xhighcut[, ktype]" :doc "A band reject filter working in the spectral domain.") csdoc-opcode-database)
(puthash "eqfil" '(:template "asig eqfil ain, kcf, kbw, kgain[, istor]" :doc "Equalizer filter") csdoc-opcode-database)
(puthash "xscanmap" '(:template "kpos, kvel xscanmap iscan, kamp, kvamp [, iwhich]" :doc "Allows the position and velocity of a node in a scanned process to be read.") csdoc-opcode-database)
(puthash "vphaseseg" '(:template "vphaseseg kphase, ioutab, ielems, itab1,idist1,itab2 [,idist2,itab3, ... ,idistN-1,itabN]" :doc "Allows one-dimensional HVS (Hyper-Vectorial Synthesis).") csdoc-opcode-database)
(puthash "stack" '(:template "stack iStackSize" :doc "WTF: ") csdoc-opcode-database)
(puthash "linen" '(:template "ares linen xamp, irise, idur, idec
kres linen kamp, irise, idur, idec" :doc "Applies a straight line rise and decay pattern to an input amp signal.") csdoc-opcode-database)
(puthash "STKBeeThree" '(:template "asignal STKBeeThree ifrequency, iamplitude, [kop4, kv1[, kop3, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STK Hammond-oid organ-like FM synthesis instrument.") csdoc-opcode-database)
(puthash "zdf_2pole_mode" '(:template "alp, abp, ahp zdf_2pole_mode ain, xcf, Q [, istor]" :doc "Zero-delay feedback implementation of 2 pole filter with multimode output.") csdoc-opcode-database)
(puthash "pvsbandp" '(:template "fsig pvsbandp fsigin, xlowcut, xlowfull, xhighfull, xhighcut[, ktype]" :doc "A band pass filter working in the spectral domain.") csdoc-opcode-database)
(puthash "pvsftr" '(:template "pvsftr fsrc, ifna [, ifnf]" :doc "Reads amplitude and/or frequency data from function tables.") csdoc-opcode-database)
(puthash "hsboscil" '(:template "ares hsboscil kamp, ktone, kbrite, ibasfreq, iwfn, ioctfn [, ioctcnt] [, iphs]" :doc "An oscillator which takes tonality and brightness as arguments.") csdoc-opcode-database)
(puthash "kgoto" '(:template "kgoto label" :doc "Transfer control during the performance-time passes.") csdoc-opcode-database)
(puthash "bpf" '(:template "ky bpf kx, kx1, ky1, kx2, ..., kxn, kyn
iy bpf ix, ix1, iy1, ix2, ..., ixn, iyn
kys[] bpf kxs[], kx1, ky1, kx2, ..., kxn, kyn
iys[] bpf ixs[], ix1, iy1, ix2, ..., ixn, iyn
ky bpf kx, kxs[], kys[]
iy bpf ix, ixs[], iys[]
ay bpf ax, kx1, ky1, kx2, ..., kxn, kyn
ay bpf ax, kxs[], kys[]
ky, kw bpf kx, kxs[], kys[], kws[]" :doc "Break point function with linear interpolation") csdoc-opcode-database)
(puthash "looptseg" '(:template "ksig looptseg kfreq, ktrig, iphase, kvalue0, ktype0, ktime0, [, kvalue1] [,ktype1] [, ktime1] [, kvalue2] [,ktype2] [, ktime2] [...] [, kvalueN] [,ktypeN] [, ktimeN]" :doc "Generate control signal consisting of exponential or linear segments delimited by two or more specified points.") csdoc-opcode-database)
(puthash "lpreson" '(:template "ares lpreson asig" :doc "Resynthesises a signal from the data passed internally by a previous lpread.") csdoc-opcode-database)
(puthash "chnclear" '(:template "chnclear Sname1[, Sname2,...]" :doc "Clears a number of audio output channel of the named software bus.") csdoc-opcode-database)
(puthash "tablecopy" '(:template "tablecopy kdft, ksft" :doc "Simple, fast table copy opcode.") csdoc-opcode-database)
(puthash "lorenz" '(:template "ax, ay, az lorenz ksv, krv, kbv, kh, ix, iy, iz, iskip [, iskipinit]" :doc "Implements the Lorenz system of equations.") csdoc-opcode-database)
(puthash "ctrl14" '(:template "idest ctrl14 ichan, ictlno1, ictlno2, imin, imax [, ifn]
kdest ctrl14 ichan, ictlno1, ictlno2, kmin, kmax [, ifn]" :doc "Allows a floating-point 14-bit MIDI signal scaled with a minimum and a maximum range.") csdoc-opcode-database)
(puthash "filepeak" '(:template "ir filepeak ifilcod [, ichnl]" :doc "Returns the peak absolute value of a sound file.") csdoc-opcode-database)
(puthash "divz" '(:template "ares divz xa, xb, ksubst
ires divz ia, ib, isubst
kres divz ka, kb, ksubst
...divz(ka, kb, ksubst)... (no rate restriction)" :doc "Safely divides two numbers.") csdoc-opcode-database)
(puthash "pvsmaska" '(:template "fsig pvsmaska fsrc, ifn, kdepth" :doc "Modify amplitudes using a function table, with dynamic scaling.") csdoc-opcode-database)
(puthash "vco2ift" '(:template "ifn vco2ift icps, iwave [, inyx]" :doc "Returns a table number at i-time for a given oscillator frequency and wavform.") csdoc-opcode-database)
(puthash "dssiinit" '(:template "ihandle dssiinit ilibraryname, iplugindex [, iverbose]" :doc "Loads a DSSI or LADSPA plugin.") csdoc-opcode-database)
(puthash "tbvcf" '(:template "ares tbvcf asig, xfco, xres, kdist, kasym [, iskip]" :doc "Models some of the filter characteristics of a Roland TB303 voltage-controlled filter.") csdoc-opcode-database)
(puthash "zdf_2pole" '(:template "asig zdf_2pole ain, xcf, xQ [, kmode, istor]" :doc "Zero-delay feedback implementation of 2 pole filter.") csdoc-opcode-database)
(puthash "OSCinit" '(:template "ihandle OSCinit iport" :doc "Start a listening process for OSC messages to a particular port.") csdoc-opcode-database)
(puthash "pvscent" '(:template "kcent pvscent fsig
acent pvscent fsig" :doc "Calculate the spectral centroid of a signal.") csdoc-opcode-database)
(puthash "nreverb" '(:template "ares nreverb asig, ktime, khdif [, iskip] [,inumCombs] [, ifnCombs] [, inumAlpas] [, ifnAlpas]" :doc "A reverberator consisting of 6 parallel comb-lowpass filters.") csdoc-opcode-database)
(puthash "cmp" '(:template "aout cmp a1, S_operator, a2
aout cmp a1, S_operator, kx
kOut[] cmp kA, S_operator, kB
kOut[] cmp k1, S_operator1, kIn[], S_operator2, k2" :doc "Compares audio signals or arrays") csdoc-opcode-database)
(puthash "specsum" '(:template "ksum specsum wsig [, interp]" :doc "Sums the magnitudes across all channels of the spectrum.") csdoc-opcode-database)
(puthash "pdhalf" '(:template "aout pdhalf ain, kShapeAmount [, ibipolar [, ifullscale]]" :doc "Distorts a phasor for reading the two halves of a table at different rates.") csdoc-opcode-database)
(puthash "timeinsts" '(:template "kres timeinsts" :doc "Read absolute time in seconds.") csdoc-opcode-database)
(puthash "outq3" '(:template "outq3 asig" :doc "Writes samples to quad channel 3 of an external device or stream.") csdoc-opcode-database)
(puthash "ampmidicurve" '(:template "igain ampmidicurve ivelocity, idynamicrange, iexponent
kgain ampmidicurve kvelocity, kdynamicrange, kexponent" :doc "Maps an input MIDI velocity number to an output gain factor with a
        maximum value of 1, modifying the output gain by a dynamic range and a
        shaping exponent.") csdoc-opcode-database)
(puthash "unwrap" '(:template "kout[] unwrap kin[]" :doc "Applies a unwrapping operation to an array of phase values.") csdoc-opcode-database)
(puthash "rigoto" '(:template "rigoto label" :doc "Transfers control during a reinit pass.") csdoc-opcode-database)
(puthash "pcauchy" '(:template "ares pcauchy kalpha
ires pcauchy kalpha
kres pcauchy kalpha" :doc "WTF: ") csdoc-opcode-database)
(puthash "rnd31" '(:template "ax rnd31 kscl, krpow [, iseed]
ix rnd31 iscl, irpow [, iseed]
kx rnd31 kscl, krpow [, iseed]" :doc "31-bit bipolar random opcodes with controllable distribution.") csdoc-opcode-database)
(puthash "octmidinn" '(:template "octmidinn (MidiNoteNumber) (init- or control-rate args only)" :doc "Converts a Midi note number value to octave-point-decimal.") csdoc-opcode-database)
(puthash "tableseg" '(:template "tableseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "Creates a new function table by making linear segments between values in stored function tables.") csdoc-opcode-database)
(puthash "outletk" '(:template "outletk Sname, ksignal" :doc "Sends a krate signal out from an instrument to a named port.") csdoc-opcode-database)
(puthash "p5gconnect" '(:template "p5gconnect" :doc "Reads data from a P5 Glove controller.") csdoc-opcode-database)
(puthash "passign" '(:template "ivar1, ... passign [istart][, iend
iarray passign [istart][, iend
karray passign [istart][, iend" :doc "Assigns a range of p-fields to ivariables.") csdoc-opcode-database)
(puthash "slider16" '(:template "i1,...,i16 slider16 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum16, imin16, imax16, init16, ifn16
k1,...,k16 slider16 ichan, ictlnum1, imin1, imax1, init1, ifn1,..., ictlnum16, imin16, imax16, init16, ifn16" :doc "Creates a bank of 16 different MIDI control message numbers.") csdoc-opcode-database)
(puthash "push_f" '(:template "push_f fsig" :doc "WTF: ") csdoc-opcode-database)
(puthash "sockrecv" '(:template "asig sockrecv iport, ilength
ksig sockrecv iport, ilength
asigl, asigr sockrecvs iport, ilength
String sockrecv iport, ilength
asig[,kstate] strecv Sipaddr, iport" :doc "Receives data from other processes using the low-level UDP or TCP protocols") csdoc-opcode-database)
(puthash "JackoInit" '(:template "JackoInit ServerName, SclientName" :doc "Initializes Csound as a Jack client.") csdoc-opcode-database)
(puthash "vbap8" '(:template "ar1, ..., ar8 vbap8 asig, kazim [, kelev] [, kspread]" :doc "Distributes an audio signal among 8 channels.") csdoc-opcode-database)
(puthash "sndloop" '(:template "asig, krec sndloop ain, kpitch, ktrig, idur, ifad" :doc "A sound looper with pitch control.") csdoc-opcode-database)
(puthash "FLtabsEnd" '(:template "FLtabsEnd" :doc "Marks the end of a tabbed FLTK interface.") csdoc-opcode-database)
(puthash "c2r" '(:template "kout[] c2r kin[]" :doc "Real to complex format conversion.") csdoc-opcode-database)
(puthash "tableiw" '(:template "tableiw isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]" :doc "Deprecated.") csdoc-opcode-database)
(puthash "zkmod" '(:template "kres zkmod ksig, kzkmod" :doc "Facilitates the modulation of one signal by another.") csdoc-opcode-database)
(puthash "filesr" '(:template "ir filesr ifilcod [, iallowraw]" :doc "Returns the sample rate of a sound file.") csdoc-opcode-database)
(puthash "midion" '(:template "midion kchn, knum, kvel" :doc "Generates MIDI note messages at k-rate.") csdoc-opcode-database)
(puthash "qnan" '(:template "qnan(x) (no rate restriction)" :doc "Questions whether the argument is not a number") csdoc-opcode-database)
(puthash "xtratim" '(:template "xtratim iextradur" :doc "Extend the duration of real-time generated events.") csdoc-opcode-database)
(puthash "array" '(:template "karray[] array ival1, ival2,.....ivaln" :doc "Deprecated.") csdoc-opcode-database)
(puthash "strcpy" '(:template "Sdst strcpy Ssrc
Sdst = Ssrc" :doc "Assign value to a string variable") csdoc-opcode-database)
(puthash "foutk" '(:template "foutk ifilename, iformat, kout1 [, kout2, kout3,....,koutN]" :doc "Outputs k-rate signals of an arbitrary number of channels to a specified file, in raw (headerless) format.") csdoc-opcode-database)
(puthash "hilbert2" '(:template "ar1, ar2 hilbert2 asig, ifftsize, ihopsize" :doc "A Hilbert transformer.") csdoc-opcode-database)
(puthash "strcpyk" '(:template "Sdst strcpyk Ssrc" :doc "Assign value to a string variable (k-rate)") csdoc-opcode-database)
(puthash "transegb" '(:template "ares transegb ia, itim, itype, ib [, itim2] [, itype] [, ic] ...
kres transegb ia, itim, itype, ib [, itim2] [, itype] [, ic] ..." :doc "Constructs a user-definable envelope in absolute time.") csdoc-opcode-database)
(puthash "bformdec" '(:template "ao1, ao2 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8 bformdec isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]]" :doc "Deprecated. Decodes an ambisonic B format signal.") csdoc-opcode-database)
(puthash "expseg" '(:template "ares expseg ia, idur1, ib [, idur2] [, ic] [...]
kres expseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "Trace a series of exponential segments between specified points.") csdoc-opcode-database)
(puthash "log" '(:template "log(x) (no rate restriction)
log(k/i[]) (k- or i-arrays )
kout[]log kin[],ibas" :doc "Returns a natural log of a number, or an array (with optional arbitrary base).") csdoc-opcode-database)
(puthash "xout" '(:template "xout xoutarg1 [, xoutarg2] ... [, xoutargN]" :doc "Retrieves variables from a user-defined opcode block,") csdoc-opcode-database)
(puthash "octmidib" '(:template "ioct octmidib [irange]
koct octmidib [irange]" :doc "Get the note number of the current MIDI event and modify it by the current pitch-bend value, express it in octave-point-decimal.") csdoc-opcode-database)
(puthash "dctinv" '(:template "kout[] dctinv kin[]
iout[] dctinv iin[]" :doc "Inverse Discrete Cosine Transform of a sample array (type-III DCT)") csdoc-opcode-database)
(puthash "biquada" '(:template "ares biquada asig, ab0, ab1, ab2, aa0, aa1, aa2 [, iskip]" :doc "A sweepable general purpose biquadratic digital filter with a-rate parameters.") csdoc-opcode-database)
(puthash "wgpluck2" '(:template "ares wgpluck2 iplk, kamp, icps, kpick, krefl" :doc "Physical model of the plucked string.") csdoc-opcode-database)
(puthash "vlinseg" '(:template "vlinseg ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]" :doc "Vectorial envelope generator") csdoc-opcode-database)
(puthash "minabsaccum" '(:template "minabsaccum aAccumulator, aInput" :doc "Accumulates the minimum of the absolute values of audio signals.") csdoc-opcode-database)
(puthash "ftlptim" '(:template "ftlptim(x) (init-rate args only)" :doc "Returns the loop segment start-time of a stored function table number.") csdoc-opcode-database)
(puthash "vadd" '(:template "vadd ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "Adds a scalar value to a vector in a table.") csdoc-opcode-database)
(puthash "urd" '(:template "aout = urd(ktableNum)
iout = urd(itableNum)
kout = urd(ktableNum)" :doc "A discrete user-defined-distribution random generator that can be used as a function.") csdoc-opcode-database)
(puthash "MixerSetLevel" '(:template "MixerSetLevel isend, ibuss, kgain" :doc "Sets the level of a send to a buss.") csdoc-opcode-database)
(puthash "sfplaym" '(:template "ares sfplaym ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "Plays a SoundFont2 (SF2) sample preset, generating a mono sound.") csdoc-opcode-database)
(puthash "ftsamplebank" '(:template "iNumberOfFile ftsamplebank SDirectory, iFirstTableNumber, iSkipTime, iFormat, iChannel,
kNumberOfFile ftsamplebank SDirectory, kFirstTableNumber, kTrigger, kSkipTime, kFormat, kChannel," :doc "Reads a directory for sound files.") csdoc-opcode-database)
(puthash "lorisread" '(:template "lorisread ktimpnt, ifilcod, istoreidx, kfreqenv, kampenv, kbwenv[, ifadetime]" :doc "Imports a set of bandwidth-enhanced partials from a SDIF-format
    data file, applying control-rate frequency, amplitude, and
    bandwidth scaling envelopes, and stores the modified partials in
    memory.") csdoc-opcode-database)
(puthash "powershape" '(:template "aout powershape ain, kShapeAmount [, ifullscale]" :doc "Waveshapes a signal by raising it to a variable exponent.") csdoc-opcode-database)
(puthash "minaccum" '(:template "minaccum aAccumulator, aInput" :doc "Accumulates the minimum value of audio signals.") csdoc-opcode-database)
(puthash "floor" '(:template "floor(x) (init-, control-, or audio-rate arg allowed)
floor(k/i[]) (k- or i-arrays )" :doc "Returns the largest integer not greater than") csdoc-opcode-database)
(puthash "fiopen" '(:template "ihandle fiopen ifilename, imode" :doc "Opens a file in a specific mode.") csdoc-opcode-database)
(puthash "JackoMidiOutConnect" '(:template "JackoMidiOutConnect ScsoundPortName, SexternalPortName" :doc "Creates a MIDI connection from Csound to a Jack port.") csdoc-opcode-database)
(puthash "cosseg" '(:template "ares cosseg ia, idur1, ib [, idur2] [, ic] [...]
kres cosseg ia, idur1, ib [, idur2] [, ic] [...]" :doc "Trace a series of line segments between specified points with
      cosine interpolation.") csdoc-opcode-database)
(puthash "platerev" '(:template "a1[, a2, ...] platerev itabexcite. itabouts, kbndry, iaspect, istiff, idecay, iloss, aexcite1[, aexcite2, ...]" :doc "Models the reverberation of a metal plate.") csdoc-opcode-database)
(puthash "jacktransport" '(:template "jacktransport icommand [, ilocation]" :doc "Start/stop jack_transport and can optionally relocate the playback head.") csdoc-opcode-database)
(puthash "downsamp" '(:template "kres downsamp asig [, iwlen]" :doc "Modify a signal by down-sampling.") csdoc-opcode-database)
(puthash "tempoval" '(:template "kres tempoval" :doc "Reads the current value of the tempo.") csdoc-opcode-database)
(puthash "olabuffer" '(:template "aout olabuffer kin, ioverlap" :doc "Sum overlapping frames of audio as k-rate arrays and read as an audio signal") csdoc-opcode-database)
(puthash "sfplay3" '(:template "ar1, ar2 sfplay3 ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "Plays a SoundFont2 (SF2) sample preset, generating a stereo sound with cubic interpolation.") csdoc-opcode-database)
(puthash "nestedap" '(:template "ares nestedap asig, imode, imaxdel, idel1, igain1 [, idel2] [, igain2] [, idel3] [, igain3] [, istor]" :doc "Three different nested all-pass filters.") csdoc-opcode-database)
(puthash "transegr" '(:template "ares transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
kres transegr ia, idur, itype, ib [, idur2] [, itype] [, ic] ..." :doc "Constructs a user-definable envelope with extended release segment.") csdoc-opcode-database)
(puthash "compilestr" '(:template "ires compilestr Sorch" :doc "compiles a new orchestra passed in as an ASCII string") csdoc-opcode-database)
(puthash "vbap" '(:template "ar1[, ar2...] vbap asig, kazim [, kelev] [, kspread] [, ilayout]
array[] vbap asig, kazim [, kelev] [, kspread] [, ilayout]" :doc "Distributes an audio signal among many channels.") csdoc-opcode-database)
(puthash "tanh" '(:template "tanh(x) (no rate restriction)
tanh(k/i[]) (k- or i-arrays )" :doc "Performs a hyperbolic tangent function.") csdoc-opcode-database)
(puthash "pset" '(:template "pset icon1 [, icon2] [...]" :doc "Defines and initializes numeric arrays at orchestra load time.") csdoc-opcode-database)
(puthash "moogvcf2" '(:template "ares moogvcf2 asig, xfco, xres [,iscale, iskip]" :doc "A digital emulation of the Moog diode ladder filter configuration.") csdoc-opcode-database)
(puthash "ftprint" '(:template "ftprint ifn [, ktrig, kstart, kend, kstep, inumcols ]" :doc "Print the contents of a table (for debugging)") csdoc-opcode-database)
(puthash "duserrnd" '(:template "aout duserrnd ktableNum
iout duserrnd itableNum
kout duserrnd ktableNum" :doc "Discrete USER-defined-distribution RaNDom generator.") csdoc-opcode-database)
(puthash "dumpk2" '(:template "dumpk2 ksig1, ksig2, ifilname, iformat, iprd" :doc "Periodically writes two orchestra control-signal values to an external file.") csdoc-opcode-database)
(puthash "JackoInfo" '(:template "JackoInfo" :doc "Prints information about the Jack system.") csdoc-opcode-database)
(puthash "nstrstr" '(:template "Sname nstrstrinsno
Sname nstrstrknsno" :doc "Returns the string of a named instr from its number.") csdoc-opcode-database)
(puthash "pvsmix" '(:template "fsig pvsmix fsigin1, fsigin2" :doc "Mix 'seamlessly' two pv signals.") csdoc-opcode-database)
(puthash "midinoteonkey" '(:template "midinoteonkey xkey, xvelocity" :doc "Gets a MIDI note number value.") csdoc-opcode-database)
(puthash "sfplay" '(:template "ar1, ar2 sfplay ivel, inotenum, xamp, xfreq, ipreindex [, iflag] [, ioffset] [, ienv]" :doc "Plays a SoundFont2 (SF2) sample preset, generating a stereo sound.") csdoc-opcode-database)
(puthash "rbjeq" '(:template "ar rbjeq asig, kfco, klvl, kQ, kS[, imode]" :doc "Parametric equalizer and filter opcode with 7 filter types, based
      on algorithm by Robert Bristow-Johnson.") csdoc-opcode-database)
(puthash "pdhalfy" '(:template "aout pdhalfy ain, kShapeAmount [, ibipolar [, ifullscale]]" :doc "Distorts a phasor for reading two unequal portions of a table in equal periods.") csdoc-opcode-database)
(puthash "ntom" '(:template "kmidi ntom Snote
imidi ntom Snote" :doc "Convert note name to midi note number") csdoc-opcode-database)
(puthash "partikkelset" '(:template "partikkelset kparameterindex, kmaskindex, iopcode_id" :doc "Set mask index for a specific mask parameter of a running") csdoc-opcode-database)
(puthash "ptable" '(:template "ares ptable andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptable indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptable kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing.") csdoc-opcode-database)
(puthash "ftfree" '(:template "ftfree ifno, iwhen" :doc "Deletes function table.") csdoc-opcode-database)
(puthash "harmon" '(:template "ares harmon asig, kestfrq, kmaxvar, kgenfreq1, kgenfreq2, imode, iminfrq, iprd" :doc "Analyze an audio input and generate harmonizing voices in synchrony.") csdoc-opcode-database)
(puthash "JackoMidiInConnect" '(:template "JackoMidiInConnect SexternalPortName, ScsoundPortName" :doc "Creates a MIDI  connection from a Jack port to Csound.") csdoc-opcode-database)
(puthash "ptable3" '(:template "ares ptable3 andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires ptable3 indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres ptable3 kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing with cubic interpolation.") csdoc-opcode-database)
(puthash "fof2" '(:template "ares fof2 xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur, kphs, kgliss [, iskip]" :doc "Produces sinusoid bursts including k-rate incremental indexing with each successive burst.") csdoc-opcode-database)
(puthash "midinoteoff" '(:template "midinoteoff xkey, xvelocity" :doc "Gets a MIDI noteoff value.") csdoc-opcode-database)
(puthash "cossegr" '(:template "ares cossegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz
kres cossegr ia, idur1, ib [, idur2] [, ic] [...], irel, iz" :doc "Trace a series of line segments between specified points with
      cosine interpolation, including a release segment.") csdoc-opcode-database)
(puthash "pvinterp" '(:template "ares pvinterp ktimpnt, kfmod, ifile, kfreqscale1, kfreqscale2, kampscale1, kampscale2, kfreqinterp, kampinterp" :doc "Interpolates between the amplitudes and frequencies of two phase vocoder analysis files.") csdoc-opcode-database)
(puthash "imagecreate" '(:template "iimagenum imagecreate iwidth, iheight" :doc "Create an empty image of a given size.") csdoc-opcode-database)
(puthash "tableshuffle" '(:template "tableshuffle ktablenum
tableshufflei itablenum" :doc "shuffles the content of a function table so that each element of the source
      table is put into a different random position.") csdoc-opcode-database)
(puthash "ftchnls" '(:template "ftchnls(x) (init-rate args only)" :doc "Returns the number of channels in a stored function table.") csdoc-opcode-database)
(puthash "filenchnls" '(:template "ir filenchnls ifilcod [, iallowraw]" :doc "Returns the number of channels in a sound file.") csdoc-opcode-database)
(puthash "tabsum" '(:template "kr tabsum ifn[[, kmin] [, kmax]]" :doc "Adding values in a range of a table.") csdoc-opcode-database)
(puthash "outipc" '(:template "outipc ichn, iprog, imin, imax" :doc "Sends MIDI program change messages at i-rate") csdoc-opcode-database)
(puthash "compress2" '(:template "ar compress2 aasig, acsig, kthresh, kloknee, khiknee, kratio, katt, krel, ilook" :doc "Compress, limit, expand, duck or gate an audio signal.") csdoc-opcode-database)
(puthash "grain3" '(:template "ares grain3 kcps, kphs, kfmd, kpmd, kgdur, kdens, imaxovr, kfn, iwfn, kfrpow, kprpow [, iseed] [, imode]" :doc "Generate granular synthesis textures with more user control.") csdoc-opcode-database)
(puthash "lpslot" '(:template "lpslot islot" :doc "Selects the slot to be use by further lp opcodes.") csdoc-opcode-database)
(puthash "atonex" '(:template "ares atonex asig, khp [, inumlayer] [, iskip]
ares atonex asig, ahp [, inumlayer] [, iskip]" :doc "Emulates a stack of filters using the atone opcode.") csdoc-opcode-database)
(puthash "trhighest" '(:template "fsig, kfr, kamp trhighest fin1, kscal" :doc "Extracts the highest-frequency track from a streaming track input signal.") csdoc-opcode-database)
(puthash "loscilx" '(:template "ar1 [, ar2, ar3, ar4, ar5, ar6, ar7, ar8, ar9, ar10, ar11, ar12, ar13, ar14, ar15, ar16] loscilx xamp, kcps, ifn [, iwsize, ibas, istrt, imod, ibeg, iend]
ar[] loscilx xamp, kcps, ifn [, iwsize, ibas, istrt, imod, ibeg, iend]" :doc "Read multi-channel sampled sound from a table.") csdoc-opcode-database)
(puthash "fof" '(:template "ares fof xamp, xfund, xform, koct, kband, kris, kdur, kdec, iolaps, ifna, ifnb, itotdur [, iphs] [, ifmode] [, iskip]" :doc "Produces sinusoid bursts useful for formant and granular synthesis.") csdoc-opcode-database)
(puthash "mp3in" '(:template "ar1, ar2 mp3in ifilcod[, iskptim, iformat, iskipinit, ibufsize]
ar1 mp3in ifilcod[, iskptim, iformat, iskipinit, ibufsize]" :doc "Reads mono or stereo audio data from an external MP3 file.") csdoc-opcode-database)
(puthash "nchnls_hw" '(:template "idacc,iadcc nchnls_hw" :doc "Returns the number of audio channels in the underlying hardware.") csdoc-opcode-database)
(puthash "taninv" '(:template "taninv(x) (no rate restriction)
taninv(k/i[]) (k- or i-arrays )" :doc "Performs an arctangent function.") csdoc-opcode-database)
(puthash "FLpack" '(:template "FLpack iwidth, iheight, ix, iy, itype, ispace, iborder" :doc "Provides the functionality of compressing and aligning FLTK widgets.") csdoc-opcode-database)
(puthash "pluck" '(:template "ares pluck kamp, kcps, icps, ifn, imeth [, iparm1] [, iparm2]" :doc "Produces a naturally decaying plucked string or drum sound.") csdoc-opcode-database)
(puthash "STKBlowBotl" '(:template "asignal STKBlowBotl ifrequency, iamplitude, [knoise, kv1[, klfo, kv2[, klfodepth, kv3[, kvol, kv4]]]]" :doc "STKBlowBotl uses a helmholtz resonator (biquad filter) with a polynomial jet excitation.") csdoc-opcode-database)
(puthash "resonxk" '(:template "kres resonxk ksig, kcf, kbw[, inumlayer, iscl, istor]" :doc "Control signal resonant filter stack.") csdoc-opcode-database)
(puthash "poscil3" '(:template "ares poscil3 aamp, acps [, ifn, iphs]
ares poscil3 aamp, kcps [, ifn, iphs]
ares poscil3 kamp, acps [, ifn, iphs]
ares poscil3 kamp, kcps [, ifn, iphs]
ires poscil3 kamp, kcps [, ifn, iphs]
kres poscil3 kamp, kcps [, ifn, iphs]" :doc "High precision oscillator with cubic interpolation.") csdoc-opcode-database)
(puthash "maca" '(:template "ares maca asig1 , asig2 [, asig3] [, asig4] [, asig5] [...]" :doc "Multiply and accumulate a-rate signals only.") csdoc-opcode-database)
(puthash "locsig" '(:template "a1, a2 locsig asig, kdegree, kdistance, kreverbsend
a1, a2, a3, a4 locsig asig, kdegree, kdistance, kreverbsend" :doc "Takes an input signal and distributes between 2 or 4 channels.") csdoc-opcode-database)
(puthash "tablexseg" '(:template "tablexseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "Creates a new function table by making exponential segments between values in stored function tables.") csdoc-opcode-database)
(puthash "FLslidBnk2Setk" '(:template "FLslidBnk2Setk ktrig, ihandle, ifn [, istartIndex, istartSlid, inumSlid]" :doc "modify the values of a slider bank.") csdoc-opcode-database)
(puthash "STKPercFlut" '(:template "asignal STKPercFlut ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STKPercFlut is a percussive flute FM synthesis instrument.") csdoc-opcode-database)
(puthash "pvslock" '(:template "fsig pvslock fsigin, klock" :doc "Frequency lock an input fsig") csdoc-opcode-database)
(puthash "alpass" '(:template "ares alpass asig, xrvt, ilpt [, iskip] [, insmps]" :doc "Reverberates an input signal with a flat frequency response.") csdoc-opcode-database)
(puthash "pyeval" '(:template "kresult pyeval "expression"
iresult pyevali "expression"
kresult pyleval "expression"
iresult pylevali "expression"
kresult pyevalt ktrigger, "expression"
kresult pylevalt ktrigger, "expression"" :doc "Evaluate a generic Python expression and store the result in a Csound variable at k-time or i-time (i suffix).") csdoc-opcode-database)
(puthash "genarray_i" '(:template "karray genarray_i istart, iend [,inc]" :doc "Generate a vector with an arithmetic sequence.") csdoc-opcode-database)
(puthash "lposcil" '(:template "ares lposcil kamp, kfreqratio, kloop, kend, ifn [, iphs]" :doc "Read sampled sound from a table with looping and high precision.") csdoc-opcode-database)
(puthash "reson" '(:template "ares reson asig, xcf, xbw [, iscl] [, iskip]" :doc "A second-order resonant filter.") csdoc-opcode-database)
(puthash "trlowest" '(:template "fsig, kfr, kamp trlowest fin1, kscal" :doc "Extracts the lowest-frequency track from a streaming track input signal.") csdoc-opcode-database)
(puthash "STKTubeBell" '(:template "asignal STKTubeBell ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STKTubeBell is a  tubular bell (orchestral chime) FM synthesis instrument.") csdoc-opcode-database)
(puthash "vdelayxws" '(:template "aout1, aout2 vdelayxws ain1, ain2, adl, imd, iws [, ist]" :doc "Variable delay opcodes with high quality interpolation.") csdoc-opcode-database)
(puthash "fluidProgramSelect" '(:template "fluidProgramSelect ienginenum, ichannelnum, isfnum, ibanknum, ipresetnum" :doc "Assigns a preset from a SoundFont to a channel on a fluidEngine.") csdoc-opcode-database)
(puthash "serialWrite_i" '(:template "serialWrite_i iPort, iByte
serialWrite_i iPort, SBytes" :doc "Write data to a serial port.") csdoc-opcode-database)
(puthash "transeg" '(:template "ares transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ...
kres transeg ia, idur, itype, ib [, idur2] [, itype] [, ic] ..." :doc "Constructs a user-definable envelope.") csdoc-opcode-database)
(puthash "FLsetSnapGroup" '(:template "FLsetSnapGroup igroup" :doc "Determines the snapshot group for FL valuators.") csdoc-opcode-database)
(puthash "pvsinfo" '(:template "ioverlap, inumbins, iwinsize, iformat pvsinfo fsrc" :doc "Get information from a PVOC-EX formatted source.") csdoc-opcode-database)
(puthash "vibrato" '(:template "kout vibrato kAverageAmp, kAverageFreq, kRandAmountAmp, kRandAmountFreq, kAmpMinRate, kAmpMaxRate, kcpsMinRate, kcpsMaxRate, ifn [, iphs" :doc "Generates a natural-sounding user-controllable vibrato.") csdoc-opcode-database)
(puthash "doppler" '(:template "ashifted doppler asource, ksourceposition, kmicposition [, isoundspeed, ifiltercutoff]" :doc "A fast and robust method for approximating sound propagation, achieving convincing Doppler shifts without having to solve equations.") csdoc-opcode-database)
(puthash "fluidNote" '(:template "fluidNote ienginenum, ichannelnum, imidikey, imidivel" :doc "Plays a note on a channel in a fluidSynth engine.") csdoc-opcode-database)
(puthash "tablefilter" '(:template "knumpassed tablefilter kouttable, kintatble, kmode, kparam" :doc "Filters a source table and writes result into a destination table.") csdoc-opcode-database)
(puthash "strfromurl" '(:template "Sdst strfromurl StringURL" :doc "Set string variable to value read from an URL") csdoc-opcode-database)
(puthash "ATSadd" '(:template "ar ATSadd ktimepnt, kfmod, iatsfile, ifn, ipartials[, ipartialoffset, ipartialincr, igatefn]" :doc "uses the data from an ATS analysis file to perform additive synthesis.") csdoc-opcode-database)
(puthash "remove" '(:template "remove insnum" :doc "Removes the definition of an instrument.") csdoc-opcode-database)
(puthash "pvsifd" '(:template "ffr,fphs pvsifd ain, ifftsize, ihopsize, iwintype[,iscal]" :doc "Instantaneous Frequency Distribution, magnitude and phase analysis.") csdoc-opcode-database)
(puthash "fmvoice" '(:template "ares fmvoice kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate[, ifn1, ifn2, ifn3, ifn4, ivibfn]" :doc "FM Singing Voice Synthesis") csdoc-opcode-database)
(puthash "pop_f" '(:template "fsig pop_f" :doc "WTF: ") csdoc-opcode-database)
(puthash "tival" '(:template "ir tival" :doc "Puts the value of the instrument's internal") csdoc-opcode-database)
(puthash "MixerSend" '(:template "MixerSend asignal, isend, ibuss, ichannel" :doc "Mixes an arate signal into a channel of a buss.") csdoc-opcode-database)
(puthash "initc21" '(:template "initc21 ichan, ictlno1, ictlno2, ictlno3, ivalue" :doc "Initializes the controllers used to create a 21-bit MIDI value.") csdoc-opcode-database)
(puthash "copya2ftab" '(:template "copya2ftab kftbl, tab" :doc "Copy data from a vector to an f-table.") csdoc-opcode-database)
(puthash "slider16table" '(:template "kflag slider16table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum16, imin16, imax16, init16, ifn16" :doc "Stores a bank of 16 different MIDI control messages to a table.") csdoc-opcode-database)
(puthash "temposcal" '(:template "asig temposcal ktimescal, kamp, kpitch, ktab, klock [,ifftsize, idecim, ithresh]" :doc "Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'.") csdoc-opcode-database)
(puthash "inleta" '(:template "asignal inleta Sname" :doc "Receives an arate signal into an instrument through a named port.") csdoc-opcode-database)
(puthash "tabmorph" '(:template "kout tabmorph kindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ...,ifnN]" :doc "Allow morphing between a set of tables.") csdoc-opcode-database)
(puthash "deltapi" '(:template "ares deltapi xdlt" :doc "Taps a delay line at variable offset times, uses interpolation.") csdoc-opcode-database)
(puthash "hrtfer" '(:template "aleft, aright hrtfer asig, kaz, kelev," :doc "Creates 3D audio for two speakers.") csdoc-opcode-database)
(puthash "midichannelaftertouch" '(:template "midichannelaftertouch xchannelaftertouch [, ilow] [, ihigh]" :doc "Gets a MIDI channel's aftertouch value.") csdoc-opcode-database)
(puthash "nchnls_i" '(:template "nchnls_i = iarg" :doc "Sets the number of channels of audio input.") csdoc-opcode-database)
(puthash "fmax" '(:template "ires[] fmax iarg1[], iarg2[]
kres[] fmax karg1[], karg2[]
ires[] fmax iarg1[], iarg2
kres[] fmax karg[], karg2" :doc "Maximum value function.") csdoc-opcode-database)
(puthash "initc14" '(:template "initc14 ichan, ictlno1, ictlno2, ivalue" :doc "Initializes the controllers used to create a 14-bit MIDI value.") csdoc-opcode-database)
(puthash "phasorbnk" '(:template "ares phasorbnk xcps, kndx, icnt [, iphs]
kres phasorbnk kcps, kndx, icnt [, iphs]" :doc "Produce an arbitrary number of normalized moving phase values.") csdoc-opcode-database)
(puthash "wrap" '(:template "ares wrap asig, klow, khigh
ires wrap isig, ilow, ihigh
kres wrap ksig, klow, khigh" :doc "Wraps-around the signal that exceeds the low and high thresholds.") csdoc-opcode-database)
(puthash "sfilist" '(:template "sfilist ifilhandle" :doc "Prints a list of all instruments of a previously loaded SoundFont2 (SF2) file.") csdoc-opcode-database)
(puthash "readscore" '(:template "readscore Sin" :doc "Read, preprocess and schedule a score from an input string.") csdoc-opcode-database)
(puthash "fmpercfl" '(:template "ares fmpercfl kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn]" :doc "Uses FM synthesis to create a percussive flute sound.") csdoc-opcode-database)
(puthash "readclock" '(:template "ir readclock inum" :doc "Reads the value of an internal clock.") csdoc-opcode-database)
(puthash "bformenc1" '(:template "aw, ax, ay, az bformenc1 asig, kalpha, kbeta
aw, ax, ay, az, ar, as, at, au, av bformenc1 asig, kalpha, kbeta
aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq bformenc1 asig, kalpha, kbeta
aarray[] bformenc1 asig, kalpha, kbeta" :doc "Codes a signal into the ambisonic B format.") csdoc-opcode-database)
(puthash "OSCcount" '(:template "kans OSCcount" :doc "Gives the Count of OSC messages currently unread.") csdoc-opcode-database)
(puthash "table" '(:template "ares table andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires table indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres table kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing.") csdoc-opcode-database)
(puthash "fluidSetInterpMethod" '(:template "fluidSetInterpMethod ienginenum, ichannelnum, iInterpMethod" :doc "Set interpolation method for channel in Fluid Engine") csdoc-opcode-database)
(puthash "pvsvoc" '(:template "fsig pvsvoc famp, fexc, kdepth, kgain [,kcoefs]" :doc "Combine the spectral envelope of one fsig with the excitation (frequencies) of another.") csdoc-opcode-database)
(puthash "FLhide" '(:template "FLhide ihandle" :doc "Hides the target FLTK widget.") csdoc-opcode-database)
(puthash "filter2" '(:template "ares filter2 asig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN
kres filter2 ksig, iM, iN, ib0, ib1, ..., ibM, ia1, ia2, ..., iaN" :doc "Performs filtering using a transposed form-II digital filter lattice with no time-varying control.") csdoc-opcode-database)
(puthash "deltap3" '(:template "ares deltap3 xdlt" :doc "Taps a delay line at variable offset times, uses cubic interpolation.") csdoc-opcode-database)
(puthash "vdelayxs" '(:template "aout1, aout2 vdelayxs ain1, ain2, adl, imd, iws [, ist]" :doc "A stereo variable delay opcode with high quality interpolation.") csdoc-opcode-database)
(puthash "FLscrollEnd" '(:template "FLscrollEnd" :doc "A FLTK opcode that marks the end of an area with scrollbars.") csdoc-opcode-database)
(puthash "JackoAudioOutConnect" '(:template "JackoAudioOutConnect ScsoundPortName, SexternalPortName" :doc "Creates an audio connection from Csound to a Jack port.") csdoc-opcode-database)
(puthash "wgbowedbar" '(:template "ares wgbowedbar kamp, kfreq, kpos, kbowpres, kgain [, iconst] [, itvel] [, ibowpos] [, ilow]" :doc "A physical model of a bowed bar.") csdoc-opcode-database)
(puthash "db" '(:template "db(x)" :doc "Returns the amplitude equivalent for a given decibel amount.") csdoc-opcode-database)
(puthash "pchbend" '(:template "ibend pchbend [imin] [, imax]
kbend pchbend [imin] [, imax]" :doc "Get the current pitch-bend value for this channel.") csdoc-opcode-database)
(puthash "sensekey" '(:template "kres[, kkeydown] sensekey" :doc "Returns the ASCII code of a key that has been pressed.") csdoc-opcode-database)
(puthash "pchoct" '(:template "pchoct (oct) (init- or control-rate args only)" :doc "Converts an octave-point-decimal value to pitch-class.") csdoc-opcode-database)
(puthash "vpow" '(:template "vpow ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "Raises each element of a vector to a scalar power.") csdoc-opcode-database)
(puthash "readfi" '(:template "Sres, iline readfi ifilname" :doc "Read a line of text from an external file.") csdoc-opcode-database)
(puthash "pvswarp" '(:template "fsig pvswarp fsigin, kscal, kshift[, klowest, kmeth, kgain, kcoefs]" :doc "Warp the spectral envelope of a PVS signal") csdoc-opcode-database)
(puthash "strindex" '(:template "ipos strindex S1, S2" :doc "Return the position of the first occurence of a string in another string") csdoc-opcode-database)
(puthash "serialRead" '(:template "kByte serialRead iPort" :doc "Read data from a serial port.") csdoc-opcode-database)
(puthash "fluidInfo" '(:template "SPrograms[] fluidInfo ienginenum" :doc "Retrieves program information from currently loaded soundfont.") csdoc-opcode-database)
(puthash "vecdelay" '(:template "vecdelay ifn, ifnIn, ifnDel, ielements, imaxdel [, iskip]" :doc "Vectorial Control-rate Delay Paths") csdoc-opcode-database)
(puthash "hrtfmove" '(:template "aleft, aright hrtfmove asrc, kAz, kElev, ifilel, ifiler [, imode, ifade, isr]" :doc "Generates dynamic 3d binaural audio for headphones using magnitude interpolation and phase truncation.") csdoc-opcode-database)
(puthash "fluidEngine" '(:template "ienginenum fluidEngine [iChorusEnabled] [, iRevervEnabled] [, iNumChannels] [, iPolyphony]" :doc "Instantiates a fluidsynth engine.") csdoc-opcode-database)
(puthash "weibull" '(:template "ares weibull ksigma, ktau
ires weibull ksigma, ktau
kres weibull ksigma, ktau" :doc "WTF: ") csdoc-opcode-database)
(puthash "vtablea" '(:template "vtablea andx, kfn, kinterp, ixmode, aout1 [, aout2, aout3, .... , aoutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "assign" '(:template "ares = xarg
ires = iarg
kres = karg
ires, ... = iarg, ...
kres, ... = karg, ...
table [ kval] = karg" :doc "Performs a simple assignment.") csdoc-opcode-database)
(puthash "dumpk4" '(:template "dumpk4 ksig1, ksig2, ksig3, ksig4, ifilname, iformat, iprd" :doc "Periodically writes four orchestra control-signal values to an external file.") csdoc-opcode-database)
(puthash "xyscale" '(:template "kout xyscale kx, ky, k00, k10, k01, k11" :doc "2D linear interpolation") csdoc-opcode-database)
(puthash "minarray" '(:template "kmin [,kindx] minarray karray" :doc "returns the minimum value in an array.") csdoc-opcode-database)
(puthash "s16b14" '(:template "i1,...,i16 s16b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb16, ictlno_lsb16, imin16, imax16, initvalue16, ifn16
k1,...,k16 s16b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb16, ictlno_lsb16, imin16, imax16, initvalue16, ifn16" :doc "Creates a bank of 16 different 14-bit MIDI control message numbers.") csdoc-opcode-database)
(puthash "outletkid" '(:template "outletkid Sname, SinstanceID, ksignal" :doc "Sends a krate signal out from an instrument to a named port.") csdoc-opcode-database)
(puthash "faustdsp" '(:template "ihandle faustdsp ifac" :doc "Instantiates a Faust program.") csdoc-opcode-database)
(puthash "maxabsaccum" '(:template "maxabsaccum aAccumulator, aInput" :doc "Accumulates the maximum of the absolute values of audio signals.") csdoc-opcode-database)
(puthash "outkc14" '(:template "outkc14 kchn, kmsb, klsb, kvalue, kmin, kmax" :doc "Sends 14-bit MIDI controller output at k-rate.") csdoc-opcode-database)
(puthash "imagefree" '(:template "imagefree iimagenum" :doc "Frees memory allocated for a previously loaded or created image.") csdoc-opcode-database)
(puthash "linsegb" '(:template "ares linsegb ia, itim1, ib [, itim2] [, ic] [...]
kres linsegb ia, itim1, ib [, itim2] [, ic] [...]" :doc "Trace a series of line segments between specified absolute points.") csdoc-opcode-database)
(puthash "out" '(:template "out asig1[, asig2,....]
out aarray" :doc "Writes audio data to an external device or stream.") csdoc-opcode-database)
(puthash "s32b14" '(:template "i1,...,i32 s32b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb32, ictlno_lsb32, imin32, imax32, initvalue32, ifn32
k1,...,k32 s32b14 ichan, ictlno_msb1, ictlno_lsb1, imin1, imax1, initvalue1, ifn1,..., ictlno_msb32, ictlno_lsb32, imin32, imax32, initvalue32, ifn32" :doc "Creates a bank of 32 different 14-bit MIDI control message numbers.") csdoc-opcode-database)
(puthash "oscili" '(:template "ares oscili xamp, xcps[, ifn, iphs]
kres oscili kamp, kcps[, ifn, iphs]" :doc "A simple oscillator with linear interpolation.") csdoc-opcode-database)
(puthash "minabs" '(:template "amin minabs ain1, ain2 [, ain3] [, ain4] [...]
kmin minabs kin1, kin2 [, kin3] [, kin4] [...]" :doc "Produces a signal that is the minimum of the absolute values of any number of input signals.") csdoc-opcode-database)
(puthash "zakinit" '(:template "zakinit isizea, isizek" :doc "Establishes zak space.") csdoc-opcode-database)
(puthash "phasor" '(:template "ares phasor xcps [, iphs]
kres phasor kcps [, iphs]" :doc "Produce a normalized moving phase value.") csdoc-opcode-database)
(puthash "STKPlucked" '(:template "asignal STKPlucked ifrequency, iamplitude" :doc "STKPlucked uses a plucked string physical model.") csdoc-opcode-database)
(puthash "link_enable" '(:template "ableton_link_enable i_peer [, k_enable]" :doc "Enable or disable synchronization with the Ableton Link session.") csdoc-opcode-database)
(puthash "sinh" '(:template "sinh(x) (no rate restriction)
sinh(k/i[]) (k- or i-arrays )" :doc "Performs a hyperbolic sine function.") csdoc-opcode-database)
(puthash "ftmorf" '(:template "ftmorf kftndx, iftfn, iresfn" :doc "Morphs between multiple ftables as specified in a list.") csdoc-opcode-database)
(puthash "pvadd" '(:template "ares pvadd ktimpnt, kfmod, ifilcod, ifn, ibins [, ibinoffset] [, ibinincr] [, iextractmode] [, ifreqlim] [, igatefn]" :doc "Reads from a") csdoc-opcode-database)
(puthash "exp" '(:template "exp(x) (no rate restriction)
exp(k/i[]) (k- or i-arrays)" :doc "Returns e raised to the x-th power.") csdoc-opcode-database)
(puthash "gendy" '(:template "ares gendy kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]
kres gendy kamp, kampdist, kdurdist, kadpar, kddpar, kminfreq, kmaxfreq, kampscl, kdurscl [, initcps] [, knum]" :doc "Dynamic stochastic approach to waveform synthesis conceived by Iannis Xenakis.") csdoc-opcode-database)
(puthash "pchmidi" '(:template "ipch pchmidi" :doc "Get the note number of the current MIDI event, expressed in pitch-class units.") csdoc-opcode-database)
(puthash "mididefault" '(:template "mididefault xdefault, xvalue" :doc "Changes values, depending on MIDI activation.") csdoc-opcode-database)
(puthash "compileorc" '(:template "ires compileorc Sfilename" :doc "compiles a new orchestra from an ASCII file") csdoc-opcode-database)
(puthash "fmod" '(:template "ires[] fmod iarg1[], iarg2[]
kres[] fmod karg1[], karg2[]
ires[] fmod iarg1[], iarg2
kres[] fmod karg[], karg2" :doc "Compute the floating point remainder operation.") csdoc-opcode-database)
(puthash "dct" '(:template "kout[] dct kin[]
iout[] dct iin[]" :doc "Discrete Cosine Transform of a sample array (type-II DCT)") csdoc-opcode-database)
(puthash "osciliktp" '(:template "ares osciliktp kcps, kfn, kphs [, istor]" :doc "A linearly interpolated oscillator that allows allows phase modulation.") csdoc-opcode-database)
(puthash "expsegba" '(:template "ares expsegba ia, itim1, ib [, itim2] [, ic] [...]" :doc "An exponential segment generator operating at a-rate with
      absolute times.") csdoc-opcode-database)
(puthash "vexpseg" '(:template "vexpseg ifnout, ielements, ifn1, idur1, ifn2 [, idur2, ifn3 [...]]" :doc "Vectorial envelope generator") csdoc-opcode-database)
(puthash "pchtom" '(:template "imidi pchtom ipch
kmidi pchtom kpch" :doc "Convert pch to midi note number") csdoc-opcode-database)
(puthash "vrandi" '(:template "vrandi ifn, krange, kcps, ielements [, idstoffset] [, iseed] [, isize] [, ioffset]" :doc "Generate a sort of 'vectorial band-limited noise'") csdoc-opcode-database)
(puthash "soundin" '(:template "ar1[, ar2[, ar3[, ... a24]]] soundin ifilcod [, iskptim] [, iformat] [, iskipinit] [, ibufsize]" :doc "Reads audio data from an external device or stream.") csdoc-opcode-database)
(puthash "tabmorphi" '(:template "kout tabmorphi kindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ..., ifnN]" :doc "Allow morphing between a set of tables with interpolation.") csdoc-opcode-database)
(puthash "birnd" '(:template "birnd(x) (init- or control-rate only)" :doc "Returns a random number in a bi-polar range.") csdoc-opcode-database)
(puthash "pareq" '(:template "ares pareq asig, kc, kv, kq [, imode] [, iskip]" :doc "Implementation of Zoelzer's parametric equalizer filters.") csdoc-opcode-database)
(puthash "powoftwo" '(:template "powoftwo(x) (init-rate or control-rate args only)" :doc "Performs a  power-of-two calculation.") csdoc-opcode-database)
(puthash "sc_lagud" '(:template "aout sc_lagud ain, klagup, klagdown
kout sc_lagud kin, klagup, klagdown" :doc "Exponential Lag") csdoc-opcode-database)
(puthash "vbap16move" '(:template "ar1, ..., ar16 vbap16move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "Distribute an audio signal among 16 channels with moving virtual sources.") csdoc-opcode-database)
(puthash "atonek" '(:template "kres atonek ksig, khp [, iskip]" :doc "A hi-pass filter whose transfer functions are the complements of the") csdoc-opcode-database)
(puthash "FLlabel" '(:template "FLlabel isize, ifont, ialign, ired, igreen, iblue" :doc "A FLTK opcode that modifies the appearance of a text label.") csdoc-opcode-database)
(puthash "fouti" '(:template "fouti ihandle, iformat, iflag, iout1 [, iout2, iout3,....,ioutN]" :doc "Outputs i-rate signals of an arbitrary number of channels to a specified file.") csdoc-opcode-database)
(puthash "JackoNoteOut" '(:template "JackoNoteOut ScsoundPortName, kstatus, kchannel, kdata1[, kdata2]" :doc "Sends a MIDI channel message to a Jack port.") csdoc-opcode-database)
(puthash "turnoff" '(:template "turnoff
turnoff inst
turnoff knst" :doc "Enables an instrument to turn itself off or to turn an instance of another instrument off.") csdoc-opcode-database)
(puthash "pvsfread" '(:template "fsig pvsfread ktimpt, ifn [, ichan]" :doc "Read a selected channel from a PVOC-EX analysis file.") csdoc-opcode-database)
(puthash "initc7" '(:template "initc7 ichan, ictlno, ivalue" :doc "Initializes the controller used to create a 7-bit MIDI value.") csdoc-opcode-database)
(puthash "FLpackEnd" '(:template "FLpackEnd" :doc "Marks the end of a group of compressed or aligned FLTK widgets.") csdoc-opcode-database)
(puthash "vbapg" '(:template "k1[, k2...] vbapg kazim [,kelev] [, kspread] [, ilayout]
karray[] vbapg kazim [,kelev] [, kspread] [, ilayout]" :doc "Calculates the gains for a sound location between multiple channels.") csdoc-opcode-database)
(puthash "until" '(:template "until condition do ... od" :doc "A syntactic looping construction.") csdoc-opcode-database)
(puthash "deltapxw" '(:template "deltapxw ain, adel, iwsize" :doc "Mixes the input signal to a delay line.") csdoc-opcode-database)
(puthash "strcat" '(:template "Sdst strcat Ssrc1, Ssrc2" :doc "Concatenate strings") csdoc-opcode-database)
(puthash "serialFlush" '(:template "serialFlush iPort" :doc "Flush data from a serial port.") csdoc-opcode-database)
(puthash "midicontrolchange" '(:template "midicontrolchange xcontroller, xcontrollervalue [, ilow] [, ihigh]" :doc "Gets a MIDI control change value.") csdoc-opcode-database)
(puthash "chnparams" '(:template "itype, imode, ictltype, idflt, imin, imax chnparams Sname" :doc "Query parameters of a channel.") csdoc-opcode-database)
(puthash "mincer" '(:template "asig mincer atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]" :doc "Phase-locked vocoder processing.") csdoc-opcode-database)
(puthash "paulstretch" '(:template "asig paulstretch istretch, iwindowsize, ift" :doc "Extreme time-stretching algorithm by Nasca Octavian Paul.") csdoc-opcode-database)
(puthash "moscil" '(:template "moscil kchn, knum, kvel, kdur, kpause" :doc "Sends a stream of the MIDI notes.") csdoc-opcode-database)
(puthash "midiprogramchange" '(:template "midiprogramchange xprogram" :doc "Gets a MIDI program change value.") csdoc-opcode-database)
(puthash "ckgoto" '(:template "ckgoto condition, label" :doc "Conditionally transfer control during the p-time passes.") csdoc-opcode-database)
(puthash "ftgenonce" '(:template "ifno ftgenonce ip1, ip2dummy, isize, igen, iarga, iargb, ..." :doc "Generate a function table from within an instrument definition, without duplication of data.") csdoc-opcode-database)
(puthash "randh" '(:template "ares randh xamp, xcps [, iseed] [, isize] [, ioffset]
kres randh kamp, kcps [, iseed] [, isize] [, ioffset]" :doc "Generates random numbers and holds them for a period of time.") csdoc-opcode-database)
(puthash "vbap8move" '(:template "ar1, ..., ar8 vbap8move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "Distributes an audio signal among 8 channels with moving virtual sources.") csdoc-opcode-database)
(puthash "hrtfearly" '(:template "aleft, aright, irt60low, irt60high, imfp hrtfearly asrc, ksrcx, ksrcy, ksrcz, klstnrx, klstnry, klstnrz, ifilel, ifiler, idefroom [,ifade, isr, iorder, ithreed, kheadrot, iroomx, iroomy, iroomz, iwallhigh, iwalllow, iwallgain1, iwallgain2, iwallgain3, ifloorhigh, ifloorlow, ifloorgain1, ifloorgain2, ifloorgain3, iceilinghigh, iceilinglow, iceilinggain1, iceilinggain2, iceilinggain3]" :doc "Generates 3D binaural audio with high-fidelity early reflections in a parametric room using a Phase Truncation algorithm.") csdoc-opcode-database)
(puthash "outkpc" '(:template "outkpc kchn, kprog, kmin, kmax" :doc "Sends MIDI program change messages at k-rate.") csdoc-opcode-database)
(puthash "mags" '(:template "kout[] mags kin[]" :doc "Obtains the magnitudes of a complex-number array") csdoc-opcode-database)
(puthash "wiiconnect" '(:template "ires wiiconnect [itimeout, imaxnum]" :doc "Reads data from a number of external Nintendo Wiimote controllers.") csdoc-opcode-database)
(puthash "goto" '(:template "goto label" :doc "Transfer control on every pass.") csdoc-opcode-database)
(puthash "midifilestatus" '(:template "ksig midifilestatus" :doc "Returns the playback status of MIDI file input.") csdoc-opcode-database)
(puthash "framebuffer" '(:template "kout[] framebuffer ain, isize" :doc "Read audio signals into 1 dimensional k-rate arrays and vice-versa with a specified buffer size.") csdoc-opcode-database)
(puthash "pvsadsyn" '(:template "ares pvsadsyn fsrc, inoscs, kfmod [, ibinoffset] [, ibinincr] [, iinit]" :doc "Resynthesize using a fast oscillator-bank.") csdoc-opcode-database)
(puthash "denorm" '(:template "denorm a1[, a2[, a3[, ... ]]]" :doc "Mixes low level noise to a list of a-rate signals") csdoc-opcode-database)
(puthash "wiidata" '(:template "kres wiidata kcontrol[, knum]" :doc "Reads data fields from a number of external Nintendo Wiimote controllers.") csdoc-opcode-database)
(puthash "remoteport" '(:template "remoteport iportnum" :doc "Defines the port for use with the remote system.") csdoc-opcode-database)
(puthash "strlenk" '(:template "klen strlenk Sstr" :doc "Return the length of a string") csdoc-opcode-database)
(puthash "slider8tablef" '(:template "kflag slider8tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum8, imin8, imax8, init8, ifn8, icutoff8" :doc "Stores a bank of 8 different MIDI control messages to a table, filtered before output.") csdoc-opcode-database)
(puthash "cepsinv" '(:template "kenv cepsinv keps[]" :doc "Calculate the inverse cepstrum of an array.") csdoc-opcode-database)
(puthash "noise" '(:template "ares noise xamp, kbeta" :doc "A white noise generator with an IIR lowpass filter.") csdoc-opcode-database)
(puthash "slider16tablef" '(:template "kflag slider16tablef ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, icutoff1, .... , ictlnum16, imin16, imax16, init16, ifn16, icutoff16" :doc "Stores a bank of 16 different MIDI control messages to a table, filtered before output.") csdoc-opcode-database)
(puthash "pvsftw" '(:template "kflag pvsftw fsrc, ifna [, ifnf]" :doc "Writes amplitude and/or frequency data to function tables.") csdoc-opcode-database)
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
kout = tb15(kIndex)" :doc "WTF: ") csdoc-opcode-database)
(puthash "lowresx" '(:template "ares lowresx asig, xcutoff, xresonance [, inumlayer] [, iskip]" :doc "Simulates layers of serially connected resonant lowpass filters.") csdoc-opcode-database)
(puthash "integ" '(:template "ares integ asig [, iskip]
kres integ ksig [, iskip]" :doc "Modify a signal by integration.") csdoc-opcode-database)
(puthash "trigseq" '(:template "trigseq ktrig_in, kstart, kloop, kinitndx, kfn_values, kout1 [, kout2] [...]" :doc "Accepts a trigger signal as input and outputs a group of values.") csdoc-opcode-database)
(puthash "gbuzz" '(:template "ares gbuzz xamp, xcps, knh, klh, kmul, ifn [, iphs]" :doc "Output is a set of harmonically related cosine partials.") csdoc-opcode-database)
(puthash "vtablek" '(:template "vtablek kndx, kfn, kinterp, ixmode, kout1 [, kout2, kout3, .... , koutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "midiout_i" '(:template "midiout_i istatus, ichan, idata1, idata2" :doc "Sends a generic MIDI message to the MIDI OUT port.") csdoc-opcode-database)
(puthash "imagesetpixel" '(:template "imagesetpixel iimagenum, ax, ay, ared, agreen, ablue
imagesetpixel iimagenum, kx, ky, kred, kgreen, kblue" :doc "Set the RGB value of a pixel inside a previously opened or created image.") csdoc-opcode-database)
(puthash "planet" '(:template "ax, ay, az planet kmass1, kmass2, ksep, ix, iy, iz, ivx, ivy, ivz, idelta [, ifriction] [, iskip]" :doc "Simulates a planet orbiting in a binary star system.") csdoc-opcode-database)
(puthash "vaddv" '(:template "vaddv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs addition between two vectorial control signals.") csdoc-opcode-database)
(puthash "resonx" '(:template "ares resonx asig, xcf, xbw [, inumlayer] [, iscl] [, iskip]" :doc "Emulates a stack of filters using the reson opcode.") csdoc-opcode-database)
(puthash "fini" '(:template "fini ifilename, iskipframes, iformat, in1 [, in2] [, in3] [, ...]" :doc "Read signals from a file at i-rate.") csdoc-opcode-database)
(puthash "STKDrummer" '(:template "asignal STKDrummer ifrequency, iamplitude" :doc "STKDrummer is a drum sampling synthesizer.") csdoc-opcode-database)
(puthash "vdelayxq" '(:template "aout1, aout2, aout3, aout4 vdelayxq ain1, ain2, ain3, ain4, adl, imd, iws [, ist]" :doc "A 4-channel variable delay opcode with high quality interpolation.") csdoc-opcode-database)
(puthash "aresonk" '(:template "kres aresonk ksig, kcf, kbw [, iscl] [, iskip]" :doc "A notch filter whose transfer functions are the complements of the reson opcode.") csdoc-opcode-database)
(puthash "inz" '(:template "inz ksig1" :doc "Reads multi-channel audio samples into a ZAK array from an external device or stream.") csdoc-opcode-database)
(puthash "vbap16" '(:template "ar1, ..., ar16 vbap16 asig, kazim [, kelev] [, kspread]" :doc "Distributes an audio signal among 16 channels.") csdoc-opcode-database)
(puthash "link_tempo_set" '(:template "link_tempo_set i_peer, k_bpm [, k_at_time_seconds]" :doc "Sets the tempo.") csdoc-opcode-database)
(puthash "if" '(:template "if ia R ib igoto label
if ka R kb kgoto label
if xa R xb goto label
if xa R xb then" :doc "Branches conditionally at initialization or during performance time.") csdoc-opcode-database)
(puthash "endin" '(:template "endin" :doc "Ends the current instrument block.") csdoc-opcode-database)
(puthash "pan2" '(:template "a1, a2 pan2 asig, xp [, imode]" :doc "Distribute an audio signal across two channels.") csdoc-opcode-database)
(puthash "FLsetColor" '(:template "FLsetColor ired, igreen, iblue, ihandle" :doc "Sets the primary color of a FLTK widget.") csdoc-opcode-database)
(puthash "vmultv_i" '(:template "vmultv_i ifn1, ifn2, ielements [, idstoffset] [, isrcoffset]" :doc "Performs mutiplication between two vectorial control signals at init time.") csdoc-opcode-database)
(puthash "cpsxpch" '(:template "icps cpsxpch ipch, iequal, irepeat, ibase" :doc "Converts a pitch-class value into cycles-per-second (Hz) for equal divisions of any interval.") csdoc-opcode-database)
(puthash "ino" '(:template "ar1, ar2, ar3, ar4, ar5, ar6, ar7, ar8 ino" :doc "Reads eight-channel audio data from an external device or stream.") csdoc-opcode-database)
(puthash "lowpass2" '(:template "ares lowpass2 asig, kcf, kq [, iskip]" :doc "A resonant lowpass filter.") csdoc-opcode-database)
(puthash "link_beat_force" '(:template "link_beat_force i_peer, k_beat [, k_at_time_seconds [, k_quantum ]]" :doc "Forces the global network Ableton Link session to adopt a specific beat number and time.") csdoc-opcode-database)
(puthash "websocket" '(:template "xout1[, xout2, xout3, ..., xoutN] websocket iport, xin" :doc "Read and write signals and arrays using a websocket connection.") csdoc-opcode-database)
(puthash "display" '(:template "display xsig, iprd [, inprds] [, iwtflg]" :doc "Displays the audio or control signals as an amplitude vs. time graph.") csdoc-opcode-database)
(puthash "locsend" '(:template "a1, a2 locsend
a1, a2, a3, a4 locsend" :doc "Distributes the audio signals of a previous") csdoc-opcode-database)
(puthash "adsynt2" '(:template "ar adsynt2 kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]" :doc "Performs additive synthesis with an arbitrary number of partials -not necessarily harmonic- with interpolation.") csdoc-opcode-database)
(puthash "STKRhodey" '(:template "asignal STKRhodey ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STK Fender Rhodes-like electric piano FM synthesis instrument.") csdoc-opcode-database)
(puthash "cauchy" '(:template "ares cauchy kalpha
ires cauchy kalpha
kres cauchy kalpha" :doc "WTF: ") csdoc-opcode-database)
(puthash "getcfg" '(:template "Svalue getcfg iopt" :doc "Return Csound settings.") csdoc-opcode-database)
(puthash "==" '(:template "(a == b ? v1 : v2)" :doc "WTF: ") csdoc-opcode-database)
(puthash "clockon" '(:template "clockon inum" :doc "Starts one of a number of internal clocks.") csdoc-opcode-database)
(puthash "gausstrig" '(:template "ares gausstrig kamp, kcps, kdev [, imode] [, ifrst1]
kres gausstrig kamp, kcps, kdev [, imode] [, ifrst1]" :doc "Random impulses around a certain frequency.") csdoc-opcode-database)
(puthash "copyf2array" '(:template "copyf2array tab, kftbl" :doc "Copy data from an f-table to a vector.") csdoc-opcode-database)
(puthash "opbitnot" '(:template "~ a (bitwise NOT)" :doc "Bitwise NOT operator.") csdoc-opcode-database)
(puthash "expcurve" '(:template "kout expcurve kindex, ksteepness" :doc "This opcode implements a formula for generating a normalised exponential curve in range 0 - 1. It is based on the Max / MSP work of Eric Singer (c) 1994.") csdoc-opcode-database)
(puthash "diskgrain" '(:template "asig diskgrain Sfname, kamp, kfreq, kpitch, kgrsize, kprate, ifun, iolaps [,imaxgrsize , ioffset]" :doc "Synchronous granular synthesis, using a soundfile as source.") csdoc-opcode-database)
(puthash "fractalnoise" '(:template "ares fractalnoise kamp, kbeta" :doc "A fractal noise generator.") csdoc-opcode-database)
(puthash "loop_gt" '(:template "loop_gt indx, idecr, imin, label
loop_gt kndx, kdecr, kmin, label" :doc "Looping constructions.") csdoc-opcode-database)
(puthash "expon" '(:template "ares expon ia, idur, ib
kres expon ia, idur, ib" :doc "Trace an exponential curve between specified points.") csdoc-opcode-database)
(puthash "pvsynth" '(:template "ares pvsynth fsrc, [iinit]" :doc "Resynthesise using a FFT overlap-add.") csdoc-opcode-database)
(puthash "tabmorphak" '(:template "aout tabmorphak aindex, kweightpoint, ktabnum1, ktabnum2, ifn1, ifn2 [, ifn3, ifn4, ... ifnN]" :doc "Allow morphing between a set of tables at audio rate with interpolation.") csdoc-opcode-database)
(puthash "vexp_i" '(:template "vexp_i ifn, ival, ielements[, idstoffset]" :doc "Performs power-of operations between a vector and a scalar") csdoc-opcode-database)
(puthash "table3" '(:template "ares table3 andx, ifn [, ixmode] [, ixoff] [, iwrap]
ires table3 indx, ifn [, ixmode] [, ixoff] [, iwrap]
kres table3 kndx, ifn [, ixmode] [, ixoff] [, iwrap]" :doc "Accesses table values by direct indexing with cubic interpolation.") csdoc-opcode-database)
(puthash "connect" '(:template "connect Tsource1, Soutlet1, Tsink1, Sinlet1" :doc "Connects a source outlet to a sink inlet.") csdoc-opcode-database)
(puthash "clip" '(:template "ares clip asig, imeth, ilimit [, iarg]" :doc "Clips a signal to a predefined limit.") csdoc-opcode-database)
(puthash "vpvoc" '(:template "ares vpvoc ktimpnt, kfmod, ifile [, ispecwp] [, ifn]" :doc "Implements signal reconstruction using an fft-based phase vocoder and an extra envelope.") csdoc-opcode-database)
(puthash "wgpluck" '(:template "ares wgpluck icps, iamp, kpick, iplk, idamp, ifilt, axcite" :doc "A high fidelity simulation of a plucked string.") csdoc-opcode-database)
(puthash "STKModalBar" '(:template "asignal STKModalBar ifrequency, iamplitude, [khard, kv1[, kpos, kv2[, klfo, kv3[, klfodepth, kv4[, kmix, kv5[, kvol, kv6[, kinstr, kv7]]]]]]]" :doc "STKModalBar is a resonant bar instrument.") csdoc-opcode-database)
(puthash "MixerSetLevel_i" '(:template "MixerSetLevel_i isend, ibuss, igain" :doc "Sets the level of a send to a buss.") csdoc-opcode-database)
(puthash "vpowv" '(:template "vpowv ifn1, ifn2, kelements [, kdstoffset] [, ksrcoffset] [,kverbose]" :doc "Performs power-of operations between two vectorial control signals") csdoc-opcode-database)
(puthash "cudasliding" '(:template "asig cudasliding ain, amod, iwinsize" :doc "Perform sliding phase vocoder algorithm with simplified
      transformational FM using GPU hardware. Experimental and
      only available as source code at the moment.") csdoc-opcode-database)
(puthash "vtabwi" '(:template "vtabwi indx, ifn, inarg1 [, inarg2, inarg3 , .... , inargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "vmult_i" '(:template "vmult_i ifn, ival, ielements [, idstoffset]" :doc "Multiplies a vector in a table by a scalar value.") csdoc-opcode-database)
(puthash "writescratch" '(:template "writescratchival[, index]" :doc "writes a value into the scratchpad of the instance of an instrument.") csdoc-opcode-database)
(puthash "chnmix" '(:template "chnmix aval, Sname" :doc "Writes audio data to the named software bus, mixing to the previous
      output.") csdoc-opcode-database)
(puthash "dcblock" '(:template "ares dcblock ain [, igain]" :doc "A DC blocking filter.") csdoc-opcode-database)
(puthash "FLprintk" '(:template "FLprintk itime, kval, idisp" :doc "A FLTK opcode that prints a k-rate value at specified intervals.") csdoc-opcode-database)
(puthash "lpread" '(:template "krmsr, krmso, kerr, kcps lpread ktimpnt, ifilcod [, inpoles] [, ifrmrate]" :doc "Reads a control file of time-ordered information frames.") csdoc-opcode-database)
(puthash "beadsynt" '(:template "aout beadsynt kFreqs[], kAmps[], kBws[] [, inumosc, iflags, kfreq, kbw, ifn, iphs ]
aout beadsynt ifreqft, iampft, ibwft, inumosc [, iflags, kfreq, kbw, ifn, iphs ]" :doc "Band-Enhanced Oscillator-Bank") csdoc-opcode-database)
(puthash "oscil" '(:template "ares oscil xamp, xcps [, ifn, iphs]
kres oscil kamp, kcps [, ifn, iphs]" :doc "A simple oscillator.") csdoc-opcode-database)
(puthash "ATSinfo" '(:template "idata ATSinfo iatsfile, ilocation" :doc "reads data out of the header of an ATS file.") csdoc-opcode-database)
(puthash "adsynt" '(:template "ares adsynt kamp, kcps, iwfn, ifreqfn, iampfn, icnt [, iphs]" :doc "Performs additive synthesis with an arbitrary number of partials, not necessarily harmonic.") csdoc-opcode-database)
(puthash "outs1" '(:template "outs1 asig" :doc "Writes samples to stereo channel 1 of an external device or stream.") csdoc-opcode-database)
(puthash "faustgen" '(:template "ihandle,a1[,a2,...] faustgen SCode[,ain1,...]" :doc "Compiles, Instantiates and runs a compiled Faust program.") csdoc-opcode-database)
(puthash "rfft" '(:template "kout[] rfft kin[]" :doc "Fast Fourier Transform of a real-value array.") csdoc-opcode-database)
(puthash "fmbell" '(:template "ares fmbell kamp, kfreq, kc1, kc2, kvdepth, kvrate[, ifn1, ifn2, ifn3, ifn4, ivfn, isus]" :doc "Uses FM synthesis to create a tublar bell sound.") csdoc-opcode-database)
(puthash "zar" '(:template "ares zar kndx" :doc "Reads from a location in za space at a-rate.") csdoc-opcode-database)
(puthash "k35_lpf" '(:template "asig K35_lpf ain, xcf, xQ [, inlp, isaturation, istor]" :doc "Zero-delay feedback implementation of Korg35 resonant low-pass filter.") csdoc-opcode-database)
(puthash "mirror" '(:template "ares mirror asig, klow, khigh
ires mirror isig, ilow, ihigh
kres mirror ksig, klow, khigh" :doc "Reflects the signal that exceeds the low and high thresholds.") csdoc-opcode-database)
(puthash "ntof" '(:template "kfreq ntof Snote
ifreq ntof Snote" :doc "Convert note name to frequency") csdoc-opcode-database)
(puthash "vtabk" '(:template "vtabk kndx, ifn, kout1 [, kout2, kout3, .... , koutN ]" :doc "Read vectors (from tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "spectrum" '(:template "wsig spectrum xsig, iprd, iocts, ifrqa [, iq] [, ihann] [, idbout] [, idsprd] [, idsinrs]" :doc "Generate a constant-Q, exponentially-spaced DFT.") csdoc-opcode-database)
(puthash "cpspch" '(:template "cpspch (pch) (init- or control-rate args only)" :doc "Converts a pitch-class value to cycles-per-second.") csdoc-opcode-database)
(puthash "distort1" '(:template "ares distort1 asig, kpregain, kpostgain, kshape1, kshape2[, imode]" :doc "Modified hyperbolic tangent distortion.") csdoc-opcode-database)
(puthash "randomh" '(:template "ares randomh kmin, kmax, xcps [,imode] [,ifirstval]
kres randomh kmin, kmax, kcps [,imode] [,ifirstval]" :doc "Generates random numbers with a user-defined limit and holds them for a period of time.") csdoc-opcode-database)
(puthash "outs2" '(:template "outs2 asig" :doc "Writes samples to stereo channel 2 of an external device or stream.") csdoc-opcode-database)
(puthash "sqrt" '(:template "sqrt(x) (no rate restriction)
sqrt(k/i[]) (k- or i-arrays )" :doc "Returns a square root value.") csdoc-opcode-database)
(puthash "envlpx" '(:template "ares envlpx xamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]
kres envlpx kamp, irise, idur, idec, ifn, iatss, iatdec [, ixmod]" :doc "Applies an envelope consisting of 3 segments.") csdoc-opcode-database)
(puthash "gogobel" '(:template "ares gogobel kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivfn" :doc "Audio output is a tone related to the striking of a cow bell or similar.") csdoc-opcode-database)
(puthash "delay1" '(:template "ares delay1 asig [, iskip]" :doc "Delays an input signal by one sample.") csdoc-opcode-database)
(puthash "mvclpf2" '(:template "asig mvclpf2 ain, xcf, xres[, istor]" :doc "Moog voltage-controlled lowpass filter emulation.") csdoc-opcode-database)
(puthash "strsubk" '(:template "Sdst strsubk Ssrc, kstart, kend" :doc "Extract a substring") csdoc-opcode-database)
(puthash "vtablewk" '(:template "vtablewk kndx, kfn, ixmode, kinarg1 [, kinarg2, kinarg3 , .... , kinargN ]" :doc "Write vectors (to tables -or arrays of vectors).") csdoc-opcode-database)
(puthash "getcol" '(:template "i/kout[] getcoli/kin[],i/kcol" :doc "Gets a given column from a 2-dimensional array as a vector.") csdoc-opcode-database)
(puthash "imageload" '(:template "iimagenum imageload filename" :doc "Load an image.") csdoc-opcode-database)
(puthash "FLxyin" '(:template "koutx, kouty, kinside FLxyin ioutx_min, ioutx_max, iouty_min, iouty_max, iwindx_min, iwindx_max, iwindy_min, iwindy_max [, iexpx, iexpy, ioutx, iouty]" :doc "Senses the mouse cursor position in a user-defined area inside an FLpanel.") csdoc-opcode-database)
(puthash "zir" '(:template "ir zir indx" :doc "Reads from a location in zk space at i-rate.") csdoc-opcode-database)
(puthash "lenarray" '(:template "ir lenarray karray[, iwhich]
kr lenarray karray[, iwhich]" :doc "Evaluates the size or shape length of an array.") csdoc-opcode-database)
(puthash "samphold" '(:template "ares samphold asig, agate [, ival] [, ivstor]
kres samphold ksig, kgate [, ival] [, ivstor]" :doc "Performs a sample-and-hold operation on its input.") csdoc-opcode-database)
(puthash "follow2" '(:template "ares follow2 asig, katt, krel" :doc "Another controllable envelope extractor.") csdoc-opcode-database)
(puthash "opcode" '(:template "opcode name, outtypes, intypes" :doc "Defines the start of user-defined opcode block.") csdoc-opcode-database)
(puthash "pvs2tab" '(:template "kframe pvs2tab tvar|kvar[], fsig
kframe pvs2tab kmags[], kfreqs[], fsig" :doc "Copies spectral data to k-rate arrays (or t-variables). Also known as pvs2array.") csdoc-opcode-database)
(puthash "sprintfk" '(:template "Sdst sprintfk Sfmt, xarg1[, xarg2[, ... ]]" :doc "printf-style formatted output to a string variable at k-rate.") csdoc-opcode-database)
(puthash "mtof" '(:template "ifreq mtof imidi
kfreq mtof kmidi
ifreqs[] mtof imidis[]
kfreqs[] mtof kmidis[]" :doc "Convert a midi to frequency") csdoc-opcode-database)
(puthash "trsplit" '(:template "fsiglow, fsighi trsplit fin, ksplit[, kgainlow, kgainhigh]" :doc "Streaming partial track frequency splitting.") csdoc-opcode-database)
(puthash "timout" '(:template "timout istrt, idur, label" :doc "Conditional branch during p-time depending on elapsed note time.") csdoc-opcode-database)
(puthash "plltrack" '(:template "acps, alock plltrack asig, kd [, kloopf, kloopq, klf, khf, kthresh]" :doc "Tracks the pitch of a signal.") csdoc-opcode-database)
(puthash "tvconv" '(:template "ares tvconv asig1, asig2, xfreez1, xfreez2, iparts, ifils" :doc "A time-varying convolution (FIR filter) opcode.") csdoc-opcode-database)
(puthash "hrtfstat" '(:template "aleft, aright hrtfstat asrc, iAz, iElev, ifilel, ifiler [,iradius, isr]" :doc "Generates static 3d binaural audio for headphones using a
      Woodworth based spherical head model with improved low frequency
      phase accuracy.") csdoc-opcode-database)
(puthash "lpinterp" '(:template "lpinterp islot1, islot2, kmix" :doc "lpinterp") csdoc-opcode-database)
(puthash "statevar" '(:template "ahp,alp,abp,abr statevar ain, xcf, xq [, iosamps, istor]" :doc "State-variable filter.") csdoc-opcode-database)
(puthash "pyinit" '(:template "pyinit" :doc "Initialize the Python interpreter.") csdoc-opcode-database)
(puthash "inh" '(:template "ar1, ar2, ar3, ar4, ar5, ar6 inh" :doc "Reads six-channel audio data from an external device or stream.") csdoc-opcode-database)
(puthash "partikkelsync" '(:template "async [,aphase] partikkelsync iopcode_id" :doc "Outputs") csdoc-opcode-database)
(puthash "vlimit" '(:template "vlimit ifn, kmin, kmax, ielements" :doc "Limiting and Wrapping Vectorial Signals") csdoc-opcode-database)
(puthash "specaddm" '(:template "wsig specaddm wsig1, wsig2 [, imul2]" :doc "Perform a weighted add of two input spectra.") csdoc-opcode-database)
(puthash "MixerGetLevel" '(:template "kgain MixerGetLevel isend, ibuss" :doc "Gets the level of a send to a buss.") csdoc-opcode-database)
(puthash "oscil1" '(:template "kres oscil1 idel, kamp, idur [, ifn]" :doc "Accesses table values by incremental sampling.") csdoc-opcode-database)
(puthash "readf" '(:template "Sres, kline readf ifilname" :doc "Read a line of text from an external file.") csdoc-opcode-database)
(puthash "vexp" '(:template "vexp ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "Performs power-of operations between a vector and a scalar") csdoc-opcode-database)
(puthash "dust2" '(:template "ares dust2 kamp, kdensity
kres dust2 kamp, kdensity" :doc "Random impulses.") csdoc-opcode-database)
(puthash "imagesize" '(:template "iwidth, iheight imagesize iimagenum" :doc "Return the width and height of a previously opened or created image.") csdoc-opcode-database)
(puthash "mclock" '(:template "mclock ifreq" :doc "Sends a MIDI CLOCK message.") csdoc-opcode-database)
(puthash "strrindexk" '(:template "kpos strrindexk S1, S2" :doc "Return the position of the last occurence of a string in another string") csdoc-opcode-database)
(puthash "chnexport" '(:template "gival chnexport Sname, imode[, itype, idflt, imin, imax]
gkval chnexport Sname, imode[, itype, idflt, imin, imax]
gaval chnexport Sname, imode
gSval chnexport Sname, imode" :doc "Export a global variable as a channel of the bus.") csdoc-opcode-database)
(puthash "lposcila" '(:template "ar lposcila aamp, kfreqratio, kloop, kend, ift [,iphs]" :doc "Read sampled sound from a table with looping and high precision.") csdoc-opcode-database)
(puthash "filebit" '(:template "ir filebit ifilcod [, iallowraw]" :doc "Returns the number of bits in each sample in a sound file.") csdoc-opcode-database)
(puthash "outic" '(:template "outic ichn, inum, ivalue, imin, imax" :doc "Sends MIDI controller output at i-rate.") csdoc-opcode-database)
(puthash "tab2array" '(:template "kout[] tab2array ifn [, kstart, kend, kstep ]
iout[] tab2array ifn [, istart, iend, istep ]" :doc "Copy a slice from an f-table to an array") csdoc-opcode-database)
(puthash "wiirange" '(:template "wiirange icontrol, iminimum, imaximum[, inum]" :doc "Sets scaling and range limits for certain Wiimote fields.") csdoc-opcode-database)
(puthash "vmult" '(:template "vmult ifn, kval, kelements [, kdstoffset] [, kverbose]" :doc "Multiplies a vector in a table by a scalar value.") csdoc-opcode-database)
(puthash "sfpreset" '(:template "ir sfpreset iprog, ibank, ifilhandle, ipreindex" :doc "Assigns an existing preset of a SoundFont2 (SF2) sample file to an index number.") csdoc-opcode-database)
(puthash "notnum" '(:template "ival notnum" :doc "Get a note number from a MIDI event.") csdoc-opcode-database)
(puthash "marimba" '(:template "ares marimba kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp, ivibfn, idec [, idoubles] [, itriples]" :doc "Physical model related to the striking of a wooden block.") csdoc-opcode-database)
(puthash "mvclpf3" '(:template "asig mvclpf3 ain, xcf, xres[, istor]" :doc "Moog voltage-controlled lowpass filter emulation.") csdoc-opcode-database)
(puthash "vbap4move" '(:template "ar1, ar2, ar3, ar4 vbap4move asig, idur, ispread, ifldnum, ifld1 [, ifld2] [...]" :doc "Distributes an audio signal among 4 channels with moving virtual sources.") csdoc-opcode-database)
(puthash "fluidLoad" '(:template "isfnum fluidLoad soundfont, ienginenum[, ilistpresets]" :doc "Loads a SoundFont into a fluidEngine, optionally listing SoundFont contents.") csdoc-opcode-database)
(puthash "getseed" '(:template "ians getseed
kans getseed" :doc "Reads the global seed value.") csdoc-opcode-database)
(puthash "metro" '(:template "ktrig metro kfreq [, initphase]" :doc "Trigger Metronome") csdoc-opcode-database)
(puthash "slider64table" '(:template "kflag slider64table ichan, ioutTable, ioffset, ictlnum1, imin1, imax1, init1, ifn1, .... , ictlnum64, imin64, imax64, init64, ifn64" :doc "Stores a bank of 64 different MIDI control messages to a table.") csdoc-opcode-database)
(puthash "tonex" '(:template "ares tonex asig, khp [, inumlayer] [, iskip]
ares tonex asig, ahp [, inumlayer] [, iskip]" :doc "Emulates a stack of filters using the tone opcode.") csdoc-opcode-database)
(puthash "ktableseg" '(:template "ktableseg ifn1, idur1, ifn2 [, idur2] [, ifn3] [...]" :doc "WTF: ") csdoc-opcode-database)
(puthash "ATSread" '(:template "kfreq, kamp ATSread ktimepnt, iatsfile, ipartial" :doc "reads data from an ATS file.") csdoc-opcode-database)
(puthash "FLprintk2" '(:template "FLprintk2 kval, idisp" :doc "A FLTK opcode that prints a new value every time a control-rate variable changes.") csdoc-opcode-database)
(puthash "flooper2" '(:template "asig1[,asig2] flooper2 kamp, kpitch, kloopstart, kloopend, kcrossfade, ifn [, istart, imode, ifenv, iskip]" :doc "Function-table-based crossfading looper.") csdoc-opcode-database)
(puthash "FLgroupEnd" '(:template "FLgroupEnd" :doc "Marks the end of a group of FLTK child widgets.") csdoc-opcode-database)
(puthash "butbr" '(:template "ares butbr asig, kfreq, kband [, iskip]" :doc "Same as the butterbr opcode.") csdoc-opcode-database)
(puthash "mandol" '(:template "ares mandol kamp, kfreq, kpluck, kdetune, kgain, ksize [, ifn] [, iminfreq]" :doc "An emulation of a mandolin.") csdoc-opcode-database)
(puthash "wguide2" '(:template "ares wguide2 asig, xfreq1, xfreq2, kcutoff1, kcutoff2, kfeedback1, kfeedback2" :doc "A model of beaten plate consisting of two parallel delay-lines and two first-order lowpass filters.") csdoc-opcode-database)
(puthash "interp" '(:template "ares interp ksig [, iskip] [, imode] [, ivalue]" :doc "Converts a control signal to an audio signal using linear interpolation.") csdoc-opcode-database)
(puthash "sekere" '(:template "ares sekere iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "Semi-physical model of a sekere sound.") csdoc-opcode-database)
(puthash "pan" '(:template "a1, a2, a3, a4 pan asig, kx, ky, ifn [, imode] [, ioffset]" :doc "Distribute an audio signal amongst four channels.") csdoc-opcode-database)
(puthash "pop" '(:template "xval1, [xval2, ... , xval31] pop
ival1, [ival2, ... , ival31] pop" :doc "WTF: ") csdoc-opcode-database)
(puthash "specdiff" '(:template "wsig specdiff wsigin" :doc "Finds the positive difference values between consecutive spectral frames.") csdoc-opcode-database)
(puthash "r2c" '(:template "kout[] r2c kin[]" :doc "Real to complex format conversion.") csdoc-opcode-database)
(puthash "STKSaxofony" '(:template "asignal STKSaxofony ifrequency, iamplitude, [kstiff, kv1[, kapert, kv2[, kblow, kv3[, knoise, kv4[, klfo, kv5[, klfodepth, kv6[, kbreath, kv7]]]]]]]" :doc "STKSaxofony is a faux conical bore reed instrument.") csdoc-opcode-database)
(puthash "noteondur" '(:template "noteondur ichn, inum, ivel, idur" :doc "Sends a noteon and a noteoff MIDI message both with the same channel, number and velocity.") csdoc-opcode-database)
(puthash "soundout" '(:template "soundout asig1, ifilcod [, iformat]" :doc "Deprecated. Writes audio output to a disk file.") csdoc-opcode-database)
(puthash "stix" '(:template "ares stix iamp, idettack [, inum] [, idamp] [, imaxshake]" :doc "Semi-physical model of a stick sound.") csdoc-opcode-database)
(puthash "sflooper" '(:template "ar1, ar2 sflooper ivel, inotenum, kamp, kpitch, ipreindex, kloopstart, kloopend, kcrossfade [, istart, imode, ifenv, iskip]" :doc "Plays a SoundFont2 (SF2) sample preset, generating a stereo sound, with user-defined
      time-varying crossfade looping.") csdoc-opcode-database)
(puthash "outx" '(:template "outx asig1, asig2, asig3, asig4, asig5, asig6, asig7, asig8, asig9, asig10, asig11, asig12, asig13, asig14, asig15, asig16" :doc "Writes 16-channel audio data to an external device or stream.") csdoc-opcode-database)
(puthash "dssiaudio" '(:template "[aout1, aout2, ..., aout9] dssiaudio ihandle, [ain1, ain2, ..., ain9]" :doc "Processes audio using a LADSPA or DSSI plugin.") csdoc-opcode-database)
(puthash "crossfm" '(:template "a1, a2 crossfm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crosspm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crosspmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmpm xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]
a1, a2 crossfmpmi xfrq1, xfrq2, xndx1, xndx2, kcps, ifn1, ifn2 [, iphs1] [, iphs2]" :doc "Two mutually frequency and/or phase modulated oscillators.") csdoc-opcode-database)
(puthash "strlowerk" '(:template "Sdst strlowerk Ssrc" :doc "Convert a string to lower case") csdoc-opcode-database)
(puthash "deltapx" '(:template "aout deltapx adel, iwsize" :doc "Read from or write to a delay line with interpolation.") csdoc-opcode-database)
(puthash "ficlose" '(:template "ficlose ihandle
ficlose Sfilename" :doc "Closes a previously opened file.") csdoc-opcode-database)
(puthash "tan" '(:template "tan(x) (no rate restriction)
tan(k/i[]) (k- or i-arrays )" :doc "Performs a tangent function.") csdoc-opcode-database)
(puthash "mvchpf" '(:template "asig mvchpf ain, xcf[, istor]" :doc "Moog voltage-controlled highpass filter emulation.") csdoc-opcode-database)
(puthash "puts" '(:template "puts Sstr, ktrig[, inonl]" :doc "Print a string constant or variable") csdoc-opcode-database)
(puthash "unirand" '(:template "ares unirand krange
ires unirand krange
kres unirand krange" :doc "WTF: ") csdoc-opcode-database)
(puthash "bformenc" '(:template "aw, ax, ay, az bformenc asig, kalpha, kbeta, kord0, kord1
aw, ax, ay, az, ar, as, at, au, av bformenc asig, kalpha, kbeta, kord0, kord1 , kord2
aw, ax, ay, az, ar, as, at, au, av, ak, al, am, an, ao, ap, aq bformenc asig, kalpha, kbeta, kord0, kord1, kord2, kord3" :doc "Deprecated. Codes a signal into the ambisonic B format.") csdoc-opcode-database)
(puthash "while" '(:template "while condition do ... od" :doc "A syntactic looping construction.") csdoc-opcode-database)
(puthash "butterbr" '(:template "ares butterbr asig, xfreq, xband [, iskip]" :doc "A band-reject Butterworth filter.") csdoc-opcode-database)
(puthash "rireturn" '(:template "rireturn" :doc "Terminates a reinit pass.") csdoc-opcode-database)
(puthash "cggoto" '(:template "cggoto condition, label" :doc "Conditionally transfer control on every pass.") csdoc-opcode-database)
(puthash "rand" '(:template "ares rand xamp [, iseed] [, isel] [, ioffset]
kres rand xamp [, iseed] [, isel] [, ioffset]" :doc "Generates a controlled random number series.") csdoc-opcode-database)
(puthash "zawm" '(:template "zawm asig, kndx [, imix]" :doc "Writes to a za variable at a-rate with mixing.") csdoc-opcode-database)
(puthash "tablewa" '(:template "kstart tablewa kfn, asig, koff" :doc "Writes tables in sequential locations.") csdoc-opcode-database)
(puthash "outkpat" '(:template "outkpat kchn, knotenum, kvalue, kmin, kmax" :doc "Sends polyphonic MIDI aftertouch messages at k-rate.") csdoc-opcode-database)
(puthash "strset" '(:template "strset iarg, istring" :doc "Allows a string to be linked with a numeric value.") csdoc-opcode-database)
(puthash "vcopy_i" '(:template "vcopy_i ifn1, ifn2, ielements [,idstoffset, isrcoffset]" :doc "Copies a vector from one table to another.") csdoc-opcode-database)
(puthash "upsamp" '(:template "ares upsamp ksig" :doc "Modify a signal by up-sampling.") csdoc-opcode-database)
(puthash "FLsetTextColor" '(:template "FLsetTextColor ired, iblue, igreen, ihandle" :doc "Sets the color of the text label of a FLTK widget.") csdoc-opcode-database)
(puthash "tabplay" '(:template "tabplay ktrig, knumtics, kfn, kout1 [,kout2,..., koutN]" :doc "Playing-back control signals.") csdoc-opcode-database)
(puthash "vaget" '(:template "kval vaget kndx, avar" :doc "Access values of the current buffer of an a-rate variable by indexing.") csdoc-opcode-database)
(puthash "vadd_i" '(:template "vadd_i ifn, ival, ielements [, idstoffset]" :doc "Adds a scalar value to a vector in a table.") csdoc-opcode-database)
(puthash "ephasor" '(:template "aexp,aph ephasor kfreq, kR" :doc "produces two outputs: a periodic phase signal and a periodic exponential decaying signal.") csdoc-opcode-database)
(puthash "button" '(:template "kres button knum" :doc "Sense on-screen controls.") csdoc-opcode-database)
(puthash "STKMandolin" '(:template "asignal STKMandolin ifrequency, iamplitude, [kbody, kv1[, kpos, kv2[, ksus, kv3[, kdetune, kv4[, kmic, kv5]]]]]" :doc "STKMandolin produces mamdolin-like sounds.") csdoc-opcode-database)
(puthash "fmwurlie" '(:template "ares fmwurlie kamp, kfreq, kc1, kc2, kvdepth, kvrate, ifn1, ifn2, ifn3, ifn4, ivfn" :doc "Uses FM synthesis to create a Wurlitzer electric piano sound.") csdoc-opcode-database)
(puthash "outh" '(:template "outh asig1, asig2, asig3, asig4, asig5, asig6" :doc "Writes 6-channel audio data to an external device or stream.") csdoc-opcode-database)
(puthash "wgclar" '(:template "ares wgclar kamp, kfreq, kstiff, iatt, idetk, kngain, kvibf, kvamp [, ifn] [, iminfreq]" :doc "Creates a tone similar to a clarinet.") csdoc-opcode-database)
(puthash "ATSpartialtap" '(:template "kfrq, kamp ATSpartialtap ipartialnum" :doc "returns a frequency, amplitude pair from an") csdoc-opcode-database)
(puthash "inch" '(:template "ain1[, ...] inch kchan1[,...]" :doc "Reads from numbered channels in an external audio signal or stream.") csdoc-opcode-database)
(puthash "pvsbin" '(:template "kamp, kfr pvsbin fsig, kbin" :doc "Obtain the amp and freq values off a PVS signal bin.") csdoc-opcode-database)
(puthash "nrpn" '(:template "nrpn kchan, kparmnum, kparmvalue" :doc "Sends a Non-Registered Parameter Number to the MIDI OUT port.") csdoc-opcode-database)
(puthash "pitchamdf" '(:template "kcps, krms pitchamdf asig, imincps, imaxcps [, icps] [, imedi] [, idowns] [, iexcps] [, irmsmedi]" :doc "Follows the pitch of a signal based on the AMDF method.") csdoc-opcode-database)
(puthash "osciln" '(:template "ares osciln kamp, ifrq, ifn, itimes" :doc "Accesses table values at a user-defined frequency.") csdoc-opcode-database)
(puthash "port" '(:template "kres port ksig, ihtim [, isig]" :doc "Applies portamento to a step-valued control signal.") csdoc-opcode-database)
(puthash "ATSaddnz" '(:template "ar ATSaddnz ktimepnt, iatsfile, ibands[, ibandoffset, ibandincr]" :doc "uses the data from an ATS analysis file to perform noise resynthesis.") csdoc-opcode-database)
(puthash "cpstuni" '(:template "icps cpstuni index, ifn" :doc "Returns micro-tuning values at init-rate.") csdoc-opcode-database)
(puthash "strrindex" '(:template "ipos strrindex S1, S2" :doc "Return the position of the last occurence of a string in another string") csdoc-opcode-database)
(puthash "pvcross" '(:template "ares pvcross ktimpnt, kfmod, ifile, kampscale1, kampscale2 [, ispecwp]" :doc "Applies the amplitudes from one phase vocoder analysis file to the data from a second file.") csdoc-opcode-database)
(puthash "link_metro" '(:template "k_trigger, k_beat, k_phase, k_current_time_seconds link_metro i_peer [, k_quantum]" :doc "Returns a trigger that is 1 on the beat and 0 otherwise along with beat, phase, and time for this session of Ableton Link.") csdoc-opcode-database)
(puthash "ctrlinit" '(:template "ctrlinit ichnl, ictlno1, ival1 [, ictlno2] [, ival2] [, ictlno3] [, ival3] [,...ival32]" :doc "Sets the initial values for a set of MIDI controllers.") csdoc-opcode-database)
(puthash "vmirror" '(:template "vmirror ifn, kmin, kmax, ielements" :doc "Limiting and Wrapping Vectorial Signals") csdoc-opcode-database)
(puthash "MixerReceive" '(:template "asignal MixerReceive ibuss, ichannel" :doc "Receives an arate signal from a channel of a buss.") csdoc-opcode-database)
(puthash "ziwm" '(:template "ziwm isig, indx [, imix]" :doc "Writes to a zk variable to an i-rate variable with mixing.") csdoc-opcode-database)
(puthash "sc_phasor" '(:template "aindex sc_phasor xtrig, xrate, kstart, kend [, kresetPos]
kindex sc_phasor xtrig, xrate, kstart, kend [, kresetPos]" :doc "A resettable linear ramp between two levels") csdoc-opcode-database)
(puthash "tradsyn" '(:template "asig tradsyn fin, kscal, kpitch, kmaxtracks, ifn" :doc "Streaming partial track additive synthesis") csdoc-opcode-database)
(puthash "prepiano" '(:template "ares prepiano ifreq, iNS, iD, iK, iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, isspread[, irattles, irubbers]
al,ar prepiano ifreq, iNS, iD, iK, iT30,iB, kbcl, kbcr, imass, ihvfreq, iinit, ipos, ivel, isfreq, isspread[, irattles, irubbers]" :doc "Creates a tone similar to a piano string prepared in a Cageian fashion.") csdoc-opcode-database)
(puthash "turnoff2" '(:template "turnoff2 kinsno, kmode, krelease" :doc "Turn off instance(s) of other instruments at performance time.") csdoc-opcode-database)
(puthash "!=" '(:template "(a != b ? v1 : v2)" :doc "WTF: ") csdoc-opcode-database)
(puthash "imagegetpixel" '(:template "ared, agreen, ablue imagegetpixel iimagenum, ax, ay
kred, kgreen, kblue imagegetpixel iimagenum, kx, ky" :doc "Return the RGB pixel values of a previously opened or created image.") csdoc-opcode-database)
(puthash "release" '(:template "kflag release" :doc "Indicates whether a note is in its") csdoc-opcode-database)
(puthash "ceps" '(:template "keps[] ceps kmags[][, icoefs]" :doc "Calculate the cepstrum of an array input, optionally liftering coefficients.") csdoc-opcode-database)
(puthash "fluidControl" '(:template "fluidControl ienginenum, kstatus, kchannel, kdata1, kdata2 [,imsgs]" :doc "Sends MIDI note on, note off, and other messages to a SoundFont preset.") csdoc-opcode-database)
(puthash "bformdec1" '(:template "ao1, ao2 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]
ao1, ao2, ao3, ao4, ao5, ao6, ao7, ao8 bformdec1 isetup, aw, ax, ay, az [, ar, as, at, au, av [, abk, al, am, an, ao, ap, aq]]]
aout[] bformdec1 isetup, abform[]" :doc "Decodes an ambisonic B format signal") csdoc-opcode-database)
(puthash "midiout" '(:template "midiout kstatus, kchan, kdata1, kdata2" :doc "Sends a generic MIDI message to the MIDI OUT port.") csdoc-opcode-database)
(puthash "wgbow" '(:template "ares wgbow kamp, kfreq, kpres, krat, kvibf, kvamp [, ifn] [, iminfreq]" :doc "Creates a tone similar to a bowed string.") csdoc-opcode-database)
(puthash "sortd" '(:template "k/i[]sortd k/i[] (k- or i-arrays )" :doc "Sorts an array in descending order.") csdoc-opcode-database)
(puthash "ceil" '(:template "ceil(x) (init-, control-, or audio-rate arg allowed)
ceil(k/i[]) (k- or i-arrays )" :doc "Returns the smallest integer not less than") csdoc-opcode-database)
(puthash "outq2" '(:template "outq2 asig" :doc "Writes samples to quad channel 2 of an external device or stream.") csdoc-opcode-database)
(puthash "exprand" '(:template "ares exprand klambda
ires exprand klambda
kres exprand klambda" :doc "WTF: ") csdoc-opcode-database)
(puthash "hdf5write" '(:template "hdf5write ifilename, xout1[, xout2, xout3, ..., xoutN]" :doc "Write signals and arrays to an hdf5 file.") csdoc-opcode-database)
(puthash "in" '(:template "ar1 in
aarray in" :doc "Reads mono audio data from an external device or stream.") csdoc-opcode-database)
(puthash "linenr" '(:template "ares linenr xamp, irise, idec, iatdec
kres linenr kamp, irise, idec, iatdec" :doc "The") csdoc-opcode-database)
(puthash "OSClisten" '(:template "kans OSClisten ihandle, idest, itype [, xdata1, xdata2, ...]
kans, kdata[] OSClisten ihandle, idest, itype" :doc "Listen for OSC messages to a particular path.") csdoc-opcode-database)
(puthash "scanhammer" '(:template "scanhammer isrc, idst, ipos, imult" :doc "Copies from one table to another with a gain control.") csdoc-opcode-database)
(puthash "exciter" '(:template "ares exciter asig, kfreq, kceil, kharmonics, kblend" :doc "A non-linear filter system to excite the signal.") csdoc-opcode-database)
(puthash "STKHevyMetl" '(:template "asignal STKHevyMetl ifrequency, iamplitude, [kmod, kv1[, kcross, kv2[, klfo, kv3[, klfodepth, kv4[, kadsr, kv5]]]]]" :doc "STKHevyMetl produces metal sounds.") csdoc-opcode-database)
(puthash "zacl" '(:template "zacl kfirst, klast" :doc "Clears one or more variables in the za space.") csdoc-opcode-database)
(puthash "STKBandedWG" '(:template "asignal STKBandedWG ifrequency, iamplitude, [kpress, kv1[, kmot, kv2[, klfo, kv3[, klfodepth, kv4[, kvel, kv5[, kstrk, kv6[, kinstr, kv7]]]]]]]" :doc "STKBandedWG uses banded waveguide techniques to model a variety of sounds.") csdoc-opcode-database)
(puthash "ksmps" '(:template "ksmps = iarg" :doc "Sets the number of samples in a control period.") csdoc-opcode-database)
(puthash "STKResonate" '(:template "asignal STKResonate ifrequency, iamplitude, [kfreq, kv1[, kpole, kv2[, knotch, kv3[, kzero, kv4[, kenv, kv5]]]]]" :doc "STKResonate is a noise driven formant filter.") csdoc-opcode-database)
(puthash "cpsoct" '(:template "cpsoct (oct) (no rate restriction)" :doc "Converts an octave-point-decimal value to cycles-per-second.") csdoc-opcode-database)
(puthash "vactrol" '(:template "ares vactrol asig [iup, idown]" :doc "Envelope follower unit generator.") csdoc-opcode-database)
(puthash "vlowres" '(:template "ares vlowres asig, kfco, kres, iord, ksep" :doc "A bank of filters in which the cutoff frequency can be separated under user control.") csdoc-opcode-database)
(puthash "sfinstr3m" '(:template "ares sfinstr3m ivel, inotenum, xamp, xfreq, instrnum, ifilhandle [, iflag] [, ioffset]" :doc "Plays a SoundFont2 (SF2) sample instrument, generating a mono sound with cubic interpolation.") csdoc-opcode-database)
(puthash "zdf_ladder" '(:template "asig zdf_ladder ain, xcf, xQ [, istor]" :doc "Zero-delay feedback implementation of 4 pole ladder filter.") csdoc-opcode-database)
(puthash "FLcolor2" '(:template "FLcolor2 ired, igreen, iblue" :doc "A FLTK opcode that sets the secondary (selection) color.") csdoc-opcode-database)
(puthash "schedkwhen" '(:template "schedkwhen ktrigger, kmintim, kmaxnum, kinsnum, kwhen, kdur [, ip4] [, ip5] [...]" :doc "Adds a new score event generated by a k-rate trigger.") csdoc-opcode-database)
(puthash "schedwhen" '(:template "schedwhen ktrigger, kinsnum, kwhen, kdur [, ip4] [, ip5] [...]" :doc "Adds a new score event.") csdoc-opcode-database)
(puthash "massign" '(:template "massign midichn, instr" :doc "assigns midi channel to instrument") csdoc-opcode-database)
(puthash "event" '(:template "event "scorechar", kinsnum, kdelay, kdur, [, kp4] [, kp5] [, ...]" :doc "Generates a score event from an instrument.") csdoc-opcode-database)
(puthash "event_i" '(:template "event_i "scorechar", iinsnum, idelay, idur, [, ip4] [, ip5] [, ...]" :doc "Generates a score event from an instrument.") csdoc-opcode-database)
(puthash "prints" '(:template "prints "string" [, kval1] [, kval2] [...]" :doc "Prints at init-time using a printf() style syntax.") csdoc-opcode-database)
(puthash "printks" '(:template "printks "string", itime [, kval1] [, kval2] [...]" :doc "Prints at k-rate using a printf() style syntax.") csdoc-opcode-database)
(puthash "printks2" '(:template "printks2 "string", kval" :doc "Prints a new value every time a control variable changes using a printf() style syntax.") csdoc-opcode-database)

 (provide 'csound-opcodes)
;;; csound-opcodes.el ends here
