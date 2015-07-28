tarr=dblarr(1)
maxa=fltarr(1)
mina=fltarr(1)
cuta=fltarr(2000,50)

;DEVICE, PSEUDO=8, DECOMPOSED=0, RETAIN=2
;WINDOW, /FREE, /PIXMAP, COLORS=256 & WDELETE, !D.WINDOW
;PRINT, 'Date:      ', systime(0)
;PRINT, 'n_colors   ', STRCOMPRESS(!D.N_COLORS,/REM)
;PRINT, 'table_size ', STRCOMPRESS(!D.TABLE_SIZE,/REM)


ii=1

nnp=1

if (ii eq 1) then begin
;loadct,4
;mixct
endif else begin
loadct,0
tek_color
endelse


timesteps = 78
timesteps=1190
;timesteps = 3

npic=timesteps

time_nn=dblarr(1+timesteps)

mass=dblarr(1)
egas=dblarr(1)
tm=dblarr(1)
dtt=dblarr(1)



pic=65
npic=70
nn=0;
for ipic=pic,npic do begin


ia=1.0

headline='                                                                               '
it=long(1)
ndim=long(1)
neqpar=long(1)
nw=long(1)
varname='                                                                               '
time=double(1)
dum=long(1)
dumd=long(1)


;nn=0


close,1

;openr,1,'/data/ap1vf/3D/torsional_driver_puls_long/3D_tube_196_100_100.out',/f77_unf

;openr,1,'/data/ap1vf/3D/torsional_driver_cont_2min_LA/3D_tube_196_100_100.out',/f77_unf

;openr,1,'/data/ap1vf/3D_tube_196_100_100_multidriver_lower.out',/f77_unf

;directory='/data/cs1mkg/smaug_spicule1/spicule4b0_b4_3d/'
;directory='/fastdata/cs1mkg/smaug/spic4b0_b4_3d/'
;directory='/fastdata/cs1mkg/smaug/spic6p7a_0_0_3d/'
;directory='/fastdata/cs1mkg/smaug/spic2p3a_0_3_3d/'
;directory='/fastdata/cs1mkg/smaug/spic4p3a_0_1_3d/'
;directory='/fastdata/cs1mkg/smaug/spic2p3a_0_3_3d/'
directory='/fastdata/cs1mkg/sac/p5b0_1_bv20g/'
;name='zeroOT_'
;name='3D_em_t1_bin_np010808_00'
;name='3D_em_f1_bin_'
name='3D_spic_128_bv20G_'



;while not(eof(1)) do begin

;picid=ipic*5+4
;picid=ipic*1000L
picid=ipic;
outfile=directory+name+strtrim(string(picid),2)+'.out'
print,'ipic=',ipic
openr,1,outfile,/f77_unf
;openr,1,outfile








readu,1,headline
readu,1,it,time,ndim,neqpar,nw
gencoord=(ndim lt 0)
tarr=[tarr,time]
ndim=abs(ndim)
nx=lonarr(ndim)
readu,1,nx
eqpar=dblarr(neqpar)
readu,1,eqpar
readu,1,varname



xout=dblarr(3)
yout=dblarr(3)


n1=nx(0)
n2=nx(1)
n3=nx(2)


wi=dblarr(n1,n2,n3)


x=dblarr(n1,n2,n3,ndim)

wi=dblarr(n1,n2,n3)

w=fltarr(n1,n2,n3,nw)

readu,1,x
for iw=0,nw-1 do begin
 readu,1,wi
  w(*,*,*,iw)=wi
endfor


;if nnp eq 1 then begin

print,time, nn

ttime=dblarr(1)



;***********************************save new ini file
close,1
;openw,1,'/fastdata/cs1mkg/smaug/spicule4_nob/zerospic1_asc_290001.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spicule5_nob/zerospic1_asc_167501.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spicule4a_nob/zerospic1_asc_149001.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/out/spicule5b4/zerospic1_asc_555001.ini'
;openw,1,'/fastdata/cs1mkg/smaug/em6b4_bhor120/zerospic1_asc_84000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/em5b4_bhor120/zerospic1_asc_86000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spicule8b0_nquotaob/zerospic1_asc_584001.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spicule8b0_nob/zerospic1_asc_643001.ini'
;openw,1,'/fastdata/cs1mkg/smaug/scnstrh5b0b_nob/zerospic1_asc_391000.ini'
;openw,1,'/data/ap1vf/3D_modif_200_100_100.ini',/f77_unf
;openw,1,'/data/cs1mkg/smaug_spicule1/3D_128_spic_asc.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic5b0_3d_rep/zerospic1_asc_506000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic5b1_2/zerospic1_asc_588000.ini'

;openw,1,'/data/cs1mkg/smaug_spicule1/3D_128_spic_bvert_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/3D_128_spic_bvert2_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/spicule4b0_b1_3d/zerospic1_asc_39000.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/spicule4b0_b2_3d/zerospic1_asc_5000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic5b0_3d/zerospic1_asc_612000.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/3D_128_spic_bvert1_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/spicule4b0_bom500_3d/zerospic1_asc_434000.ini'
;openw,1,'/data/cs1mkg/vac3d/3D_196_100_100_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/spicule4b3_3_3d/zerospic1_asc_360000.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/configs/3D_128_spic_btube1_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/configs/3D_128_spic_btube2_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/configs/3D_128_spic_btube3_asc.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/configs/3D_128_spic_btube4_asc.ini'
;openw,1,'../configs/3D_128_spic_bvert100G_asc.ini'
;openw,1,'/data/cs1mkg/smaug_pmode/spicule6b0_3d/zerospic1_asc__302000.ini'
;openw,1,'/data/cs1mkg/smaug_spicule1/spicule4p3a_0_1_3d/zerospic1_asc_1206000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic2p82a_0_0_3d/zerospic1_asc_586000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic2p00a_0_1_3d/zerospic1_asc_588000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic1p33a_0_2_3d/zerospic1_asc_624000.ini'
;openw,1,'/fastdata/cs1mkg/smaug/spic1p00a_0_3_3d/zerospic1_asc_575000.ini'
;/fastdata/cs1mkg/smaug/spic1p79a_0_0_3d/zerospic1__408000.out
;openw,1,'/fastdata/cs1mkg/smaug/spic6b1_1/zerospic1_asc_209000.ini'

;openw,1,'//data/cs1mkg/smaugtemp/smaug/out/sedov_asc_118000.ini'
;openr,1,'/data/cs1mkg/smaugtemp/smaug/out/sedov_118000.out'

ascoutfile=directory+'asc/'+name+strtrim(string(picid),2)+'.out'
openw,1,ascoutfile





printf,1, FORMAT='(%"%s ")',headline
printf,1,FORMAT='(%"%d %g %d %d %d ")',it,time,ndim,neqpar,nw
printf,1,FORMAT='(%"%d %d %d ")',nx(0),nx(1),nx(2)
printf,1,FORMAT='(%"%g %g %g %g %g %g %g")',eqpar(0),eqpar(1),eqpar(2),eqpar(3),eqpar(4),eqpar(5),eqpar(6)
printf,1,FORMAT='(%"%s ")',varname
;printf,1,x_code

i1=long(1)
i2=long(1)
i3=long(1)

for i3=0, n3-1 do begin
for i2=0, n2-1 do begin
for i1=0, n1-1 do begin
       ; printf,1,x_code(i1,i2,i3,0),x_code(i1,i2,i3,1),w(i1,i2,i3,0),w(i1,i2,i3,1),w(i1,i2,i3,2),w(i1,i2,i3,3),w(i1,i2,i3,4),w(i1,i2,i3,5),w(i1,i2,i3,6),w(i1,i2,i3,7),w(i1,i2,i3,8),w(i1,i2,i3,9),FORMAT='(15F5)'
 printf,1, FORMAT='(%"%g %g %g %g %g %g %g %g %g %g %g %g %g %g %g %g ")',x(i1,i2,i3,0),x(i1,i2,i3,1),x(i1,i2,i3,2),w(i1,i2,i3,0),w(i1,i2,i3,1),w(i1,i2,i3,2),w(i1,i2,i3,3),w(i1,i2,i3,4),w(i1,i2,i3,5),w(i1,i2,i3,6),w(i1,i2,i3,7),w(i1,i2,i3,8),w(i1,i2,i3,9),w(i1,i2,i3,10),w(i1,i2,i3,11),w(i1,i2,i3,12)
	;for iw=0,nw-1 do begin
	; wi=w(*,*,iw)
	; printf,1,w(i1,i2,i3,iw)
	;endfor
endfor
endfor
endfor


 
close,1


endfor
;endwhile



end
