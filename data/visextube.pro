tarr=dblarr(1)
maxa=fltarr(1)
mina=fltarr(1)
cuta=fltarr(2000,50)

DEVICE, PSEUDO=8, DECOMPOSED=0, RETAIN=2
WINDOW, /FREE, /PIXMAP, COLORS=256 & WDELETE, !D.WINDOW
PRINT, 'Date:      ', systime(0)
PRINT, 'n_colors   ', STRCOMPRESS(!D.N_COLORS,/REM)
PRINT, 'table_size ', STRCOMPRESS(!D.TABLE_SIZE,/REM)


ii=1

istart=1

if (ii eq 1) then begin
;loadct,4
;mixct
endif else begin
loadct,0
tek_color
endelse




mass=dblarr(1)
egas=dblarr(1)
tm=dblarr(1)
dtt=dblarr(1)

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

; Open an MPEG sequence: 
;mpegID = MPEG_OPEN([700,1200],FILENAME='myMovie.mpg') 

window, 0,xsize=1100,ysize=500,XPOS = 350, YPOS = 300 


nn=0
kkk=4

nn_i=0

close,1
close,2


openr,1,'/data/ap1vf/zero2BB_shiftVx.out',/f77_unf



while not(eof(1)) do begin
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




xout=dblarr(2)
yout=dblarr(2)


n1=nx(0)
n2=nx(1)
x=dblarr(n1,n2,ndim)
if (nn eq 0) then w=dblarr(n2,n1,nw)   ;was n1,n2,nw
wi=dblarr(n1,n2)
readu,1,x
for iw=0,nw-1 do begin
 readu,1,wi
 w(*,*,iw)=rotate(wi,1)
endfor

zmin=min(x(*,1,0))
zmax=max(x(*,1,0))

xmin=min(x(1,*,1))
xmax=max(x(1,*,1))

scale=1.0d6

Vt=dblarr(n1,n2)
B=dblarr(n1,n2)
B_bg=dblarr(n1,n2)

p=dblarr(n1,n2,1)
;e2=dblarr(n1,n2)


mu=4.0*!PI/1.0e7

print,time

kk=50

label_rho='!4q!X'+' ('+'!19kg/m!X!U3'+'!N)'
label_p='p'+' ('+'!19H/m!X!U2'+'!N)'
label_Bx='Bx'
label_By='By'
label_Bz='Bz'



arho_t=w(*,*,7)+w(*,*,0)
avz=w(*,*,1)/arho_t
avx=w(*,*,2)/arho_t
ae=w(*,*,3)
aeb=w(*,*,6)
arho=w(*,*,0)
abz=w(*,*,4)
abx=w(*,*,5)
abz_t=w(*,*,8)+w(*,*,4)
abx_t=w(*,*,9)+w(*,*,5)

R=8.3e+003
mu_gas=0.6
gamma=1.66667

xstart=0
xend=511


zstart=0
zend=240

n11=zend-zstart+1
n22=n2

Bxx=dblarr(n22,n11)
Bzz=dblarr(n22,n11)

xx=dblarr(n22)
zz=dblarr(n11)

xx(*)=x(1,*,1)
zz(*)=x(zstart:zend,1,0)


Bxx(*,*)=reform((w(*,zstart:zend,9))) 
Bzz(*,*)=reform((w(*,zstart:zend,8))) 


btot=sqrt(Bxx^2.0+Bzz^2.0)



wset,0
!p.multi = [0,2,0,0,1]

tvframe,avz,/sample, /bar,title='Vz [m/s]', xtitle='x [Mm]', ytitle='z [Mm]',charsize=1.0, $
        xrange=[xmin/scale, xmax/scale],yrange=[zmin/scale, zmax/scale]

;line2_b_classic, Bxx,Bzz,xx, zz, 1, scale ; 0 - plot, 1 - oplot

tvframe,avx,/sample, /bar,title='Vx [m/s]',xtitle='x [Mm]', ytitle='z [Mm]',charsize=1.0, $
        xrange=[xmin/scale, xmax/scale],yrange=[zmin/scale, zmax/scale] 

line2_b_classic, Bxx,Bzz,xx, zz, 1, scale ; 0 - plot, 1 - oplot

 ss='time ='+strTrim(string(time),1)
 
 xyouts,20,2, ss, /device, color=200
 

indexs=strtrim(nn,2)

a = strlen(indexs)                                                  
case a of                                                           
 1:indexss='0000'+indexs                                             
 2:indexss='000'+indexs                                              
 3:indexss='00'+indexs                                               
 4:indexss='0'+indexs                                               
endcase   

image_p = TVRD_24()
write_png,'/data/ap1vf/png/30Vxdriver/'+indexss+'.png',image_p, red,green, blue


nn=nn+1



endwhile


end
