function B_x,x_i,y_i,z_i
 RETURN,y_i
end  
function B_y,x_i,y_i,z_i
 RETURN,x_i
end  

function B_z,x_i,y_i,z_i
 RETURN,z_i
end  

DEVICE, PSEUDO=8, DECOMPOSED=0, RETAIN=2
WINDOW, /FREE, /PIXMAP, COLORS=256 & WDELETE, !D.WINDOW

;window,0, xsize=600,ysize=600
;window,1, xsize=600,ysize=600

;   windowsize=20
   ; Create some windows.
   Window, Title='Data Window', xsize=600,ysize=600, $
        /FREE, XPOS = 150, YPOS = 400
   dataWin = !D.Window

;   Window, Title='Polygon Window', xsize=600,ysize=600, $
;        /FREE, XPOS = 750, YPOS = 400
;   polyWin = !D.Window
   
   ; Draw plot in data window.

;   Window, Title='Transparent Polygon Window', xsize=600,ysize=600, $
;         XPOS = 1150, YPOS = 400, /FREE
;   outWin = !D.Window

print, 'data or function ? (data "1", function "2")'
;read,aa

aa=1

;**********  begin data from file

ni=0

mu=4.0*!PI/1.0e7
close,1

;openr,1,'/data/ap1vf/3D_VelA30.out',/f77_unf
;openr,1,'/data/ap1vf/3D_tube_196_100_100t.out',/f77_unf
openr,1,'/data/ap1vf/3D_tube_196_100_100_120s.out',/f77_unf

tarr=dblarr(1)
headline='                                                                               '
it=long(1)
ndim=long(1)
neqpar=long(1)
nw=long(1)
varname='                                                                               '
time=double(1)
dum=long(1)
dumd=long(1)
nn=0

while not(eof(1)) do begin
readu,1,headline
print,'head = ', headline
readu,1,it,time,ndim,neqpar,nw
print,'it, time', it,time,ndim,neqpar,nw
gencoord=(ndim lt 0)
tarr=[tarr,time]
ndim=abs(ndim)
print, '*',ndim 
nx=lonarr(ndim)
readu,1,nx
print, 'nx = ', nx
eqpar=dblarr(neqpar)
readu,1,eqpar
print,eqpar
readu,1,varname
print,varname

n1=nx(0) 
n2=nx(1)
n3=nx(2)

xx=dblarr(n1,n2,n3,ndim)

print, '****************'

readu,1,xx

wi=dblarr(n1,n2,n3)
w=dblarr(n1,n2,n3,nw)

for iw=0,nw-1 do begin
 readu,1,wi
  w(*,*,*,iw)=wi
  print, iw
endfor

;close,1


;***********************************************
nzs=0
nz=190

nxs=0
nxe=99

nys=0	
nye=99

;***********************************************

bx=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)
by=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)
bz=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)

vx=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)
vy=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)
vz=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)

rho=dblarr(nz-nzs+1,nxe-nxs+1,nye-nys+1)

if aa eq 1 then begin

;************************** B E G I N   F I L E *******************************

bx(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,6)+w(nzs:nz,nxs:nxe,nys:nye,11)
by(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,7)+w(nzs:nz,nxs:nxe,nys:nye,12)
bz(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,5)+w(nzs:nz,nxs:nxe,nys:nye,10)

vx(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,2)
vy(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,3)
vz(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,1)

rho(0:nz-nzs,0:nxe-nxs,0:nye-nys)=w(nzs:nz,nxs:nxe,nys:nye,0)+w(nzs:nz,nxs:nxe,nys:nye,9)
;bx(nzs:nz,nxs:nxe,nys:nye)=w(nzs:nz,nxs:nxe,nys:nye,11)
;by(nzs:nz,nxs:nxe,nys:nye)=w(nzs:nz,nxs:nxe,nys:nye,12)
;bz(nzs:nz,nxs:nxe,nys:nye)=w(nzs:nz,nxs:nxe,nys:nye,10)

;************************** E N D   F I L E *******************************

endif


   
   WSet, dataWin


;wset,0
;set_plot, 'ps'

;device, filename='p_1.ps', xsize=12, ysize=12, /color


ll=5

loadct, ll


!p.multi=[0,1,0,0,1]

scale=1.0d6

z=dblarr(nz-nzs+1)
x=dblarr(nxe-nxs+1)
y=dblarr(nye-nys+1)


;****************end data from file 

;--------------------
; Make a vector of 16 points, A[i] = 2pi/16: 
A = FINDGEN(17) *  (!PI*2/16.) 
; Define the symbol to be a unit circle with 16 points,  
; and set the filled flag: 
USERSYM, 1.*COS(A), 1.*SIN(A), /fill
;--------------------



for i=0,nz-nzs do begin
  if aa eq 1 then begin
    z(i)=xx(nzs+i,0,0,0)/scale
  endif else begin
   z(i)=(nzs+i-nz/2.0)
  endelse 
 endfor

for j=0,nxe-nxs do begin
  if aa eq 1 then begin
    x(j)=xx(0,nxs+j,0,1)/scale
  endif else begin
    x(j)=((nxs+j-nxe)/2.0)
  endelse 
 endfor

for k=0,nye-nys do begin
  if aa eq 1 then begin
    y(k)=xx(0,0,nys+k,2)/scale
  endif else begin
    y(k)=((nys+k-nye)/2.0)
  endelse  
endfor


xyz_color=dblarr(1)

sc=0.9d0

mi_x=min(x) ;*sc
ma_x=max(x) ; *sc

mi_y=min(y) ;*sc
ma_y=max(y) ;*sc

mi_z=min(z)
ma_z=max(z)

b_total=sqrt(bx(0,*,*)^2.0+by(0,*,*)^2.0+bz(0,*,*)^2.0)

v_total=sqrt(vx(*,*,*)^2.0+vy(*,*,*)^2.0+vz(*,*,*)^2.0)/rho(*,*,*)

;tvframe,b_total, /bar

max_b=max(b_total)
min_b=min(b_total)


;******************************************************
nn=4.0
;******************************************************

xm=dblarr(1)
ym=dblarr(1)
zm=dblarr(1)



x0=ma_x
y0=ma_y
z0=0.95*ma_z
z0=1.0d0*ma_z

y0_pred=y0

x0_s=x0
y0_s=y0
z0_s=z0

xm(0)=x0
ym(0)=y0
zm(0)=z0

xyz_color(0)=1.0

color_min=40
color_max=256.0

;******************************************************************
dL=2000.d0/scale
;******************************************************************

delta_x=(ma_x-mi_x)/nn
delta_y=(ma_y-mi_y)/nn


SURFACE, DIST(5), /NODATA, /SAVE, XRANGE=[mi_x, ma_x],  $
   YRANGE=[mi_y, ma_y], ZRANGE=[mi_z, ma_z], XSTYLE=1, $
   YSTYLE=1, ZSTYLE=1, CHARSIZE=2.0, xtitle='x [Mm]',ytitle='y [Mm]', $
   ztitle='z [Mm]', POSITION=[0.2, 0.1, 0.95, 0.95, 0.1, 0.95], az=30.0, ax=30.0
   ;BACKGROUND=FSC_Color('ivory')  

;CONTOUR, vz(12,*,*), x, y, /T3D, /NOERASE, /NOCLIP,  $
;   XSTYLE=1, YSTYLE=1, ZSTYLE=1,$
;   ZVALUE = 0.1, nlevels=100 , POSITION=[0.2, 0.1, 0.95, 0.95, 0.5, 0.95], /fill, $
   
 levels = 40
loadct,3, NColors=40, Bottom=3

step = (Max(vz(12,*,*)) - Min(vz(12,*,*))) / levels
userLevels = IndGen(levels) * step + Min(vz(12,*,*))

Contour, vz(12,*,*), x, y, /Fill, C_Colors=Indgen(levels)+3, Background=1, /T3D,$
   Levels=userLevels, POSITION=[0.2, 0.1, 0.95, 0.95, 0.1, 0.95], Color=black, ZVALUE = 0.1, $
   XSTYLE=1, YSTYLE=1, ZSTYLE=1, /NOCLIP,CHARSIZE=2.0, XRANGE=[mi_x, ma_x],  $
   YRANGE=[mi_y, ma_y], ZRANGE=[mi_z, ma_z]
   
;Contour, vz(12,*,*), x, y, /Overplot, Levels=userLevels, /Follow, Color=black, /T3D, $ 
;         ZVALUE = 0.1, XSTYLE=1, YSTYLE=1, ZSTYLE=1, /NOCLIP

;ColorBar, NColors=12, Bottom=3, Divisions=6, $
;   Range=[Min(data), Max(data)], Format='(I4)', $
;   Position = [0.1, 0.9, 0.9, 0.95], Color=black
   
   
loadct,ll

SURFACE, DIST(5), /NODATA, /SAVE, XRANGE=[mi_x, ma_x], /noerase, $
   YRANGE=[mi_y, ma_y], ZRANGE=[mi_z, ma_z], XSTYLE=1, $
   YSTYLE=1, ZSTYLE=1, CHARSIZE=2.0, xtitle='x [Mm]',ytitle='y [Mm]', $
   ztitle='z [Mm]', POSITION=[0.2, 0.1, 0.95, 0.95, 0.1, 0.95], az=30.0, ax=30.0
   ;BACKGROUND=FSC_Color('ivory')  



AXIS, XAXIS=1, XRANGE=[mi_x, ma_x], /T3D, charsize=2.0, /NODATA
AXIS, YAXIS=1, XRANGE=[mi_y, ma_y], /T3D, charsize=2.0, /NODATA
ii=0


R=(ma_x-mi_x)/2.d0


while y0_s ge mi_y do begin

while (x0_s ge mi_x) do begin


if R^2.d0-((y0_s-mi_y-(ma_y-mi_y)/2.d0)^2.d0+(x0_s-mi_x-(ma_x-mi_x)/2.d0)^2.d0) ge -0.02d0 then begin

;print, '*****', R, Rt2, R^2.d0-((y0_s-ma_y/2.d0)^2.d0+(x0_s-ma_x/2.d0)^2.d0)

while ((x0 ge mi_x) and (x0 le ma_x) and (y0 ge mi_y) and (y0 le ma_y) and (z0 ge mi_z) and (z0 le ma_z)) do begin 


     xc=interpol(dindgen(nxe-nxs+1),x,x0)
     yc=interpol(dindgen(nye-nys+1),y,y0)
     zc=interpol(dindgen(nz-nzs+1),z,z0)

          
  Bbx=interpolate(bx, zc, xc, yc)
  Bby=interpolate(by, zc, xc, yc)
  Bbz=interpolate(bz, zc, xc, yc)  

   Bb=sqrt(Bbx^2.0+Bby^2.0+Bbz^2.0)

   cl=color_max-(max_b-Bb)/(max_b-min_b)*(color_max-color_min)
   
   xyz_color=[xyz_color,cl]
   
   dx=-dL*Bbx/Bb
   dy=-dL*Bby/Bb
   dz=-dL*Bbz/Bb   


   x0=x0+dx
   y0=y0+dy
   z0=z0+dz

   xm=[xm,x0]
   ym=[ym,y0]
   zm=[zm,z0]
  
   
 ;   PLOTS, xm,ym,zm, psym=3, /T3D 
endwhile
endif
  x0_s=x0_s-delta_x
  x0=x0_s
 
  y0=y0_pred
  z0=0.95*ma_z
  z0=1.0d0*ma_z  
  print,'ii=', ii, x0
  ii=ii+1
  
endwhile
  
  y0_s=y0_s-delta_y
  y0=y0_s
  y0_pred=y0
  
  x0=ma_x
  x0_s=ma_x
  z0=0.95*ma_z    
  z0=1.0d0*ma_z


endwhile

;for i=0,n_elements(xm) do begin
; Create an empty, 3-D array:  
;SPHERE = FLTARR(10, 10, 10)  
  
; Create the spherical dataset:  
;FOR Xs=0,9 DO FOR Ys=0,9 DO FOR Zs=0,9 DO $  
;   SPHERE(Xs, Ys, Zs) = SQRT((Xs-xm[i])^2 + (Ys-ym[i])^2 + (Zs-zm[i])^2)  
   
;SHADE_VOLUME, SPHERE, 8, V, P 

;SCALE3, XRANGE=[0,10], YRANGE=[0,10], ZRANGE=[0,10]  

;TV, POLYSHADE(v, p, /T3D)



;endfor

;FLOW3,xm,ym,zm



PLOTS, [ma_x,ma_x], [ma_y,ma_y], [mi_z, ma_z], /T3D

PLOTS, xm,ym,zm, color=xyz_color, psym=8, /T3D

PLOTS, [mi_x,mi_x], [mi_y,mi_y], [mi_z, ma_z], /T3D
PLOTS, [ma_x,ma_x], [mi_y,mi_y], [mi_z, ma_z], /T3D

surf=dblarr(nxe-nxs+1,nye-nys+1)

nzz=nz
surf(*,*)=sqrt(bx(nzz-1,*,*)^2.d0+by(nzz-1,*,*)^2.d0+bz(nzz-1,*,*)^2.d0)*sqrt(mu)*1.0e4

CONTOUR, surf, x, y, /T3D, /noerase, zvalue=0.95,/follow, $
         XRANGE=[mi_x, ma_x], YRANGE=[mi_y, ma_y], $
	 CHARSIZE=2.0,  nlevels=10, POSITION=[0.2, 0.1, 0.95, 0.95, 0.95, 0.95], $
	 C_charsize=2.d0, xstyle=1, ystyle=1



; /NOERASE, /NOCLIP,$ ; /nodata, $
;   , $
;   ZVALUE = 0.4, nlevels=10 , POSITION=[0.2, 0.1, 0.95, 0.95, 0.95, 0.95], $
;   CHARSIZE=2.0, xtitle='x [Mm]',ytitle='y [Mm]' ;, /fill
;   XSTYLE=1, YSTYLE=1, ZSTYLE=1,$

;stop   
   ; Take a snapshot.
;   background = TVREAD(TRUE=3)
   
   ; Copy data window and draw a polygon in the polygon window.
;   WSet, polyWin
;   DEVICE, COPY=[0, 0, WINDOWSIZE, WINDOWSIZE, 0, 0, dataWin]
   

;CONTOUR, vz(12,*,*), x, y, /T3D, /NOERASE, /NOCLIP,  $
;   XSTYLE=1, YSTYLE=1, ZSTYLE=1,$
;   ZVALUE = 0.1, nlevels=100 , POSITION=[0.2, 0.1, 0.95, 0.95, 0.5, 0.95], /fill
	     
;   ; Take a snapshot of this window, then delete it.
;   foreground = TVREAD(TRUE=3)   
;   
;   ; Use a 25% transparent alpha.
;   alpha = 0.35
;   Window, Title='Transparent Polygon Window', xsize=600,ysize=600, $
;        XPOS=2*WINDOWSIZE+20, YPOS=0, /FREE
;   Wset, outWin	
;   TV, (foreground * alpha) + (1 - alpha) * background, TRUE=3
 
 
     
ss='time ='+strTrim(string(time),1)+' it ='+strTrim(string(it),1)+'  nn = '+strTrim(string(nn),1)
 xyouts,50,2, ss, /device, color=200	


;device, /close
;set_plot, 'x'


indexs=strtrim(ni,2)

a = strlen(indexs)                                                  
case a of                                                           
 1:indexss='0000'+indexs                                             
 2:indexss='000'+indexs                                              
 3:indexss='00'+indexs                                               
 4:indexss='0'+indexs                                               
endcase   

image_p = TVRD_24()
write_png,'/data/ap1vf/png/3D/tube/test_120s/full/'+indexss+'.png',image_p, red,green, blue


ni=ni+1


endwhile
 
end
