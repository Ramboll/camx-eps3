      subroutine grdprep(nx,ny,cell_lon,cell_lat,
     &                                  orgx,orgy,dx,dy,xsize,ysize)
c 
c----CAMx v6.40_NCF_3 170630
c 
c     GRDPREP calculates coarse grid parameters for the following projections:
c        0. Lat/lon grid
c        1. Universal Transverse Mercator (UTM)
c        2. Lambert Conformal
c        3. Rotated (RAMS) Polar Stereographic
c        4. (WRF) Polar Stereographic 
c        5. (WRF) Mercator
c                           
c     Copyright 1996 - 2017
c     Ramboll Environ
c
c     Modifications: 
c        4/10/15      Added WRF Polar Stereographic and Mercator projections
c                     (Polar different from original Rotated Polar Stereographic
c                     from RAMS)
c 
c     Input arguments: 
c        nx                  number of columns
c        ny                  number of rows
c
c     Output arguments: 
c        cell_lon            array of cell centroid longitude (deg)
c        cell_lat            array of cell centroid latitude (deg)
c        orgx                origin of grid in X direction
c        orgy                origin of grid in X direction
c        dx                  cell width of grid in X direction
c        dy                  cell width of grid in Y direction
c        xsize               array of cell width in X direction
c        ysize               cell width in y direction
c             
c     Routines Called: 
c        UTMGEO
c        PSPGEO
c        LCPGEO
c             
c     Called by: 
c        STARTUP
c 
      implicit none
      include 'param.inc'
      include 'uamregn.inc'
c
      integer nx,ny
      real cell_lon(nx,ny), cell_lat(nx,ny)
      real orgx, orgy, dx, dy
      real xsize(ny), ysize
c
      integer i,j
      real deg2km,deg2rad
      real xloc_now,yloc_now,xloce,xlocw,alon,alat
      real colat,colat1,colat2,expn,xdel,ydel,elon,wlon,elat,wlat
c
      data deg2km /111.1338/
      data deg2rad /0.01745329/
c
c-----Entry point
c
c-----Cartesian coordinates are selected
c     Input cell sizes (dx and dy) are already in km
c
      if( .NOT. llatlon ) then
c
c-----Determine xsize and ysize (m) for coarse grid
c
        ysize = 1000.*dy
        do 10 j=1,ny
          xsize(j) = 1000.*dx
c
c-----Calculate lat/lon at cell centroids, and map-scale factor
c
          yloc_now = orgy + dy*(float(j) - 0.5)
          do 15 i = 1,nx
            xloc_now = orgx + dx*(float(i) - 0.5)
            if (lambrt) then
              call lcpgeo(1,polelon,polelat,tlat1,tlat2,xloc_now,
     &                                         yloc_now,alon,alat)
              colat1 = deg2rad*(90. - tlat1)
              colat = deg2rad*(90. - alat)
              if (tlat1 .ne. tlat2) then
                colat2 = deg2rad*(90. - tlat2)
                expn = (log(sin(colat1))    - log(sin(colat2))) /
     &                 (log(tan(colat1/2.)) - log(tan(colat2/2.)))
              endif
            elseif (lpolar) then
              call pspgeo(1,polelon,polelat,tlat1,xloc_now,yloc_now,
     &                                                     alon,alat)
            elseif (lmerc) then
              call mrcgeo(1,polelon,polelat,tlat1,xloc_now,yloc_now,
     &                                                     alon,alat)
              colat1 = deg2rad*(90. - tlat1)
              colat = deg2rad*(90. - alat)
            else
              if (lutm) then
                call utmgeo(1,izone,xloc_now,yloc_now,alon,alat)
                xloce = xloc_now + dx/2. 
                call utmgeo(1,izone,xloce,yloc_now,elon,elat)
                xlocw = xloc_now - dx/2. 
                call utmgeo(1,izone,xlocw,yloc_now,wlon,wlat)
              elseif (lrpolar) then
                call rpsgeo(1,polelon,polelat,xloc_now,yloc_now,alon,alat)
                xloce = xloc_now + dx/2. 
                call rpsgeo(1,polelon,polelat,xloce,yloc_now,elon,elat)
                xlocw = xloc_now - dx/2. 
                call rpsgeo(1,polelon,polelat,xlocw,yloc_now,wlon,wlat)
              endif
              xdel = deg2km*(elon - wlon)*cos(deg2rad*alat)  
              ydel = deg2km*(elat - wlat) 
            endif
            cell_lon(i,j) = alon
            cell_lat(i,j) = alat
 15       continue
 10     continue
c
c-----Lat/lon coordinates are selected
c
      else
c
c-----Determine xsize and ysize (m) on coarse grid
c
        ysize = 1000.*deg2km*dy
        do 20 j = 1,ny
          alat = orgy + dy*(float(j) - 0.5)
          xsize(j) = 1000.*deg2km*dx*cos(alat*deg2rad)
c
c-----Calculate lat/lon at cell centroids
c
          do 25 i = 1,nx
            alon = orgx + dx*(float(i) - 0.5)
            cell_lon(i,j) = alon
            cell_lat(i,j) = alat
 25        continue
 20     continue
      endif
c
      return
      end
