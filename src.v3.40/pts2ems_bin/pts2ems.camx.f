C23456789012345678901234567890123456789012345678901234567890123456789012
C
C Convert the Point Emission format to Area Emission format for
C visualization
C ENVIRON 2003 by Steven Lau
C
C Modified to increase precision in emission summation
C J Johnson Oct 2011
C
C23456789012345678901234567890123456789012345678901234567890123456789012

      program pts2ems

      parameter(MX=600,MY=600,MZ=40,MPTS=1500000,MSPEC=100)
      character*200 ifile ! Input and Output file name

      character*4 filename(10), fileid(60)
      character*10 emissions
      integer nseg, nspecies, ibegdat, ienddat
      real    begtim, endtim

      real    xutm,yutm,xloc,yloc,dx,dy,htsl,minhtsd,minhtdt
      integer iutmzone,nx,ny,nz,ncellsd,ncelldt

      integer ilocx, ilocy, noxx, noyy
      
      character*4 spname(10,MSPEC)

      real*8   ems(MX,MY),pts(MPTS)
      real     emsout(MX,MY),ptsin(MPTS),rjunk

      integer npts, junk
      real    putmx(MPTS), putmy(MPTS)
      integer putmix(MPTS), putmiy(MPTS), px, py, ix(MPTS), iy(MPTS)
      real    xorg, yorg

C Read in Control File
      read(*,101) ifile ! Read the Input Point Source file name
      open(unit=10,file=ifile,status='old',form='unformatted')

      read(*,101) ifile ! Read the Output Area Emission formatted file name
      open(unit=11,file=ifile,status='new',form='unformatted')

      read(*,101) ifile ! Read the XORG and YORG for the Emission file
      read(ifile,*) xorg, yorg, nx, ny, dx, dy
      xorg = xorg * 1000
      yorg = yorg * 1000
      dx   = dx * 1000
      dy   = dy * 1000
      if (nx.gt.mx.or.ny.gt.my) then
         write(*,102) nx, mx, ny, my
         stop
      endif
101   format(20x,a)
102   format('NX  >  MX or NX  > MY '/
     +        i3,3x,i3,4x,i3,3x,i3/
     +       'Larger MX or MY is required.'/
     +       'Recompile the code and run it again.')

      open(unit=20,file='pts2ems.tmp',status='unknown')

C Read the Point Source Header Record
      read(10) filename,fileid,nseg,nspec,ibegdat,begtim,ienddat,enddat
      print*, filename,fileid,nseg,nspec,ibegdat,begtim,ienddat,enddat
      if (nspec.gt.mspec) then
         write(*,103) nspec, mspec
         stop
      endif
103   format('NSPEC     > MSPEC     '/
     +       i10,2x,i10/
     +       'Larger MSPEC is required.'/
     +       'Recompile the code and run it again.')
      emissions = 'EMISSIONS '
      do i=1,10
         filename(i)(1:1) = emissions(i:i)
      enddo
      write(11) filename,fileid,nseg,nspec,ibegdat,begtim,ienddat,enddat

      read(10)  xutm,yutm,iutmzone,xloc,yloc,rjunk1,rjunk2,junk1,junk2,
     +          nz,ncellsd,ncelldt,htsl,minhtsd,minhtdt
      write(*,*)xutm,yutm,iutmzone,xloc,yloc,rjunk1,rjunk2,junk1,junk2,
     +          nz,ncellsd,ncelldt,htsl,minhtsd,minhtdt
      i0 = 0
      write(11) i0,i0,iutmzone,xorg,yorg,dx,dy,nx,ny,nz,
     +          ncellsd,ncelldt,htsl,minhtsd,minhtdt

      read(10)  ilocx, ilocy, noxx, noyy
      write(11) ilocx, ilocy, nx, ny

      read(10) ((spname(m,l),m=1,10),l=1,nspec)
      write(11) ((spname(m,l),m=1,10),l=1,nspec)

C Read the Point Source Time-Invariant Data Record

      read(10) nseg,npts 
      if (npts.gt.MPTS) then
         write(*,104) npts, mpts
         stop
      endif
104   format('NPTS      > MPTS      '/
     +       i10,2x,i10/
     +       'Larger MPTS is required.'/
     +       'Recompile the code and run it again.')
       
      if (npts.gt.0) then  
         read(10) (putmx(i),putmy(i),rjunk,rjunk,rjunk,rjunk,i=1,npts)
         do i=1,npts
            putmix(i) = int((putmx(i)+xutm-xorg)/dx)+1
            putmiy(i) = int((putmy(i)+yutm-yorg)/dy)+1
         enddo
      endif 

C Time Varying Data

      ihour = 0

301   read(10,end=999) begdat,ibegtim,enddat,iendtim
      write(11) begdat,ibegtim,enddat,iendtim

      ihour = ihour + 1

      read(10) nseg, npts
      if (npts.gt.0) 
     + read(10) (ix(i),iy(i),junk,rjunk,rjunk,i=1,npts)
      if (ihour.eq.1) then
      do i=1,npts
         write(20,203) i,putmx(i)+xutm,putmy(i)+yutm,xutm,yutm,
     +               xorg,yorg,putmix(i),putmiy(i),ix(i),iy(i)
203   format(i5,5x,6f12.2,4i5)
      enddo
      endif

      do l=1,nspec  
         do i=1,nx
         do j=1,ny
            ems(i,j) = 0
            emsout(i,j) = 0
         enddo
         enddo

         read(10) nseg,(spname(m,l),m=1,10),(ptsin(i),i=1,npts)
         do i=1,npts
            icell = FLOOR( (putmx(i) - xorg) / dx ) + 1
            jcell = FLOOR( (putmy(i) - yorg) / dy ) + 1
            pts(i)=ptsin(i)
            if (icell.ge.1.and.icell.le.nx.and.
     +          jcell.ge.1.and.jcell.le.ny) 
     +      ems(icell,jcell)=ems(icell,jcell)+pts(i)
          
         enddo

         do i=1,nx
         do j=1,ny
            emsout(i,j) = ems(i,j) 
         enddo
         enddo

         write(11) nseg,(spname(m,l),m=1,10),
     +      ((emsout(i,j),i=1,nx),j=1,ny)
      enddo

      go to 301
999   continue

      end
