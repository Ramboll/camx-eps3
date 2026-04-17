c**** RDHDR.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for UAM header data used by the MRGUAM program
c
c-----------------------------------------------------------------------
c    Control packet data:
c-----------------------------------------------------------------------
c
c   idateb  I  beginning date of episode
c   idatee  I  ending date of episode
c   tbegti  R  beginning time of episode
c   tendti  R  ending time of episode
c
      integer*4 idateb
      integer*4 idatee
      real*4    tbegti
      real*4    tendti
c
c-----------------------------------------------------------------------
c    Region packet data:
c-----------------------------------------------------------------------
c
c   izonet  I  utm zone of origin
c   nxcelt  I  number of grid cells in X direction
c   nycelt  I  number of grid cells in Y direction
c   nzcelt  I  number of vertical cells
c   nzlowt  I  number of vertical cells below diffusion break
c   nzuppt  I  number of vertical cells above diffusion break
c   thtlow  R  minumum thickness of vertical cells below diffusion break
c   thtsur  R  height of surface
c   thtupp  R  minumum thickness of vertical cells above diffusion break
c   tutmx   R  utm easting coordinate of origin
c   txorig  R  modeling origin in X direction
c   tyorig  R  modeling origin in Y direction
c   yutmy   R  utm northing coordinate of origin
c
      integer*4 izonet
      integer*4 nxcelt
      integer*4 nycelt
      integer*4 nzcelt
      integer*4 nzlowt
      integer*4 nzuppt
      real*4    tdxcel
      real*4    tdycel
      real*4    thtlow
      real*4    thtsur
      real*4    thtupp
      real*4    tutmx
      real*4    tutmy
      real*4    txorig
      real*4    tyorig
c
c-----------------------------------------------------------------------
c   Common block:
c-----------------------------------------------------------------------
c
      common /hdrdat/ idateb, idatee, tbegti, tendti,
     &    txorig, tyorig, izonet, tutmx, tutmy, tdxcel, tdycel,
     &    nxcelt, nycelt, nzcelt, nzlowt, nzuppt,
     &    thtsur, thtlow, thtupp
