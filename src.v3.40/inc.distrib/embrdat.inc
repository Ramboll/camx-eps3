c*** EMBRDAT.INC 
c
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the EMBR data records.
c
c-----------------------------------------------------------------------
c   Log:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   /HEADER/ variables:
c-----------------------------------------------------------------------
c
c   dxrgn    R   cell width in X direction
c   dyrgn    R   cell width in Y direction
c   filtyp   C   type of sources contained in file
c   irgzn    I   UTM zone of region
c   ncrthd   I   number of criteria pollutants in header
c   crtnam   C   array of named of the criteria pollutants
c   isarod   I   array of SAROAD codes for criteria pollutants
c   nspchd   I   number if speciated pollutants in file
c   ispcrt   I   array of indexes into criteria pollutant arrays
c   molwt    R   array of molecular weights for species
c   nxrgn    I   number of cells in X direction
c   nyrgn    I   number of cells in Y direction
c   rgutme   R   UTM easting coordinate of region
c   rgutmn   R   UTM northing coordinate of region
c   spclst   C   array for names of CBIV species
c   spcrit   C   array of criteria pollutants that produced CBIV species
c
      character*30 filtyp
      character*10 crtnam(MXCRIT)
      character*10 spclst(MXSPEC)
      character*10 spcrit(MXSPEC)
      integer*4    isarod(MXCRIT)
      integer*4    irgzn
      integer*4    ispcrt(MXSPEC)
      integer*4    nxrgn
      integer*4    nyrgn
      integer*4    nspchd
      integer*4    ncrthd
      real*4       dxrgn
      real*4       dyrgn
      real*4       molwt(MXSPEC)
      real*4       rgutme
      real*4       rgutmn
c
      common /hdrchr/ filtyp, spclst, spcrit, crtnam
      common /hdrdat/ irgzn, nxrgn, nyrgn, nspchd, ncrthd, isarod,
     &                ispcrt, rgutme, rgutmn, dxrgn, dyrgn, molwt
c
c-----------------------------------------------------------------------
c   /DATA/ variables:
c-----------------------------------------------------------------------
c
c   emin     R   array for emissions of criteria pollutants
c   emout    R   array for output emissions for criteria pollutants
c   fipin    C   FIPS state/county code
c   ijcell   I   cell location of gridded inventory
c   invbeg   I   beginning date and time
c   invend   I   ending Julian date and time
c   period   C   flag to determine the type of inventory (ie. PO )
c   segno    C   segment number
c   iutmzn   I   UTM zone
c   plant    C   NEDS plant identification code
c   stkid    C   stack number
c   point    C   point identification
c   rectyp   C   record type
c   sbrgn    C   subregion code
c   scccd    C   SCC code
c   siccd    C   SIC code
c   spcout   R   array for output emissions of CBIV species
c   spcpol   R   array for emissions of CBIV species
c   stkdia   R   stack diameter
c   stkgev   R   stack gas exit velocity
c   stkhgt   R   stack height
c   stktmp   R   stack exit gas temperature
c   utme     R   UTM easting coordinate
c   utmn     R   UTM northing coordinate
c
      character*10 scccd
      character*10 plant
      character*10 stkid
      character*10 point
      character*5  fipin
      character*5  sbrgn
      character*4  siccd
      character*3  segno
      character*2  period
      character*1  rectyp
      integer*4    ijcell
      integer*4    invbeg
      integer*4    invend
      integer*4    iutmzn
      real*4       emin(MXCRIT)
      real*4       emout(MXCRIT)
      real*4       spcout(MXSPEC)
      real*4       spcpol(MXSPEC)
      real*4       stkdia
      real*4       stkgev
      real*4       stkhgt
      real*4       stktmp
      real*4       utme
      real*4       utmn
c
      common /datchr/  fipin, sbrgn, siccd, scccd, rectyp, period,
     &                 plant, stkid, point, segno
      common /datdat/  invbeg, invend, ijcell, iutmzn, utme, utmn, 
     &                 stkgev, stkdia, stktmp, stkhgt, emin, spcpol, 
     &                                                    emout, spcout
c
c-----------------------------------------------------------------------
c   I/O unit and filename variables:
c-----------------------------------------------------------------------
c
c   curifl   C   name of the current input file being read
c   curofl   C   name of the current output file being written
c
      character*(MXSTR) curifl
      character*(MXSTR) curofl
c
      common /curchr/ curifl, curofl
