c*** PREAM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for variables for airs data used bu the PREAM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Variables:
c-----------------------------------------------------------------------
c
c   crtpol  R   emissions for criteria pollutant
c   ibasyr  I   base year of inventory
c   ibegdt  I   beginning date/time of inventory
c   ienddt  I   ending date/time of inventory
c   inasc   C   ASC code
c   inetyp  C   emissions type
c   infips  C   FIPS state/county code
c   inpol   I   SAROAD code of pollutant
c   insbrg  C   subregion code
c   insic   C   SIC code
c   iperod  C   inventory period code
c   irefyr  I   reference year of inventory
c   itype   C   invetory type
c
      character*10 inasc
      character*5  insbrg
      character*5  infips
      character*4  insic
      character*2  inetyp
      character*2  iperod
      character*1  itype
      integer*4    ibasyr
      integer*4    ibegdt
      integer*4    ienddt
      integer*4    inpol
      integer*4    irefyr
      real*4       crtpol(MXCRIT)
c
      common /preamc/ itype, inetyp, infips, insbrg, insic, inasc,
     &                iperod
      common /preamn/ irefyr, ibasyr, ibegdt, ienddt, inpol, crtpol
c
c-----------------------------------------------------------------------
c   Variables for pollutant codes
c-----------------------------------------------------------------------
c
c   ipolcd   I   array of codes for criteria pollutants
c   ncdpol   I   number of pollutant codes in the array
c
      integer*4 ipolcd(MXCRIT)
      integer*4 ncdpol
c
      common /poldat/ ipolcd, ncdpol
