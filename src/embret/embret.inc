c*** EMBRET.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for user input parameters and variables for EMBRET
c    program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Array bounds parameters:
c-----------------------------------------------------------------------
c
c   MXPLNT   I   maximum number of plant codes
c   MXSCC    I   maximum number of SCC codes
c   MXSIC    I   maximum number of SIC codes
c   MXSUB    I   maximum number of subgrids
c
      integer*4 MXPLNT
      integer*4 MXSCC
      integer*4 MXSIC
      integer*4 MXSUB
c
      parameter( MXPLNT = 100 )
      parameter( MXSCC  = 100 )
      parameter( MXSIC  = 100 )
      parameter( MXSUB  = 100 )
c
c-----------------------------------------------------------------------
c    County Packet
c-----------------------------------------------------------------------
c
c   ctyscd   C   array of FIPS codes to retrieve
c   ncdcty   I   number of FIPS codes in /COUNTY/ packet
c
      character*5 ctyscd(MXFIP)
      integer*4   ncdcty
c
      common /ctychr/ ctyscd
      common /ctydat/ ncdcty
c
c-----------------------------------------------------------------------
c    SIC packet
c-----------------------------------------------------------------------
c
c   ncdsic   I   number of SIC codes in /SIC/ packet
c   rsiccd   C   array of SIC codes to extract
c
      character*4 rsiccd(MXSIC)
      integer*4   ncdsic
c
      common /sicchr/ rsiccd
      common /sicdat/ ncdsic
c
c-----------------------------------------------------------------------
c    ASC-SCC packet
c-----------------------------------------------------------------------
c
c   ncdscc   I   number of SCC code in /SCC/ packet
c   rscccd   C   array of SCC codes to retrieve
c
      character*10 rscccd(MXSCC)
      integer*4    ncdscc
c
      common /sccchr/ rscccd
      common /sccdat/ ncdscc
c
c-----------------------------------------------------------------------
c    Plant packet
c-----------------------------------------------------------------------
c
c   ncdpln   I   number of plant code in /PLANT/ packet
c   plntcd   C   array of plant codes to retrieve
c
      character*10 plntcd(MXPLNT)
      integer*4    ncdpln
c
      common /pltchr/ plntcd
      common /pltdat/ ncdpln
c
c-----------------------------------------------------------------------
c    Subgrid packet
c-----------------------------------------------------------------------
c
c   ibegcl   I   lower left corner cell of subgrid
c   iendcl   I   upper right corner cell of subgrid
c   nsub     I   number of subgrids in /SUBGRID/ packet
c
      integer*4 ibegcl(MXSUB)
      integer*4 iendcl(MXSUB)
      integer*4 nsub
c
      common /subdat/ ibegcl, iendcl, nsub
