c*** CNTYLST.INC 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the /COUNTY/ packets which contains
c   the county list and the time zone
c
c-----------------------------------------------------------------------
c   /COUNTY/ parameters:    obsolete
c-----------------------------------------------------------------------
c
c   IDXDCO   I   index of CO design values in array
c   IDXDO3   I   index of O3 design values in array
c   IDXDPM   I   index of PM design values in array
c   MXDVAL   I   maximum number of design values
c
      integer*4  IDXDCO
      integer*4  IDXDO3
      integer*4  IDXDPM
      integer*4  MXDVAL

      parameter (IDXDCO=2)
      parameter (IDXDO3=1)
      parameter (IDXDPM=3)
      parameter (MXDVAL=3)
c
c-----------------------------------------------------------------------
c   /COUNTY/ variables:
c-----------------------------------------------------------------------
c
c   fipcd    C   array of state/county FIPS codes
c   itmzon   I   vector to hold the county timezone index
c   ncdfip   I   number of FIPS codes in array
c
      character*5 fipcd(MXFIP)
      integer*4   itmzon(MXFIP)
      integer*4   ncdfip
c
      common /fipchr/  fipcd
      common /fipdat/  ncdfip, itmzon
c
c-----------------------------------------------------------------------
c   Hash table variables:
c-----------------------------------------------------------------------
c
c  idxhsh_next_fips  I  index of next FIPS code for the hash value
c
      integer*4 idxhsh_next_fips(MXFIP+1)
c
      common /fiphash/ idxhsh_next_fips
