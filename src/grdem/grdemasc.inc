C**** GRDEMASC.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for emissions totals by ASC/SCC for GRDEM program
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXASCHR  I   maximum number of hours in FIPS/ASC totals array
c                (set to zero for daily totals) 
c   MXASCCL  I   maximum number of cells in FIPS/ASC totals array
c                (set to zero for non-gridded totals) 
c
       integer*4 MXASCHR
       integer*4 MXASCCL
c
       parameter ( MXASCHR =    1  )
       parameter ( MXASCCL =    1  )
c
c-----------------------------------------------------------------------
c   Variables for totals of emissions:
c-----------------------------------------------------------------------
c
c   asccd    C   array of ASC/SCC codes in input EMBR file
c   ascfip   R   array for ASC/FIP emissions of criteria pollutants (gm)
c   ncdasc   I   number of ASC/SCC codes
c   nclasc   I   number of cells for FIPS/ASC pair
c   ijclasc  I   index for the cells for the FIPS/ASC pair
c   lushr    L   array of flags to determine if hour has any emissions
c
      character*10 asccd(MXASC)
      integer*4    ncdasc
      integer*4    nclasc(MXASC,MXFIP)
      integer*4    ijclasc(MXASC,MXASCCL)
      real*4       ascfip(0:MXASCHR,0:MXASCCL,MXCRIT,1,1)
      logical*4    lushr(MXHOUR)
c
      common /ascchr/ asccd
c
      common /ascdat/ ncdasc, ascfip, nclasc, ijclasc, lushr
c
