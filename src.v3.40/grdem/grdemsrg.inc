c*** GRDEMSRG.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for the gridded surrogate data and cross reference
c    data used by the GRDEM module
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c    May 2008 mjimenez  Changed isgxrf to srgxrf (integer to character)
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   MXREF       I   maximum number of cross reference records
c
      integer*4 MXREF
c
      parameter( MXREF     =  6000 )
c
c-----------------------------------------------------------------------
c   ASC-SCC/surrogate cross reference data:
c-----------------------------------------------------------------------
c
c   fipxrf   C  long FIPS code (state/count/subregion) cross reference
c   srgxrf   C  surrogate code for cross reference 
c               (3 values: primary, secondary, tertiary)
c   nsgxrf   I  number of cross reference records
c   sccxrf   C  ASC-SCC code for cross reference
c
      character*10 sccxrf(MXREF)
      character*10 fipxrf(MXREF)
      character*3  srgxrf(MXREF,3)
      integer*4    nsgxrf
c
      common /sxfchr/ sccxrf, fipxrf, srgxrf
c
      common /sxfdat/ nsgxrf
c
c-----------------------------------------------------------------------
c    Variables for hash table arrays:
c-----------------------------------------------------------------------
c
c   idxhsh_xrf_first_pair I index of first record for this hash value
c   idxhsh_xrf_next_pair  I index of next record in the hash table
c
       integer*4 idxhsh_xrf_first_pair(MXHASH_PAIR)
       integer*4 idxhsh_xrf_next_pair(MXREF)
c
      common /datxrfhash/ idxhsh_xrf_first_pair, idxhsh_xrf_next_pair
c
c-----------------------------------------------------------------------
c   Parameters for array bounds:
c-----------------------------------------------------------------------
c
c   NSURG    I   number of surrogate codes
c   MXSRG    I   maximum number of gridded surrogate cells
c   DELSRG   R   delta used to compare surrogates to test completely
c                gridded county
c
      integer*4 NSURG
      integer*4 MXSRG
      real*4    DELSRG
c
      parameter( NSURG = 65 )
      parameter( MXSRG = 2*MXCELX*MXCELY )
      parameter( DELSRG = 0.005 )
c
c-----------------------------------------------------------------------
c   Gridded surrogate data:
c-----------------------------------------------------------------------
c
c   codnm    C  surrogate code names
c   fipsrg   C  long FIPS code (state/county/subregion) for surrogate
c   ncdsrg   I  number of codes in FIPS array
c   ncodes   I  number of gridded surg codes (columns) in gridded surg data
c   ibegsg   I  pointer to beginning of county in linked list
c   iclsrg   I  grid cell for surrogate
c   iendsg   I  pointer to end of county in linked list
c   lcodhdr  L  true if surrogate data has header record "CODES" defining
c               the columns of surg data
c   nsgcty   I  number of gridded surrogates for each county
c   nsrgrd   I  number of gridded surrogate records stored
c   srgfrc   R  fraction of county totals in the grid cell for each
c               surrogate
c   srgtot   R  total of each surrogate type in the county (should be 1.0)
c
      character*10 fipsrg(MXFIP)
      character*3  codnm(NSURG)
      integer*4    ncdsrg
      integer*4    ibegsg(MXFIP)
      integer*4    iclsrg(MXSRG)
      integer*4    iendsg(MXFIP)
      integer*4    nsgcty(MXFIP)
      integer*4    nsrgrd
      integer*4    ncodes
      logical*4    lcodhdr
      real*4       srgfrc(NSURG,MXSRG)
      real*4       srgtot(MXFIP,NSURG)
c
      common /srgchr/ fipsrg, codnm
      common /srgdat/ ncdsrg, nsrgrd, nsgcty, ibegsg, iendsg, iclsrg,
     &                srgfrc, srgtot, lcodhdr, ncodes
