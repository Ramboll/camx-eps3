c*** HASH.INC 
c
c-----------------------------------------------------------------------
c
c   Variables and common blocks for the hash tables used for lookup.
c
c-----------------------------------------------------------------------
c   Log:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Paarameters for indexing global arrays:
c-----------------------------------------------------------------------
c
c   MXHASH_PAIR  I  maximum value for the hash table for FIPS/ASC pair
c   MXHASH_FIPS  I  maximum value for the hash table for FIPS/ASC pair
c   MXHASH_ASC   I  maximum value for the hash table for FIPS/ASC pair
c
      integer*4 MXHASH_PAIR
      integer*4 MXHASH_FIPS
      integer*4 MXHASH_ASC
c
       parameter( MXHASH_PAIR = 76818 )
       parameter( MXHASH_FIPS =   348 )
       parameter( MXHASH_ASC  = 23808 )
c
c-----------------------------------------------------------------------
c   Global arrays:
c-----------------------------------------------------------------------
c
c   idxhsh_first_pair  I  first entry in array for the hashed value for
c                         FIPS/ASC pair hash table
c   idxhsh_first_fips  I  first entry in array for the hashed value for
c                         FIPS code hash table
c   idxhsh_first_asc   I  first entry in array for the hashed value for
c                         ASC code hash table
c
      integer*4 idxhsh_first_pair(MXHASH_PAIR)
      integer*4 idxhsh_first_fips(MXHASH_FIPS)
      integer*4 idxhsh_first_asc(MXHASH_ASC)
c
      common /hashdat/ idxhsh_first_pair, idxhsh_first_fips, 
     &                 idxhsh_first_asc
