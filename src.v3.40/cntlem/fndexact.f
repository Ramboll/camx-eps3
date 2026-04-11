C**** FNDEXACT.F 
c
      function fndexact( idxasc, fipsin, asccd, ibeghr, iendhr )
      integer*4 fndexact
c
c-----------------------------------------------------------------------
c
c     This routine searches through the arrays of cross reference data
c     seaching for a match of the input data.  This must be an exact match.
c     This routine is designed to be effecient, assuming that all of the
c     matches are exact.
c
c   Arguments:
c     Outputs:
c       idxcod   I   array of matches
c     Inputs:
c       idxasc   I   index of SIC/SCC pair in totals arrays
c       fipsin   C   FIPS code to match
c       asccd    C   ASC-SCC code to match
c       ibeghr   I   beginning hour
c       iendhr   I   ending hour
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c   16Jan2014  mmj  - Added the required hash index from hash value
c                   - Removed setting SCC code off, still need to 
c                     look with a different FIPS
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'ctldsc.inc'
      include 'ctltot.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 fipsin
      character*10 asccd
      integer*4    idxasc
      integer*4    ibeghr
      integer*4    iendhr
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
      integer*4 mkhash_pair
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxhash
      logical*4 lfound
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c  --- assume no match ---
c
      fndexact = 0
      lfound = .FALSE.
c
c  --- get the hash value for this pair ---
c
      idxhash = mkhash_pair( fipsin, asccd )
      idxhash = idxhsh_src_first_pair(idxhash)
c
c  --- check the first hash entry for a match of this record ---
c
 111  continue
      lfound = .TRUE.
      if( fipsin .NE. srcfip(idxhash) ) lfound = .FALSE.
      if( asccd .NE. srcscc(idxhash) ) lfound = .FALSE.
      if( ibeghr .NE. ihrsrc(idxhash,1) ) lfound = .FALSE.
      if( iendhr .NE. ihrsrc(idxhash,2) ) lfound = .FALSE.
      if( lfound ) then
         fndexact = idxhash
         ilstidx = idxhash
         goto 9999
      endif
c
c  --- did not find a match, check if there is a next entry
c      for this hash ----
c
      if( idxhsh_src_next_pair(idxhash) .EQ. IMISS )  then
c mj need to continue to look at scc with different fips          lcodctl(idxasc) = .FALSE.
          goto 9999
      endif
c
c  --- there is another entry for this hash, reset
c      the index and check again ---
c
      idxhash = idxhsh_src_next_pair(idxhash) 
      goto 111
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
