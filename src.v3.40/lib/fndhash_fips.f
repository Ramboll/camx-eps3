C**** FNDHASH_FIPS.F 
c
      function fndhash_fips( fipin )
      integer*4 fndhash_fips
c
c-----------------------------------------------------------------------
c
c     This routine searches through the hash arrays for FIPS code,
c     and identifies the index for the FIPS code passed in the
c     argument list.
c     
c   Arguments:
c     Outputs:
c       Return value:
c          index of code, zero means it was not found
c     Inputs:
c       fipin    C   FIPS code to match
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'hash.inc'
      include 'cntylst.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*5 fipin
c
c-----------------------------------------------------------------------
c   External functions: 
c-----------------------------------------------------------------------
c
c   mkhash_fips  I  returns the hash value for the FIPS code
c
      integer*4 mkhash_fips
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxfrst, idxhash, idxnext
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ---
c
      fndhash_fips = 0
c
c   ---- find the hash value for this FIPS code ---
c
      idxfrst = mkhash_fips( fipin )
c
c   --- if this is the first entry for this hash, then this
c       county is not in the list ---
c
      if( idxhsh_first_fips(idxfrst) .EQ. IMISS ) goto 9999
c
c   --- kick start the list ---
c
      idxhash = idxhsh_first_fips(idxfrst)
 111  continue
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this coutnty ----
c
      if( idxhash .EQ. IMISS ) goto 9999
c
c   ---- check for a match with this entry ----
c
      if( fipin .EQ. fipcd(idxhash) ) then
          fndhash_fips = idxhash
          goto 9999
c
c  --- no match here, get next entry for this hash ----
c
      else
          idxnext = idxhsh_next_fips(idxhash)
          idxhash = idxnext
          goto 111
      endif
c
c   ---- did not find a match ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
