C**** FNDHASH_XRF.F 
c
      function fndhash_xrf( ierr, fipin, subin, ascin )
      integer*4 fndhash_xrf
c
c-----------------------------------------------------------------------
c
c     This routine searches through the hash arrays for a match
c     of the codes passed in the arugment list. If a match is 
c     not found, the codes are added to the hash table.
c     
c   Arguments:
c     Outputs:
c       ierr     I   error code
c       Return value:
c          index of code, if it is not found it is added
c     Inputs:
c       fipin    C   FIPS code code to match
c       subin    C   subregion code to match
c       ascin    C   ASC code to match
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
      include 'grdemsrg.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      character*5  fipin
      character*5  subin
      character*10 ascin
c
c-----------------------------------------------------------------------
c   External functions: 
c-----------------------------------------------------------------------
c
c   mkhash_pair  I  returns the hash value for the FIPS/ASC code pair
c
      integer*4 mkhash_pair
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*10 lngfip
      integer*4    idxfrst, idxhash, idxnext, idxprev
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
      ierr = IFAIL
      fndhash_xrf = 0
c
c   --- create the "long" FIPS code ----
c
      lngfip = fipin // subin
c
c   ---- find the hash value for this ASC code ---
c
      idxfrst = mkhash_pair( fipin, ascin )
c
c   --- if this is the first entry for this hash, update the
c       necessary arrays ---
c
      if( idxhsh_xrf_first_pair(idxfrst) .EQ. IMISS ) then
          nsgxrf = nsgxrf + 1
          if( nsgxrf .GT. MXREF ) goto 7000
          idxhsh_xrf_first_pair(idxfrst) = nsgxrf
          fndhash_xrf = nsgxrf
          ierr = ISUCES
          goto 9999
      endif
c
c   --- kick start the list ---
c
      idxhash = idxhsh_xrf_first_pair(idxfrst)
 111  continue
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
      if( idxhash .EQ. IMISS ) goto 222
c
c   ---- check for a match with this entry ----
c
      if( lngfip .EQ. fipxrf(idxhash) .AND. 
     &                         ascin .EQ. sccxrf(idxhash) ) then
          fndhash_xrf = idxhash
          ierr = ISUCES
          goto 9999
c
c  --- no match here, get next entry for this hash ----
c
      else
          idxnext = idxhsh_xrf_next_pair(idxhash)
          idxprev = idxhash
          idxhash = idxnext
          goto 111
      endif
c
c   ---- did not find a match, add code to arrays and
c        update all necessary arrays ---
c
  222 continue
      nsgxrf = nsgxrf + 1
      if( nsgxrf .GT. MXREF ) goto 7000
      idxhsh_xrf_next_pair(idxprev) = nsgxrf
      fndhash_xrf = nsgxrf
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &                       'ASC-SCC/surrogate cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &           'ASC-SCC/surrogate cross reference records exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
