C**** FNDHASH_TPL.F 
c
      function fndhash_tpl( ierr, 
     &                         lngin, plantin, stackin, pointin, sccin )
      integer*4 fndhash_tpl
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
c          index of record, if it is not found it is added
c     Inputs:
c       lngin    C   long FIPS code to match
c       plantin  C   plant ID code to match
c       stackin  C   stack ID to match
c       pointin  C   stack ID to match
c       sccin    C   SCC code to match
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
      include 'tmprlprf.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      character*10 lngin
      character*10 plantin
      character*10 stackin
      character*10 pointin
      character*10 sccin
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
      integer*4    idxfrst, idxhash, idxnext, idxprev
      logical*4    lfound
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
      fndhash_tpl = 0
c
c   ---- find the hash value for this ASC code ---
c
      idxfrst = mkhash_pair( lngin(1:5), sccin )
c
c   --- if this is the first entry for this hash, update the
c       necessary arrays ---
c
      if( idxhsh_tpl_first_pair(idxfrst) .EQ. IMISS ) then
          npfxrf = npfxrf + 1
          if( npfxrf .GT. MXREF ) goto 7000
          idxhsh_tpl_first_pair(idxfrst) = npfxrf
          fndhash_tpl = npfxrf
          ierr = ISUCES
          goto 9999
      endif
c
c   --- kick start the list ---
c
      idxhash = idxhsh_tpl_first_pair(idxfrst)
 111  continue
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
      if( idxhash .EQ. IMISS ) goto 222
c
c   ---- check for a match with this entry ----
c
      lfound = .TRUE.
      if( lngin .NE. fipxrf(idxhash) ) lfound = .FALSE.
      if( plantin .NE. facxrf(idxhash) ) lfound = .FALSE.
      if( stackin .NE. stkxrf(idxhash) ) lfound = .FALSE.
      if( pointin .NE. pntxrf(idxhash) ) lfound = .FALSE.
      if( sccin .NE. sccxrf(idxhash) ) lfound = .FALSE.
      if( lfound ) then
          fndhash_tpl = idxhash
          ierr = ISUCES
          goto 9999
c
c  --- no match here, get next entry for this hash ----
c
      else
          idxnext = idxhsh_tpl_next_pair(idxhash)
          idxprev = idxhash
          idxhash = idxnext
          goto 111
      endif
c
c   ---- did not find a match, add code to arrays and
c        update all necessary arrays ---
c
  222 continue
      npfxrf = npfxrf + 1
      if( npfxrf .GT. MXREF ) goto 7000
      idxhsh_tpl_next_pair(idxprev) = npfxrf
      fndhash_tpl = npfxrf
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading ',
     &              'ASC-SCC/temporal profile cross reference file.'
      write(ioerr,'(2A)',ERR=9999) 'Maximum number of ',
     &    'ASC-SCC/temporal profile cross reference records exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXREF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXREF ',
     &                                                  'and recompile.'
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
