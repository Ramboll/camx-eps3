C**** FNDHASH_ASC.F 
c
      function fndhash_asc( ierr, ascin )
      integer*4 fndhash_asc
c
c-----------------------------------------------------------------------
c
c     This routine searches through the hash arrays for ASC code,
c     and identifies the index for the ASC code passed in the
c     argument list.
c     
c   Arguments:
c     Outputs:
c       ierr     I   error code
c       Return value:
c          index of code, if it is not found it is added
c     Inputs:
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
      include 'msgtot.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      character*10 ascin
c
c-----------------------------------------------------------------------
c   External functions: 
c-----------------------------------------------------------------------
c
c   mkhash_asc  I  returns the hash value for the ASC code
c
      integer*4 mkhash_asc
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxfrst, idxhash, idxnext, idxprev
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
      fndhash_asc = 0
c
c   ---- find the hash value for this ASC code ---
c
      idxfrst = mkhash_asc( ascin )
c
c   --- if this is the first entry for this hash, update the
c       necessary arrays ---
c
      if( idxhsh_first_asc(idxfrst) .EQ. IMISS ) then
          nmsgasc = nmsgasc + 1
          if( nmsgasc .GT. MXASC ) goto 7000
          msgasc(nmsgasc) = ascin
          idx_lastasc = nmsgasc
          last_msgasc = ascin
          idxhsh_first_asc(idxfrst) = nmsgasc
          fndhash_asc = nmsgasc
          ierr = ISUCES
          goto 9999
      endif
c
c   --- kick start the list ---
c
      idxhash = idxhsh_first_asc(idxfrst)
 111  continue
c
c   ---- if this index is not initialized, we are at
c        the end of the hash entries for this pair ----
c
      if( idxhash .EQ. IMISS ) goto 222
c
c   ---- check for a match with this entry ----
c
      if( ascin .EQ. msgasc(idxhash) ) then
          idx_lastasc = idxhash
          last_msgasc = ascin
          fndhash_asc = idxhash
          ierr = ISUCES
          goto 9999
c
c  --- no match here, get next entry for this hash ----
c
      else
          idxnext = idxhsh_msgtot_next_asc(idxhash)
          idxprev = idxhash
          idxhash = idxnext
          goto 111
      endif
c
c   ---- did not find a match, add code to arrays and
c        update all necessary arrays ---
c
  222 continue
      nmsgasc = nmsgasc + 1
      if( nmsgasc .GT. MXASC ) goto 7000
      msgasc(nmsgasc) = ascin
      idx_lastasc = nmsgasc
      last_msgasc = ascin
      idxhsh_msgtot_next_asc(idxprev) = nmsgasc
      fndhash_asc = nmsgasc
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error statements:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Saving data in ',
     &                                                  'totals arrays.'
      write(ioerr,'(A)',ERR=9999)
     &                   'Number of source category codes exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXASC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXASC ',
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
