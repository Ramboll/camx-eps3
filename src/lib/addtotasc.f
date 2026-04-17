C*** ADDTOTASC.F 
c
      subroutine addtotasc( ierr,
     &                         idxtot, ascin, idxpol, idxspc, emsval)
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals arrays of 
c     emissions by source category code.  The arguments indicate 
c     where the data should be stored.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       ascin    I   source category code
c       idxpol   I   index of criteria pollutant in arrays
c                    (0 = don't load the criteria array)
c       idxspc   I   index of species in arrays
c                    (0 = don't load the species array)
c       emsval   R   emissions to add to totals
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'msgtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      character*10 ascin
      integer*4    idxtot
      integer*4    idxpol
      integer*4    idxspc
      real*4       emsval
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
      integer*4 fndhash_asc
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 idxasc, jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- set error flag ---
c
      ierr = IFAIL
c
c   --- if not doing this table, just return ---
c
      if( .NOT. lcrtasc(IDXFMT) .AND. .NOT. lspcasc(IDXFMT) .AND. 
     &      .NOT. lcrtasc(IDXCSV) .AND. .NOT. lspcasc(IDXCSV) ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   ---- find the index of code in the hash table ---
c
      if( ascin .EQ. last_msgasc ) then
         idxasc = idx_lastasc
      else
         idxasc = fndhash_asc( jerr, ascin )
         if( jerr .NE. ISUCES ) goto 9999
         last_msgasc = ascin
         idx_lastasc = idxasc
      endif
c
c   --- skip if emissions is invalid ---
c
      if( emsval .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   ---- add emissions to input array ---
c
      if( idxtot .EQ. ICDIN ) then
          if( idxpol .GT. 0 ) crtiasc(idxasc,idxpol) = 
     &                              crtiasc(idxasc,idxpol) + emsval
          if( idxspc .GT. 0 ) spciasc(idxasc,idxspc) = 
     &                              spciasc(idxasc,idxspc) + emsval
      endif
c
c   ---- add emissions to output array ---
c
      if( idxtot .EQ. ICDOUT ) then
          if( idxpol .GT. 0 ) crtoasc(idxasc,idxpol) = 
     &                              crtoasc(idxasc,idxpol) + emsval
          if( idxspc .GT. 0 ) spcoasc(idxasc,idxspc) = 
     &                              spcoasc(idxasc,idxspc) + emsval
      endif
c
c   --- set error flag to sucess ---
c
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
c   Format Statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
