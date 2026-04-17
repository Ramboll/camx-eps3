C**** ADDTOTPID.F 
c
      subroutine addtotpid( ierr,
     &                         idxtot, idxfip, plntin, stckin, pntin, 
     &                                         idxpol, idxspc, emsval )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array for the
c     tables by point source ID.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxfip   I   index of FIPS code in totals array
c                    (0 = do not add to FIPS totals)
c       plntin   C   plant id
c       stckin   C   stack id
c       pntin    C   point id
c       idxpol   I   index of criteria pollutant in arrays
c       idxspc   I   index of speciated pollutant in arrays
c       emiss    R   emissions to add to totals for criteria pollutant
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
      include 'cntylst.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      integer*4    idxfip
      character*10 plntin
      character*10 stckin
      character*10 pntin
      integer*4    idxpol
      integer*4    idxspc
      real*4       emsval
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*35 srcid
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
      if( .NOT. lcrtpid .AND. .NOT. lspcpid ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   --- skip if emissions is not positive ---
c
      if( emsval .LE. 0. ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- if the source is different from the last one, add it
c        to the list ----
c
      if( idxfip .LE. MXFIP ) then
         srcid = fipcd(idxfip) // plntin // stckin // pntin
      else
         srcid = 'Skip '
      endif
      if( nmsgpid .EQ. 0 ) then
          nmsgpid = 1
          msgpid(nmsgpid) = srcid
      endif
      if( srcid .NE. msgpid(nmsgpid) ) then
          nmsgpid = nmsgpid + 1
          if( nmsgpid .GT. MXPID ) goto 7000
          msgpid(nmsgpid) = srcid
      endif
c
c   ---- add to the totals arrays ---
c
      if( idxtot .EQ. ICDIN ) then
          if( idxpol .GT. 0 ) crtipid(nmsgpid,idxpol) =
     &                                 crtipid(nmsgpid,idxpol) + emsval
          if( idxspc .GT. 0 ) spcipid(nmsgpid,idxspc) =
     &                                 spcipid(nmsgpid,idxspc) + emsval
      else if( idxtot .EQ. ICDOUT ) then
          if( idxpol .GT. 0 ) crtopid(nmsgpid,idxpol) =
     &                                 crtopid(nmsgpid,idxpol) + emsval
          if( idxspc .GT. 0 ) spcopid(nmsgpid,idxspc) =
     &                                 spcopid(nmsgpid,idxspc) + emsval
      endif

c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Saving data in ',
     &                                                 'totals arrays.'
      write(ioerr,'(2A)',ERR=9999) 'Number of sources exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXPID
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPID ',
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
