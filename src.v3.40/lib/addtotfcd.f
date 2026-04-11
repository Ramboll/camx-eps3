C*** ADDTOTCOD.F 
c
      subroutine addtotfcd( ierr,
     &                 idxtot, idxfip, ascin, idxpol, idxspc, emsval )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array for the FIPS
c     and source category pair.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxfip   I   index of FIPS code in totals array
c                    (0 = do not add to FIPS totals)
c       ascin    C   ASC code
c       idxpol   I   index of criteria pollutant
c       idxspc   I   index of speciated pollutant
c       emsval   R   emissions to add to totals for criteria pollutant
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
      include 'cntylst.inc'
      include 'msgtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      character*10 ascin
      integer*4    ierr
      integer*4    idxtot
      integer*4    idxfip
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
      character*16 fipasc
      integer*4    idxcod, idxasc, jerr
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
      if( .NOT. lcrtfcd(IDXFMT) .AND. .NOT. lspcfcd(IDXFMT) .AND.
     &      .NOT. lcrtfcd(IDXCSV) .AND. .NOT. lspcfcd(IDXCSV) ) then
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
c   --- skip if county index is not valid ---
c
      if( idxfip .LE. 0 ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- find the index of ASC code in the hash table ---
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
c   ---- find the index of code in list, if not listed add it ---
c
      if( idxfip .LE. MXFIP ) then
         fipasc = fipcd(idxfip)//' '//ascin
      else
         fipasc = 'Skip '//' '//ascin
      endif
      if( fipasc .EQ. last_msgfcd ) then
         idxcod = idx_lastfcd
      else
         idxcod = idxfcd( idxfip, idxasc )
         if( idxcod .LE. 0 ) then
             nmsgfcd = nmsgfcd + 1
             if( nmsgfcd .GT. MXFCD ) goto 7001
             msgfcd(nmsgfcd) = fipasc
             idxcod = nmsgfcd
             idxfcd( idxfip, idxasc ) = idxcod
         endif
      endif
c
c   ---- add to the generic totals arrays ---
c 
      if( idxtot .EQ. ICDIN ) then
          if( idxpol .GT. 0 ) crtifcd(idxcod,idxpol) = 
     &                                 crtifcd(idxcod,idxpol) + emsval
          if( idxspc .GT. 0 ) spcifcd(idxcod,idxspc) = 
     &                                 spcifcd(idxcod,idxspc) + emsval
      else if( idxtot .EQ. ICDOUT ) then
          if( idxpol .GT. 0 ) crtofcd(idxcod,idxpol) = 
     &                                 crtofcd(idxcod,idxpol) + emsval
          if( idxspc .GT. 0 ) spcofcd(idxcod,idxspc) = 
     &                                 spcofcd(idxcod,idxspc) + emsval
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
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Saving data in ',
     &                                                 'totals arrays.'
      write(ioerr,'(2A)',ERR=9999) 'Number of county/source category ',
     &                                             'pairs exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed: ',MXFCD
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFCD ',
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
