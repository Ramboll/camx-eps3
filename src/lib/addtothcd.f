C*** ADDTOTHCD.F 
c
      subroutine addtothcd( ierr,
     &                            ascin, ibegdat, ienddat, idxtot, 
     &                                         idxpol, idxspc, emsval)
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals arrays of 
c     emissions by source category code and hour.  The arguments 
c     indicate where the data should be stored.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       ascin    C   source category code
c       ibegdat  I   beginning date (yyjjjhh)
c       ienddat  I   ending date (yyjjjhh)
c       idxtot   I   index of the record type in the totals array
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
      integer*4    ibegdat
      integer*4    ienddat
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
      integer*4 idxasc, ibeghr, iendhr, nhours, jerr, i
      real*4    emshr
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
      if( .NOT. lcrthcd(IDXFMT) .AND. .NOT. lspchcd(IDXFMT) .AND.
     &       .NOT. lcrthcd(IDXCSV) .AND. .NOT. lspchcd(IDXCSV) ) then
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
c   ---- get the hours for this record ---
c
      ibeghr = MOD( ibegdat, 100 )
      iendhr = MOD( ienddat, 100 )
      if( iendhr .EQ. 0 ) iendhr = 24
      ibeghr = ibeghr + 1
      iendhr = iendhr
      if( iendhr .GT. 24 ) iendhr = 24
      nhours = iendhr - ibeghr + 1
      emshr = emsval / FLOAT(nhours)
c
c   ---- add emissions to input array ---
c
      if( idxtot .EQ. ICDIN ) then
          do i=ibeghr,iendhr
              if( idxpol .GT. 0 ) crtihcd(idxasc,i,idxpol) = 
     &                              crtihcd(idxasc,i,idxpol) + emshr
              if( idxspc .GT. 0 ) spcihcd(idxasc,i,idxspc) = 
     &                              spcihcd(idxasc,i,idxspc) + emshr
          enddo
      endif
c
c   ---- add emissions to output array ---
c
      if( idxtot .EQ. ICDOUT ) then
          do i=ibeghr,iendhr
             if( idxpol .GT. 0 ) crtohcd(idxasc,i,idxpol) = 
     &                              crtohcd(idxasc,i,idxpol) + emshr
             if( idxspc .GT. 0 ) spcohcd(idxasc,i,idxspc) = 
     &                              spcohcd(idxasc,i,idxspc) + emshr
          enddo
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
