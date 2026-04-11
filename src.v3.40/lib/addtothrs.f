C*** ADDTOTHRS.F 
c
      subroutine addtothrs( ierr,
     &                         ibegdat, ienddat, idxtot, idxpol, 
     &                                                  idxspc, emsval)
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals arrays of 
c     emissions by hours.  The arguments indicate where the
c     data should be stored.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
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
      integer*4 ierr
      integer*4 ibegdat
      integer*4 ienddat
      integer*4 idxtot
      integer*4 idxpol
      integer*4 idxspc
      real*4    emsval
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 ibeghr, iendhr, nhours, i
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
      if( .NOT. lcrthr(IDXFMT) .AND. .NOT. lspchr(IDXFMT) .AND.
     &         .NOT. lcrthr(IDXCSV) .AND. .NOT. lspchr(IDXCSV) ) then
         ierr = ISUCES
         goto 9999
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
      if( iendhr .GT. 24 ) iendhr = 24
      nhours = iendhr - ibeghr + 1
      emshr = emsval / FLOAT(nhours)
c
c   ---- add emissions to input array ---
c
      if( idxtot .EQ. ICDIN ) then
          do i=ibeghr,iendhr
              if( idxpol .GT. 0 ) crtihrs(i,idxpol) = 
     &                              crtihrs(i,idxpol) + emshr
              if( idxspc .GT. 0 ) spcihrs(i,idxspc) = 
     &                              spcihrs(i,idxspc) + emshr
          enddo
      endif
c
c   ---- add emissions to output array ---
c
      if( idxtot .EQ. ICDOUT ) then
          do i=ibeghr,iendhr
             if( idxpol .GT. 0 ) crtohrs(i,idxpol) = 
     &                              crtohrs(i,idxpol) + emshr
             if( idxspc .GT. 0 ) spcohrs(i,idxspc) = 
     &                              spcohrs(i,idxspc) + emshr
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
