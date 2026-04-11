C*** ADDTOTCTY.F 
c
      subroutine addtotcty( ierr,
     &                         idxtot, idxfip, idxpol, idxspc, emsval)
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals arrays of 
c     emissions by county.  The arguments indicate where the
c     data should be stored.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxfip   I   index of FIPS code in totals array
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
      integer*4 idxtot
      integer*4 idxfip
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
      if( .NOT. lcrtcty(IDXFMT) .AND. .NOT. lspccty(IDXFMT) .AND.
     &      .NOT. lcrtcty(IDXCSV) .AND. .NOT. lspccty(IDXCSV) ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   --- skip if emissions is invalid ---
c
      if( emsval .LE. 0. ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   --- skip if county code is invalid ---
c
      if( idxfip .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   ---- add emissions to input array ---
c
      if( idxtot .EQ. ICDIN ) then
          if( idxpol .GT. 0 ) crticty(idxfip,idxpol) = 
     &                              crticty(idxfip,idxpol) + emsval
          if( idxspc .GT. 0 ) spcicty(idxfip,idxspc) = 
     &                              spcicty(idxfip,idxspc) + emsval
      endif
c
c   ---- add emissions to output array ---
c
      if( idxtot .EQ. ICDOUT ) then
          if( idxpol .GT. 0 ) crtocty(idxfip,idxpol) = 
     &                              crtocty(idxfip,idxpol) + emsval
          if( idxspc .GT. 0 ) spcocty(idxfip,idxspc) = 
     &                              spcocty(idxfip,idxspc) + emsval
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
