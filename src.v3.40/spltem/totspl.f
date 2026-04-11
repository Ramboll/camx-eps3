C**** TOTSPL.F
c
      subroutine totspl(
     &                   idxtot, idxprf, emiss )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array at index
c     given by the input argument.  The number of records variable is
c     also incramented for that index.
c
c   Arguments:
c
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxfip   I   index of profile code in totals array
c                    (0 = do not add to profile totals)
c       emiss    R   array of emissions to add to totals for criteria
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     08/13/97  -mmj-  original development
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'spltem.inc'
      include 'spltemtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4 idxtot
      integer*4 idxprf
      real*4    emiss(MXCRIT)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- add emissions to criteria totals ---
c
      if( idxprf .EQ. 0 ) then
         do 10 i=1,ncrthd
         if( emiss(i) .GT. 0. ) totems(idxtot, i) = totems(idxtot, i) +
     &                                                         emiss(i)
   10    continue
c
c        ---- accumulate number of records processed counter ---
         nrec(idxtot) = nrec(idxtot) + 1
      endif
c
c   ---- if we have a profile code, add emissions to totals for profile ----
c
      if( idxprf .GT. 0 ) then
            totprf(idxprf) = totprf(idxprf) + emiss(1)
   20    continue
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
