C**** TOTPRP.F 
c
      subroutine totprp( ierr, idxtot, idxpol, emsval )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array at index
c     given by the input argument.  The number of records variable is
c     also incramented for that index.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxpol   I   index of pollutant in pollutant arrays
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
      include 'prepntot.inc'
      include 'prpntstk.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      integer*4    idxpol
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
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- set error flag ---
c
      ierr = IFAIL
c
c   --- skip if emissions is not positive ---
c
      if( emsval .LE. 0. ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- add emissions to processing array ---
c
      totems(idxtot,idxpol) = totems(idxtot,idxpol) + DBLE(emsval)
      if( idxtot .NE. IDXELE .AND. idxtot .NE. IDXLOW 
     &                          .AND. idxtot .NE. IDXOUT )
     &                                nrec(idxtot) = nrec(idxtot) + 1
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
