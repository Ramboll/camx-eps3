C**** TOTPTS2EMS.F 
c
      subroutine totpts2ems( ierr, 
     &                       idxtot, spcin, idxspc, this_hour )

c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to a the totals array at index
c     given by the input argument.
c
c   Arguments:
c
c     Inputs:
c       ierr      I  error code
c       idxtot    I  index of the record type in the totals array
c       spcin     R  emissions to add to totals for CBIV
c       idxspc    I  index of species in arrays
c       this_hour I  hour for hourly totals 
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
      include 'pts2emstot.inc'
      include 'uamcntl.inc'
      include 'msgtot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      real*4       spcin
      integer*4    idxspc
      logical*4    this_hour
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
c   ---- initialize the error code ---
c
      ierr = IFAIL
c
c   ---- add any species emissions to totals ---
c
      if( spcin .GT. 0. ) then
          totspc(idxtot,idxspc) = totspc(IDXTOT,idxspc) 
     &                        + DBLE(spcin * molwt_out(idxspc) * CVTTON)
          if( idxtot .EQ. IDXIN .AND. this_hour .GT. 0 ) then
                spcihrs(this_hour,idxspc) = spcihrs(this_hour,idxspc) 
     &                        + DBLE(spcin * molwt_out(idxspc) * CVTTON)
          endif
          if( idxtot .EQ. IDXOUT .AND. this_hour .GT. 0 ) then
                spcohrs(this_hour,idxspc) = spcohrs(this_hour,idxspc) 
     &                        + DBLE(spcin * molwt_out(idxspc) * CVTTON)
          endif
      endif
c
c  --- set the error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
