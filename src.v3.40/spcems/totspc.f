C**** TOTSPC.F 
c
      subroutine totspc(
     &                  idxtot, emscrt, emssum, lflag, idxpcd )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array at index
c     given by the input argument.
c
c   Arguments:
c     Outputs:
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       emscrt   R   emissions to add to totals for criteria pollutant
c       emssum   R   emissions to add to totals for sum of speciated
c       lflag    L   flag indicating to sum profile tables
c       idxpcd   I   index of profile code in arrays by criteria pollutant
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
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'factors.inc'
      include 'totspc.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 idxtot
      real*4    emscrt(MXCRIT)
      real*4    emssum(MXCRIT)
      logical*4 lflag
      integer*4 idxpcd(MXCRIT)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- add emissions to processing array ---
c
       do i = 1, ncrthd
         if( emscrt(i) .GT. 0. ) then
             if( idxtot .EQ. IDXSPC ) then
                 totems(idxtot,i) = totems(idxtot,i) + DBLE(emssum(i))
             else
                 totems(idxtot,i) = totems(idxtot,i) + DBLE(emscrt(i))
             endif
          endif
      enddo
c
      nrec(idxtot) = nrec(idxtot) + 1
c
c   ---- if not in or out, then that is all we do ----
c
      if( idxtot .NE. IDXIN .AND. idxtot .NE. IDXOUT ) goto 9999
c
c   ---- if we have a profile code, add emissions to profile totals ----
c   ---- profile code, criteria emissions ---
c
      if( lflag ) then
c
           do i=1,ncrthd
              if( emssum(i) .GT. 0. ) then
                     totpfc(idxtot,idxpcd(i),i) =
     &                           totpfc(idxtot,idxpcd(i),i) 
     &                            + emssum(i)
              endif
           enddo
c
      endif

c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
