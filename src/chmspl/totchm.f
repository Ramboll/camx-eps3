C**** TOTCHM.F 
c
      subroutine totchm(
     &                  idxtot, emscrt, emssum, emspc, idxpcd )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to a the totals array at index
c     given by the input argument.  The number of records variable is
c     also incremented for that index.
c
c   Arguments:
c     Outputs:
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       emscrt   R   emissions to add to totals for criteria pollutant
c       emssum   R   emissions to add to totals for sum of speciated
c       emspc    R   emissions to add to totals for speciated emissions
c       idxpcd   I   index of profile code in arrays
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'splits.inc'
      include 'totchm.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 idxtot
      real*4    emscrt(MXCRIT)
      real*4    emssum(MXCRIT)
      real*4    emspc(MXSPEC)
      integer*4 idxpcd
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
       do i=1,ncrthd
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
c
      if( idxpcd .GT. 0 ) then
           do i=1,ncrthd
              if( emssum(i) .GT. 0. ) then
                  if( lsplcrt(i) ) then 
                     totpfc(idxtot,idxpcd,i) =
     &                           totpfc(idxtot,idxpcd,i) + emssum(i)
                  else
                     totpfc(idxtot,nfac,i) =
     &                           totpfc(idxtot,nfac,i) + emssum(i)
                  endif
              endif
           enddo
           if( idxtot .EQ. IDXOUT ) then
              do i=1,nspecs
                 if( emspc(i) .GT. 0. ) then
                    if( indspl(i) .GT. 0 ) then
                       totpfs(idxpcd,i) = totpfs(idxpcd,i) + 
     &                                emspc(i) * molwt(i) * CVTTON
                    else
                       totpfs(nfac,i) = totpfs(nfac,i) + 
     &                                emspc(i) * molwt(i) * CVTTON
                    endif
                 endif
              enddo
           endif
      endif
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
