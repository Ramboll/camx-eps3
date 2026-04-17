C**** TOTMRGU.F 
c
      subroutine totmrgu( ierr, idxtot, emiss, idxem, sccin, ihour )
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to a the totals array at index
c     given by the input argument.  The number of records variable is
c     also incremented for that index.
c
c   Arguments:
c     Outputs:
c       ierr    I   error code
c     Inputs:
c       idxtot  I   index of the record type in the totals array
c       emiss   R   emissions to add to totals for speciated emissions
c       idxem   I   index of species in arrays 
c       sccin   C   source category code (really just file number)
c       ihour   I   current hour
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
      include 'iomrguam.inc'
      include 'usrmrguam.inc'
      include 'mrguemis.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 sccin
      integer*4    ierr
      integer*4    idxtot
      integer*4    idxem
      integer*4    ihour
      real*4       emiss
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- intialize the error code ---
c
      ierr = IFAIL
c
c   ---- add emissions to processing array ---
c
      if( emiss .GT. 0. ) then
        totems(idxtot,ispcrt(idxem)) = 
     &                       totems(idxtot,ispcrt(idxem)) + DBLE(emiss)
        totspc(idxtot,idxem) = totspc(idxtot,idxem) + DBLE(emiss)
      endif
c
c   --- add to totals by source category (really file number)
c
      call addtotasc( jerr,idxtot,sccin,ispcrt(idxem),idxem,emiss )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- add to totals by hour ---
c
      call addtothrs( jerr,ihour-1,ihour,idxtot,
     &                                       ispcrt(idxem),idxem,emiss )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- add to totals by hour and source category ---
c
      call addtothcd( jerr,sccin,ihour-1,ihour,idxtot,
     &                                       ispcrt(idxem),idxem,emiss )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- set error code to sucess and return ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
