c**** GETPAR.F
c
      subroutine getpar( cdir, mdir, dmin, dmax, dinc, ndir )
c
c-----------------------------------------------------------------------
c
c    This subroutine obtains valid grid parameters.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c
c   Argument Declaration:
c   Inputs:
c      cdir  C  name of direction for required parameters
c      mdir  I  maximum number of grid cells for required direction
c   Outputs:
c      dmin  R  grid minimum for required direction
c      dmax  R  grid maximum for required direction
c      dinc  R  grid increment for required direction
c      ndir  I  number of grid cells for required direction
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
      character*(*) cdir
      integer*4     mdir
      real*4        dmin 
      real*4        dmax 
      real*4        dinc 
      real*4        dlen
      integer*4     ndir
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c ---- find grid minimum ---
c
 111  continue
      write(*,9002) cdir
      read(*,9000,ERR=111) dmin
c
c  ---- find grid maximum ----
c
 222  continue
      write(*,9003) cdir
      read(*,9000,ERR=222) dmax
      if( dmax .LE. dmin ) then
         write(*,9005) dmax, dmin
         goto 111
      endif
c
c  ---- find grid increment ----
c
 333  continue
      write(*,9004) cdir
      read(*,9000,ERR=333) dinc
      if( dinc .LE. 0.0 ) then
         write(*,9006) cdir, dinc
         goto 333
      endif
c
c  ---- find number of grid cells ----
c
      dlen = dmax - dmin
      ndir = INT(dlen/dinc)
      if (dlen .GT. ndir *dinc) ndir = ndir + 1
      if (ndir .Gt. mdir) then
         write(*,9007) ndir, mdir
         goto 111
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(BN, F12.0)
 9002 format(/,'Enter ',A,' minimum of grid: ',$)
 9003 format(/,'Enter ',A,' maximum of grid: ',$)
 9004 format(/,'Enter ',A,' grid increment: ',$)
 9005 format(/,'ERROR: ',A,' maximum (',F10.0,') is not greater than',
     &         ' minimum (',F10.0,')')
 9006 format(/,'ERROR: ',A,' grid increment (',F10.0,') is less than',
     &     ' or equal to zero')
 9007 format(/,'ERROR: No of ',A,' grid cells (',I5,') is greater than',
     &     ' allocated maximum (',I5,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
