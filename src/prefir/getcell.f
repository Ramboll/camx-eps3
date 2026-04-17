C**** GETCELL.F 
c
      subroutine getcell(iclout,jclout,xloc,yloc)
c
c-----------------------------------------------------------------------
c
c    Determines the grid cell indexes based on possible finer grid
c    resolution.
c
c    Argument declarations:
c      Outputs:
c        iclout  I  X index of grid cell
c        jclout  I  Y index of grid cell
c     Inputs:
c        xloc    R  UTM easting coordinate
c        yloc    R  UTM nortihing coordinate
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'uamregn.inc'
      include 'usrprfir.inc'
c
c-----------------------------------------------------------------------
c   Argument declerations:
c-----------------------------------------------------------------------
c
      integer*4 iclout
      integer*4 jclout
      real*4    xloc
      real*4    yloc
c
c-----------------------------------------------------------------------
c   local parameters:
c----------------------------------------------------------------------
c
c   BFUZZ   R   fuzz for handling roundoff od coordinates
c
      real*4 BFUZZ
c
      parameter( BFUZZ = 0.005     )
c
c-----------------------------------------------------------------------
c   Local variables:
c----------------------------------------------------------------------
c
c
      real*4 xdiff, ydiff
c
c-----------------------------------------------------------------------
c   Entry point:
c----------------------------------------------------------------------
c
c   --- initlialize output arguments ---
c
      iclout = 0
      jclout = 0
c
      xdiff = BFUZZ + ( xloc  - xorig )
      ydiff = BFUZZ + ( yloc - yorig )
c
c   ---- calculate grid cell indexes ---
c
      if( xdiff .LT. 0 .OR. ydiff .LT. 0 ) goto 9999
      iclout = INT(xdiff/dxcomb) + 1
      jclout = INT(ydiff/dxcomb) + 1
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
