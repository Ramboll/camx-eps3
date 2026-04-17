C**** SETIJ.F 
c
      subroutine setij(iclout,jclout,xloc,yloc,angle)
c
c-----------------------------------------------------------------------
c
c    Calculates the grid cell indexes from UTM coordinates
c
c    Argument declarations:
c      Outputs:
c        iclout  I  X index of grid cell in larger region
c        jclout  I  Y index of grid cell in larger region
c     Inputs:
c        xloc    R  UTM easting coordinate
c        yloc    R  UTM nortihing coordinate
c        angle   R  angle of rotation
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
c
c-----------------------------------------------------------------------
c   Argument declerations:
c-----------------------------------------------------------------------
c
      integer*4 iclout
      integer*4 jclout
      real*4    xloc
      real*4    yloc
      real*4    angle
c
c-----------------------------------------------------------------------
c   local parameters:
c----------------------------------------------------------------------
c
c   PICVT   R   converion factor for degrees to radians
c   BFUZZ   R   fuzz for handling roundoff od coordinates
c
      real*4 PICVT
      real*4 BFUZZ
c
      parameter( PICVT = 0.0174532 )
      parameter( BFUZZ = 0.005     )
c
c-----------------------------------------------------------------------
c   Local variables:
c----------------------------------------------------------------------
c
c   rangle  R  angle between point and origin
c
      real*4 rangle, xdiff, ydiff
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
c   --- find distance from origin in x direction on rotated grid,
c       since some computers are unable to store some numbers,
c       the "fuzz" value ensures that the correct integer will
c       be assinged later ----
c
      rangle = PICVT * angle
      xdiff = BFUZZ + (xloc * cos(rangle) + yloc * sin(rangle) ) -
     &              (xorig * cos(rangle) + yorig * sin(rangle))
c
c   --- find distance from origin in Y direction on rotated grid ---
c
      ydiff = BFUZZ + (-xloc * sin(rangle) + yloc * cos(rangle) ) -
     &              (-xorig * sin(rangle) + yorig * cos(rangle))
c
c   ---- calculate grid cell indexes ---
c
      if( xdiff .LT. 0 .OR. ydiff .LT. 0 ) goto 9999
      iclout = INT(xdiff/dxcell) + 1
      jclout = INT(ydiff/dycell) + 1
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
