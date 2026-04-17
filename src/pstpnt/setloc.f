C**** SETLOC.F 
c
      subroutine setloc(ierr,
     &                        xloc,yloc,angle)
c
c-----------------------------------------------------------------------
c
c    Calculates the grid cell indexes from UTM coordinates
c
c    Argument Declarations:
c      Outputs:
c        ierr    I  error flag (IFAIL = not in region )
c     Inputs:
c        xloc    R  UTM easting coordinate
c        yloc    R  UTM nortihing coordinate
c        angle   R  angle of rotation
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
c
c-----------------------------------------------------------------------
c   Argument Declerations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      real*4    xloc
      real*4    yloc
      real*4    angle
c
c-----------------------------------------------------------------------
c   Local Parameters:
c----------------------------------------------------------------------
c
c   PICVT   R   converion factor for degrees to radians
c
      real*4 PICVT
c
      parameter( PICVT = 0.0174532 )
c
c-----------------------------------------------------------------------
c   Local Variables:
c----------------------------------------------------------------------
c
c   rangle  R  angle between point and origin
c
      integer*4 icell, jcell
      real*4    rangle, xdiff, ydiff
c
c-----------------------------------------------------------------------
c   Entry Point:
c----------------------------------------------------------------------
c
c   --- initlialize error flag ---
c
      ierr = IFAIL
c
c   --- find distance from origin in x direction on rotated grid ---
c
      rangle = PICVT * angle
      xdiff = (xloc * cos(rangle) + yloc * sin(rangle) ) -
     &              (xorig * cos(rangle) + yorig * sin(rangle))
c
c   --- find distance from origin in y direction on rotated grid ---
c
      ydiff = (-xloc * sin(rangle) + yloc * cos(rangle) ) -
     &              (-xorig * sin(rangle) + yorig * cos(rangle))
c
c   ---- calculate grid cell indexes ---
c
      if( xdiff .LT. 0 .OR. ydiff .LT. 0 ) goto 9999
      icell = INT(xdiff/dxcell) + 1
      jcell = INT(ydiff/dycell) + 1
      if( icell .LE. 0 .OR. icell .GT. nxcell .OR. jcell .LE. 0 .or.
     &                                    jcell .GT. nycell ) goto 9999
c
c   --- set error flag to sucess ---
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
