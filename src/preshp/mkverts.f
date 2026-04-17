C*** MKVERTS.F 
c
      subroutine mkverts(nvert, xvert, yvert, shiparea, 
     &                        distpts, center_slope, perp_slope, theta,
     &                xptbeg, yptbeg, distbeg, xptend, yptend, distend )
c
c-----------------------------------------------------------------------
c
c    This routine calculates the vertices of the polygon making
c    up the shipping channel. It also calculates the total area 
c    of this polygon. The shipping channel is designated by the
c    coordinates of the end and beginning of the lane and a 
c    width at each end. A line segment is calculated connecting
c    these endpoints and the widths are layed out perpindicularly.
c
c    Argument Declaration.
c     Outputs:
c       nvert        I  number of verices (always 4 for this application)
c       xvert        R  array of X-coordinates of the vertices
c       yvert        R  array of Y-coordinates of the vertices
c       shiparea     R  area of the shipping lane
c       distpts      R  distance along center line of the channel
c       center_slope R  slope of centerline of the channel
c       perp_slope   R  slope of line perpindicular to centerline
c       theta        R  angle of centerline with horizontal
c     Inputs:
c       xptbeg       R  X-coordinate of the beginning of shipping channel
c       yptbeg       R  Y-coordinate of the beginning of shipping channel
c       distbeg      R  distance of the width at the beginning
c       xptend       R  X-coordinate of the end of shipping channel
c       yptend       R  Y-coordinate of the end of shipping channel
c       distend      R  distance of the width at the end
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 nvert
      real*4    xvert(*)
      real*4    yvert(*)
      real*4    shiparea
      real*4    distpts
      real*4    center_slope
      real*4    perp_slope
      real*4    theta
      real*4    xptbeg
      real*4    yptbeg
      real*4    distbeg
      real*4    xptend
      real*4    yptend
      real*4    distend
c
c-----------------------------------------------------------------------
c  Local Parameters:
c-----------------------------------------------------------------------
c
c    PI   R  approximation of pi
c
      real*4 PI
c
      parameter( PI = 3.141592653 )
c
c-----------------------------------------------------------------------
c  Local Variables
c-----------------------------------------------------------------------
c
      real*4    yint_beg, yint_end
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- always a 4-gon (conclusion? - Oh, I kill myself)
c
       nvert = 4
c
c   --- first calculate the area of the polygon ---
c
      distpts = sqrt( (xptend-xptbeg)**2 + (yptend-yptbeg)**2 )
      shiparea = distpts * 0.5 * (distbeg + distend) 
c
c   ---- make sure it is not a vertical line, 
c        or a horizonatal line ---
c
      if( (xptbeg .NE. xptend) .AND. (yptbeg .NE. yptend) ) then
c
c   ---- calculate the slope of the line segment,
c        and the slope of a line perpindicular to it,
c        and the angle of rotation ---
c
         center_slope = (yptend - yptbeg) / (xptend - xptbeg)
         theta = ATAN( (yptend - yptbeg) / (xptend - xptbeg) )
c
         perp_slope = -1.0 / center_slope
         yint_beg = yptbeg - perp_slope * xptbeg
         yint_end = yptend - perp_slope * xptend
c
c  ---- calculate the point "above" this point on the same line as
c       the beginning point ---
c
         xvert(1) = xptbeg - 
     &                sqrt( (distbeg/2.0)**2 / (1 + perp_slope**2 ) )
         yvert(1) = perp_slope * xvert(1) + yint_beg
c
c  ---- calculate the point "below" this point on the same line as
c       the beginning point ---
c
         xvert(2) = xptbeg + 
     &                sqrt( (distbeg/2.0)**2 / (1 + perp_slope**2 ) )
         yvert(2) = perp_slope * xvert(2) + yint_beg
c
c  ---- calculate the point "above" this point on the same line as
c       the ending point ---
c
         xvert(3) = xptend + 
     &                sqrt( (distend/2.0)**2 / (1 + perp_slope**2 ) )
         yvert(3) = perp_slope * xvert(3) + yint_end
c
c  ---- calculate the point "below" this point on the same line as
c       the ending point ---
c
         xvert(4) = xptend - 
     &                sqrt( (distend/2.0)**2 / (1 + perp_slope**2 ) )
         yvert(4) = perp_slope * xvert(4) + yint_end
c
c  --- deal with a horizontal line ---
c
      else if( yptbeg .EQ. yptend ) then
         center_slope = 0.0
         theta = 0.
         xvert(1) = xptbeg
         yvert(1) = yptbeg + distbeg / 2.0
c
         xvert(2) = xptbeg
         yvert(2) = yptbeg - distbeg / 2.0
c
         xvert(3) = xptend
         yvert(3) = yptend - distend / 2.0
c
         xvert(4) = xptend
         yvert(4) = yptend + distend / 2.0
c
c  --- deal with a vertical line ---
c
      else if( xptbeg .EQ. xptend ) then
         perp_slope = 0.0
         theta = PI/2.0
         xvert(1) = xptbeg + distbeg / 2.0
         yvert(1) = yptbeg
c
         xvert(2) = xptbeg - distbeg / 2.0
         yvert(2) = yptbeg
c
         xvert(3) = xptend - distend / 2.0
         yvert(3) = yptend
c
         xvert(4) = xptend + distend / 2.0
         yvert(4) = yptend
      endif
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
