C*** MKCHANNEL.F 
c
      subroutine mkchannel(xptend, yptend,
     &                          channel_len, xptbeg, yptbeg, 
     &                                center_slope, perp_slope, theta ) 
c
c-----------------------------------------------------------------------
c
c    This routine calculates the coordinates of a new shipping channel
c    based on the geometric features passed in through the argument 
c    list. The end coordinates are calculated using the distance from
c    the beginning coordinates and the angle from horizontal.
c
c    Argument Declaration.
c     Outputs:
c       xptend       R  X-coordinate of the end of shipping channel
c       yptend       R  Y-coordinate of the end of shipping channel
c     Inputs:
c       channel_len  R  length of this channel
c       xptbeg       R  X-coordinate of the beginning of shipping channel
c       yptbeg       R  Y-coordinate of the beginning of shipping channel
c       center_slope R  slope of the centerline for this channel
c       perp_slope   R  slope of the line perpindicular to the centerline
c       theta        R  angle of centerline with horizontal
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
      real*4 xptend
      real*4 yptend
      real*4 channel_len
      real*4 xptbeg
      real*4 yptbeg
      real*4 center_slope
      real*4 perp_slope
      real*4 theta
c
c-----------------------------------------------------------------------
c  Local Parameters:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c  Local Variables
c-----------------------------------------------------------------------
c
       real*4 xtmp, ytmp
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- first deal with a horizontal line ----
c
      if( center_slope .EQ. 0. ) then
         xptend = xptbeg + channel_len
         yptend = yptbeg
c
c   ---- now deal with a vertical line ----
c
      else if( perp_slope .EQ. 0. ) then
         xptend = xptbeg
         yptend = yptbeg + channel_len
c
c   ---- now do the typical case ---
c
      else
         xptend = xptbeg + channel_len*COS( theta )
         yptend = yptbeg + channel_len*SIN( theta )
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
