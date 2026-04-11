c**** POIN.F
c
      logical*4 function poin( xpt, ypt, xvert, yvert, nvert )
c
c-----------------------------------------------------------------------
c
c    This routine tests if a point is inside a polygon.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c      Reference:  K.R.Anderson
c            Mathematical Geology, Vol. 8, No. 1, 1976, pp 105 - 106
c
c   Argument Declaration:
c   Inputs:
c     xpt    R  X co-ordlnate of the point
c     ypt    R  Y co-ordlnate of the point
c     xvert  R  array of X coordinates of the polygon
c     yvert  R  array of Y coordinates of the polygon
c     nvert  I  number of vertices in the polygon
c   Outputs:
c      Returns: 
c       .TRUE.  if point is inside the polygon
c       .FALSE. if point is outside or on the polygon
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
      integer*4 nvert
      real*4    xpt 
      real*4    ypt 
      real*4    xvert(nvert)
      real*4    yvert(nvert)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 ivert, ipre
      real*4    grad, xint, xcut
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  ---- count number of polygon sides to the right of the point ---
c
      poin = .FALSE.
      ipre = nvert
      do ivert=1, nvert
          if( ypt .GT. yvert(ipre) .EQV. ypt .LE. yvert(ivert) ) then
               grad = (xvert(ivert) - xvert(ipre))/
     &                                      (yvert(ivert) - yvert(ipre))
               xint = xvert(ipre) - grad * yvert(ipre)
               xcut = grad * ypt + xint
               if( xpt .LT. xcut ) poin = .NOT. poin
          endif
          ipre = ivert
      enddo
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
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
