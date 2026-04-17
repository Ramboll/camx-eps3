c**** CLOCK.F
c
      logical*4 function clock( ierr, xvert, yvert, nvert )
c
c-----------------------------------------------------------------------
c
c    This routine determines if a set of polygon vertices are in a 
c    clockwise or anti-clockwise order.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c    NB. the polygon being tested must not have intersecting or 
c    coincident sides or equivalent points
c
c    reference : K.B. Salomon.
c    Algorithm for Determining the Orientation of a Boundary
c    Computers & Geosclences, Vol 3, pp 383 - 384
c
c   Argument Declaration:
c   Inputs:
c      xvert  R  array of X coordinates of the polygon
c      yvert  R  array of Y coordinates of the polygon
c      nvert  I  number of vertices in the polygon
c   Outputs:
c      Returns: 
c       .TRUE.  if co-ordinates are clockwise
c       .FALSE. if co-ordinates are anti-clockwise
c      ierr   I  error code
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
c    Argument decelarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 nvert
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
      integer*4 ivert, itop
      real*4    xpre, ypre, xsub, ysub
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = ISUCES
c
c   ---- find vertex with maximum y co-ordlnate ---
c
      itop = 1
      do ivert = 2, nvert
         if( yvert(ivert) .GT. yvert(itop) ) itop = ivert
      enddo
c
c   ----  calculate distance to previous and subsequent points ----
c
      if( itop .EQ. 1 ) then
         xpre = xvert(1) - xvert(nvert)
         ypre = yvert(1) - yvert(nvert)
      else
         xpre = xvert(itop) - xvert(itop-1)
         ypre = yvert(itop) - yvert(itop -1)
      endif
     
      if( itop .EQ. nvert ) then
         xsub =xvert(nvert) - xvert(1)
         ysub =yvert(nvert) - yvert(1)
      else
         xsub =xvert(itop) - xvert(itop + 1)
         ysub =yvert(itop) - yvert(itop + 1)
      endif
c
c   ----- check that the previous and subsequent sides are not coincident ---
c
      if( ypre*xsub .NE. ysub*xpre ) then
         clock = ypre*xsub .LT. ysub * xpre
      else
         ierr = IFAIL
         goto 9999
      endif
c
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
