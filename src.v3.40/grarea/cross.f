c**** CROSS.F
c
      logical*4 function cross( xvert, yvert, nvert )
c
c-----------------------------------------------------------------------
c
c    This routine to determines if a polygon has any equivalent 
c    vertices, intersecting sides or coincident sides.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c
c   Argument Declaration:
c   Inputs:
c     xvert  R array of X-coordinates of the polygon
c     yvert  R array of Y-coordinates of the polygon
c      nvert  I  number of vertices in the polygon
c   Outputs:
c      Returns:
c       .TRUE.  if polygon has any equivalent vertices intersecting 
c               or coincident sides
c       .FALSE. if polygon has no equivalent vertices,intersecting 
c               or coincident sides
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
      integer*4 ipre, ivert, jvert, lvert
      logical*4 inter
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- determine if enough vertices for a polygon ----
c
      if( nvert .LE. 2 ) then
          cross = .TRUE.
c
c   ---- test that points of a triangle are not co-linear ----
c
      elseif( nvert .EQ. 3 ) then
           cross = 
     &          (yvert(2) - yvert(1)) * (xvert(3) - xvert(1)) .EQ.
     &          (yvert(3) - yvert(1)) * (xvert(2) - xvert(1))
      else
c
c ---- test each polygon side ----
c     
         lvert = nvert - 1
         ipre = nvert
c
         do ivert = 1, nvert-2
c
c ---- test all further sides against side ivert ----
c
             do jvert = ivert + 2, lvert
                 cross = inter(xvert(ipre), yvert(ipre),xvert(ivert), 
     &                     yvert(ivert),xvert(jvert-1),yvert(jvert-1),
     &                                        xvert(jvert),yvert(jvert))
                 if( cross ) return
             enddo
             ipre = ivert
             lvert = nvert
         enddo
      endif
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
