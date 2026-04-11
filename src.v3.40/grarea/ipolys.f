c**** IPOLYS.F
c
      integer*4 function ipolys( xvert1, yvert1, nvert1, xvert2, 
     &                                               yvert2, nvert2 )
c
c-----------------------------------------------------------------------
c
c    This routine tests the relationship between two polygons.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c   Argument Declaration:
c   Inputs:
c       xvert1  R array of X-coordinates of the first polygon
c       yvert1  R array of Y-coordinates of the first polygon
c       nvert1  I number of vertices in the first polygon
c       xvert2  R array of X-coordinates of the second polygon
c       yvert2  R array of Y-coordinates of the second polygon
c       nvert2  I number of vertices in the second polygon
c   Outputs:
c      Returns: 
c        0 if the two polygons are mutually exclusive
c        1 if the first polygon is inside the second
c        2 if the second polygon is inside the first
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
      integer*4 nvert1
      real*4    xvert1(nvert1)
      real*4    yvert1(nvert1)
      integer*4 nvert2
      real*4    xvert2(nvert2)
      real*4    yvert2(nvert2)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      logical*4 inter
      logical*4 poin
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
      integer ipre1, ipre2, ivert1, ivert2
c
c  ----test if polygons intersect
c          loop through first polygon ----

      ipre1 = nvert1
      do ivert1=1, nvert1

! loop thru second polygon

         ipre2 = nvert2

         do ivert2=1, nvert2

            if( inter(xvert1(ipre1), yvert1(ipre1),xvert1(ivert1),
     &                   yvert1(ivert1),xvert2(ipre2),yvert2(ipre2),
     &                             xvert2(ivert2),yvert2(ivert2)) ) then
               ipolys = -1
               goto 9999
            endif
           ipre2 = ivert2
         enddo
         ipre1 = ivert1
      enddo
c
c  ---- find if first point of the first polygon is inside 
c       the second polygon ---
c
      if( poin(xvert1(1),yvert1(1),xvert2,yvert2,nvert2) ) then
          ipolys = 1
c
c  ---- find if first point of the second polygon is inside the 
c       first polygon ----
c
      elseif( poin(xvert2(1), yvert2(1), xvert1, yvert1, nvert1) ) then
          ipolys = 2
      else
          ipolys = 0
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
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
