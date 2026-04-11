c**** INTER.F
c
      logical*4 function inter( xa1, ya1, xa2, ya2, xb1, yb1, xb2, yb2 )
c
c-----------------------------------------------------------------------
c
c    This routine determines if two line segments intersect.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c   Argument Declaration:
c   Inputs:
c      xa1  R  X-coordinate of start of the first line
c      ya1  R  Y-coordinate of start of the first line
c      xa2  R  X-coordinate of end of the first line
c      ya2  R  Y-coordinate of end of the first line
c      xb1  R  X-coordinate of start of the second line
c      yb1  R  Y-coordinate of start of the second line
c      xb2  R  X-coordinate of end of the second line
c      yb2  R  Y-coordinate of end of the second line
c   Outputs:
c       .TRUE.  if an intersection is found
c       .FALSE. if no intersection is found
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
      real*4 xa1
      real*4 ya1
      real*4 xa2
      real*4 ya2
      real*4 xb1
      real*4 yb1
      real*4 xb2
      real*4 yb2
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      real*4 xamin, yamin, xamax, yamax, xbmin, ybmin, xbmax, ybmax 
      real*4 grada, gradb, yinta, yintb, xcross, ycross
      real*4 toler
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data toler/0.1e-8/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c ----  sort end point co-ordinates ---
c
      call sort2(xa1, xa2, xamin, xamax)
      call sort2(ya1, ya2, yamin, yamax)
      call sort2(xb1, xb2, xbmin, xbmax)
      call sort2(yb1, yb2, ybmin, ybmax)
c
c ---- check if the surrounding rectangles of the llne 
c      seqments overlap ----
c
      if( xamin .LE. xbmax .AND. xamax .GE. xbmin .AND. 
     &                yamin .LE. ybmax .AND. yamax .GE. ybmin ) then
c
c  ---- check if first line is vertical ----
c
         if( ABS(xa2 - xa1) .LT. toler ) then
c
c  ---- check if second llne is vertical ----
c
             if( ABS(xb2 - xb1) .LT. toler ) then
                inter = .TRUE.
             else
c
c  ---- find point where the two lines intersect ----
c
                gradb = (yb2-yb1)/(xb2-xb1)
                yintb = yb1-gradb*xb1
c
                ycross = gradb*xa1 + yintb
c
c  ---- check if intersection is on the first line segment ----
c           
                inter = ycross .GE. yamin .AND. ycross .LE. yamax
             endif
         else
c
c  ---- check if second line is vertical ----
c
             if( ABS(xb2 - xb1) .LT. toler ) then
c
c  ---- find point where the two lines intersect ----
c
                  grada = (ya2 - ya1) / (xa2 - xa1)
                  yinta = ya1 - grada * xa1
                  ycross = grada * xb1 + yinta
c
c  ---- check if intersection is on the second line segment ----
c
                  inter = ycross .GE. ybmin .AND. ycross .LE. ybmax
             else
                  grada = (ya2 - ya1) / (xa2 - xa1)
                  yinta = ya1 - grada * xa1
                  gradb = (yb2 - yb1) / (xb2 - xb1)
                  yintb = yb1 - gradb * xb1
c
c  ---- check if lines are parallel ----
c
                  if( ABS(grada - gradb) .LT. toler ) then
c
c  ---- check if lines are coincident ----
c
                        inter = ABS(yinta - yintb) .LT. toler
                  else
c
c  ---- find the intersection between the two lines ----
c
                        xcross = (yintb - yinta) / (grada - gradb)
                        ycross = grada * xcross + yinta
c
c  ---- check if intersection is within the line segments ----
c
                        inter = xcross .GE. xamin .AND. 
     &                          xcross .LE. xamax .AND. 
     &                          xcross .GE. xbmin .AND. 
     &                          xcross .LE. xbmax .AND.
     &                          ycross .GE. yamin .AND. 
     &                          ycross .LE. yamax .AND.
     &                          ycross .GE. ybmin .AND. 
     &                          ycross .LE. ybmax

                  endif
             endif
         endif
      else
        inter = .FALSE.
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
