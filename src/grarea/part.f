c**** PART.F
c
      subroutine part( ierr, xmin, xinc, ymin, yinc, grid, igrid,
     &                        mcol, mrow, xvert, yvert, nvert, okdir )
c
c-----------------------------------------------------------------------
c
c    This subroutine calculates grid cell areas partially contained 
c    within a given polygon.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c   Argument Declaration:
c   Inputs:
c       xmin   R  easting minimum
c       xinc   R  easting grid increment
c       ymin   R  northing minimum
c       yinc   R  northing grid increment
c       mcol   I  number of grid columns
c       mrow   I  number of grid rows
c       xvert  R  array of X-coordinates of the polygon
c       yvert  R  array of Y-coordinates of the polygon
c       nvert  I  number of vertices in the polygon
c       okdir  L  .TRUE. = clockwise polygon vertices
c   Outputs:
c       ierr   I  error code
c       grid   R  areas of grid cells partially in the polygon
c       igrid  I  flag of grid cells partially in the polygon
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
      integer*4 mcol
      integer*4 mrow
      integer*4 nvert
      real*4    xvert(nvert)
      real*4    yvert(nvert)
      logical*4 okdir
      real*4    grid(mcol,mrow)
      integer*4 igrid(mcol,mrow)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      integer*4 ivert, ist, iend, idir, ix1, iy1, ix2, iy2
      integer*4 ix, iy, iright, iyst, ibelow, iyend, i
      real*4    x1, x2, y1, y2, xmin, ymin, xinc, yinc, xgrad
      real*4    ygrad, xint, sx, sy, yrec, xx, yy, xleft, yint 
      real*4    xend, yend, yst
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
      ierr = ISUCES
c
c --- Initialize end point of preceeding polygon edge
c     okdir .TRUE. means polygon is clockwise ----
c
      if( okdir ) then
        ist = 1
        iend = nvert
        idir = 1
      else
        ist = nvert
        iend = 1
        idir = -1
      endif
      x2 = xvert(iend)
      y2 = yvert(iend)
      ix2 = INT((x2 - xmin) / xinc) + 1
      iy2 = INT((y2 - ymin) / yinc) + 1
      ix2 = MAX(MIN(ix2,mcol),1)
      iy2 = MAX(MIN(iy2,mrow),1)
c
c --- Calculate areas of partial cells along each of the polygon edges ----
c
      do ivert=ist, iend, idir
        x1 = x2
        y1 = y2
        ix1 = ix2
        iy1 = iy2
        x2 = xvert(ivert)
        y2 = yvert(ivert)
        ix2 = int((x2 - xmin) / xinc) + 1
        iy2 = int((y2 - ymin) / yinc) + 1
        ix2 = MAX(MIN(ix2,mcol),1)
        iy2 = MAX(MIN(iy2,mrow),1)
c
c --- Calculate inverse gradient, x-intercept and flag to determine if 
c     included area is to the left (iright=-1) or to the right 
c     (iright = 1) of the edge, these items are only required if 
c     y2 not equal to y1 ----
c
        if( y2 .NE. y1 ) then
          xgrad = (x2 - x1) / (y2 - y1)
          xint = x1 - xgrad * y1
          if( y2 .GT. y1 ) then
            iright = 1
          else
            iright = -1
          endif
        else
          xgrad = 0.0
          xint = 0.0
          iright = 1
        endif
c
c --- Calculate gradient, y-intercept and flag to determine if included
c     area is below (ibelow = 1) or above (ibelow = -1) the edge ----
c
        if( x2 .NE. x1 ) then
          ygrad = (y2 - y1) / (x2 - x1)
          yint = y1 - ygrad * x1
          if( x2 .GT. x1 ) then
            ibelow = 1
          else
            ibelow = -1
          endif
        else
          ygrad = 0.0
          yint = 0.0
          ibelow = 1
        endif
c
c --- Initialize end points of preceeding column ----
c               
        yend = y1
        iyend = iy1
c
c --- For each column crossed by the polygon edge calculate the 
c     areas of the partial cells ----
c
        do ix=ix1, ix2, ibelow
          yst = yend
          iyst = iyend
          if( ix .EQ. ix2 ) then
            yend = y2
            iyend = iy2
          else
            xend = xmin + (ix + (ibelow - 1) / 2)* xinc
            yend = ygrad * xend + yint
            yend = max(yend, ymin)
            iyend = int((yend - ymin) / yinc) + 1
          endif
c
c --- Indicate that this grid cell has an inside/outside change and
c     calculate the area of the rectangle below the intersection of
c     the polygon edge and the right hand edge of the cell ----
c
          if( x2 .GT. x1 ) then
            if( ix .NE. ix2 ) then
                igrid(ix, iyend) = -1*igrid(ix,iyend)
                yrec             = yend - ymin - (iyend - 1)*yinc
                grid(ix, iyend)  = grid(ix,iyend) + ibelow*xinc*yrec
            endif
          elseif( x2 .LT. x1 ) then
            if( ix .NE. ix1 ) then
               igrid(ix, iyst)  = -1*igrid(ix, iyst)
               yrec             = yst - ymin - (iyst - 1)*yinc
               grid(ix, iyst)   = grid(ix, iyst) + ibelow*xinc*yrec
            endif
          endif
c
c --- For each row within this column, calculate area of
c     the trapezium to the left of the polygon edge ---
c
c     Note: the expression (yy - sy) determines if an addition,
c     subtraction or no operation is required
c
          yy = yst
          xx = xgrad*yy + xint
          xleft = xmin + (ix -1)*xinc
          do iy=iyst,iyend,iright
            sy = yy
            if( iy .EQ. iyend ) then
               yy = yend
            else
               yy = ymin + (iy + (iright - 1) / 2) * yinc
            endif
            sx = xx
            xx = xgrad * yy + xint
            grid(ix, iy) = grid(ix,iy)-(yy-sy) *
     &                               (0.5*(xx + sx) - xleft)
          enddo
        enddo
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
