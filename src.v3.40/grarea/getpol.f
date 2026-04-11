c**** GETPOL.F
c
      subroutine getpol( lupol, ptype, xvert, yvert, ivert, mvert, 
     &                         nvert, ipoly, xmin, xmax, ymin, ymax )
c
c-----------------------------------------------------------------------
c
c    This subroutine obtains and validates a polygon.
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c
c   Argument Declaration:
c   Inputs:
c     lupol   L  logical unit for polygon file
c      ptype  C  'including' or 'excluding' polygon
c      xvert  R  array of X coordinates of the polygon
c      yvert  R  array of X coordinates of the polygon
c      ivert  I  pointer to polygon's first vertice
c      mvert  I  maximum total number of polygon vertices
c      nvert  I  number of vertices in each polygon
c      ipoly  I  polygon number
c      xmin   R  grid minimum in X direction
c      xmax   R  grid maximum in X direction
c      ymin   R  grid minimum in Y direction
c      ymax   R  grid maximum in Y direction
c   Outputs:
c      xvert  R  array of X coordinates of the polygon
c      yvert  R  array of X coordinates of the polygon
c      nvert  I  number of vertices in each polygon
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
      integer*4     lupol
      character*(*) ptype
      integer*4     mvert
      real*4        xvert(mvert)
      real*4        yvert(mvert)
      integer*4     ivert
      integer*4     ipoly
      integer*4     nvert(ipoly)
      real*4        xmin
      real*4        xmax
      real*4        ymin
      real*4        ymax
c
c-----------------------------------------------------------------------
c   Local Parameters:
c-----------------------------------------------------------------------
c
      integer*4 MXFILE
      parameter( MXFILE = 40 )
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      logical*4 cross
      integer*4 ipolys
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      character*(MXFILE) polfil
      integer*4          jvert, kvert, ipol, kpoly, itest
      real*4             xpt, ypt
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c ----  open polygon file ----
c
 111  continue
      write(*,7000) ptype, ipoly
      read(*,7001) polfil
      if( polfil .EQ. ' ' ) goto 222
      open(UNIT=lupol,FILE=polfil,status='OLD',ERR=333)
      goto 444
 333  continue
      write(*,7002) polfil
      goto 111
c          
c ---- read polygon file and store non-equivalent points ----
c
 444  continue
      jvert = ivert
      ipol = 1
      read(lupol,*,ERR=555) xvert(jvert), yvert(jvert)
      ipol = ipol + 1
      read(lupol,*,ERR=555,END=777) xpt, ypt
 888  continue
c
      if( xpt .NE. xvert(jvert) .OR. ypt .NE. yvert(jvert) ) then
           jvert = jvert + 1
           if( jvert .GT. mvert ) then
               write(*,7003) mvert
               goto 111
           endif
           xvert(jvert) = xpt
           yvert(jvert) = ypt
      endif
      ipol = ipol + 1
      read(lupol,*,ERR=555,END=666) xpt, ypt
      goto 888
c
c ---- error reading polygon file ----
c
 555  continue
      write(*,7004) ipol
      close(lupol)
      goto 111
c
c ---- test if last point same as first ----
c
 666  continue
      if( xvert(jvert) .EQ. xvert(ivert) .AND. yvert(jvert) .EQ.
     &                                yvert(ivert) ) jvert = jvert - 1
c
c ---- check that polygon is within the grid limits ----
c
 777  continue
      close(lupol)
c
c ---- check that polygon does not cross itself ----
c     
      nvert(ipoly) = jvert - ivert + 1
      if( cross( xvert(ivert), yvert(ivert), nvert(ipoly)) ) then
         write(*,7005)
         goto 111
      endif
c
c  ---- check that polygon neither intersects or is inside 
c       any previous polygon ----
c          
      kvert = 1
      do kpoly = 1, ipoly - 1
          itest = ipolys( xvert(ivert), yvert(ivert), nvert(ipoly),
     &                      xvert(kvert), yvert(kvert), nvert(kpoly) )

         if( itest .LT. 0 ) then
             write(*,7006) ptype, kpoly
             goto 111
         elseif( itest .EQ. 1 ) then
             write (*,7007) ptype, kpoly
             goto 111
        elseif( itest .EQ. 2 ) then
             write(*,7008) ptype, kpoly
             goto 111
        endif
c
        kvert = kvert + nvert(kpoly)
      enddo
      goto 9999
c
 222  continue
      nvert(ipoly) = 0
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
 7000 format(/,'Enter filename for ',a,' polygon no.',I3,
     &                                            '(blank to end): ',$)
 7001 format(A)
 7002 format(/,'ERROR: Unable to open file ',A20)
 7003 format(/,'ERROR: No of polygon co-ordinates exceeds maximum ',
     &                                 'allocated in program (',I5,')')
 7004 format(/,'ERROR: Polygon vertice no',I6,' is an invalid number')
 7005 format(/,'ERROR: Entered polygon crosses itself')
 7006 format(/,'ERROR: Entered polygon intersects ',A,' polygon no.',I3)
 7007 format(/,'ERROR: Entered polygon is within ',A,' polygon no.',I3)
 7008 format(/,'ERROR: Entered polygon surrounds ',A,' polygon no.',I3)
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
