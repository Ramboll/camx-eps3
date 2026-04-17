c**** GRAREA.F
c
      subroutine grarea( ierr, iclxbeg, iclxend, 
     &                        jclybeg, jclyend, gridarea, 
     &                                   xvert, yvert, nvert, shiparea )
c
c-----------------------------------------------------------------------
c
c    This subroutine is the main driver for the GRAREA polygon
c    gridding algoirthm. 
c
c     code by Brett Larkin, Computers & Geoscience (1988), v14, pp1-14
c     adapted for EPS3 by ENVIRON International Corp, 2009
c
c
c   Argument Declaration:
c   Inputs:
c       ierr     I error code
c       xvert    R array of X-coordinates of the polygon
c       yvert    R array of Y-coordinates of the polygon
c       nvert    I number of vertices in the first polygon
c       shiparea R total area of the shipping lane
c   Outputs:
c       iclxbeg  I first X grid cell that intersects the polygon
c       iclxend  I last X grid cell that intersects the polygon
c       jclybeg  I first Y grid cell that intersects the polygon
c       jclyend  I last Y grid cell that intersects the polygon
c       gridarea R amount of polygon area in each grid cell
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
      include 'uamregn.inc'            
c
c-----------------------------------------------------------------------
c    Argument decelarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr 	
      integer*4 iclxbeg
      integer*4 iclxend
      integer*4 jclybeg
      integer*4 jclyend
      real*4    gridarea(MXCELX,MXCELY)
      integer*4 nvert
      real*4    xvert(nvert)
      real*4    yvert(nvert)
      real*4    shiparea
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 ipolys
      logical*4 clock
c
c-----------------------------------------------------------------------
c   Local Parameters:
c-----------------------------------------------------------------------
c
c  MXEXTENDX  I  amount of cells to extend the domain in the X-direction  
c  MXEXTENDY  I  amount of cells to extend the domain in the Y-direction
c
      integer*4 MXEXTENDX
      integer*4 MXEXTENDY
c
      parameter( MXEXTENDX = MXCELX )
      parameter( MXEXTENDY = MXCELY )
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 igrid(3*MXEXTENDX,3*MXEXTENDY)
      integer*4 ncol, nrow, icltmp, jcltmp         
      integer*4 ncount, in, icladj, jcladj, i, j
      real*4    xvertadj(MXVERT), yvertadj(MXVERT)
      real*4    tmparea(3*MXEXTENDX,3*MXEXTENDY)
      real*4    xmin, xmax, xinc, ymin, ymax, yinc, area
      logical*4 okdir, lfill
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
c  ---- set the grid parameters based on the modeling grid ---
c
      xmin = xorig
      xmax = xorig + (nxcell-1)*dxcell
      xinc = dxcell
      ncol = nxcell
      ymin = yorig
      ymax = yorig + (nycell-1)*dycell
      yinc = dycell
      nrow = nycell
c
c  ---- find the beginning and ending of where this polyomial
c       intersects the grid ----
c
      iclxbeg = nxcell+1
      iclxend = 0
      jclybeg = nycell+1
      jclyend = 0
      do i=1,nvert
        call setcell(icltmp, jcltmp, xvert(i), yvert(i), 0. )
        iclxbeg = MIN(iclxbeg,icltmp)
        iclxend = MAX(iclxend,icltmp)
        jclybeg = MIN(jclybeg,jcltmp)
        jclyend = MAX(jclyend,jcltmp)
      enddo
c
c  --- skip if outside the domain ----
c
      if( iclxbeg .GT. nxcell .OR. iclxend .LE. 0 .OR. 
     &                    jclybeg .GT. nycell .OR. jclyend .LE. 0 ) then
         ierr = ISKIP
         goto 9999
      endif
c
c  --- make sure there is space for the 
c
      if(iclxbeg + MXEXTENDX .LE. 0 ) goto 7000
      if(iclxend + MXEXTENDX .GT. 3*MXEXTENDX ) goto 7000
      if(jclybeg + MXEXTENDY .LE. 0 ) goto 7000
      if(jclyend + MXEXTENDY .GT. 3*MXEXTENDY ) goto 7000
c
c  ---- clip cell min/max to the domain edges ----
c
      iclxbeg = MAX(1,iclxbeg)
      iclxend = MIN(nxcell,iclxend)
      jclybeg = MAX(1,jclybeg)
      jclyend = MIN(nycell,jclyend)
c
c  ---- initialize grid values ----
c
      do i=1,MXCELX
        do j=1,MXCELY
           gridarea(i,j) = 0.0
        enddo
      enddo
      do i=1,3*MXEXTENDX
        do j=1,3*MXEXTENDY
           tmparea(i,j) = 0.0
           igrid(i,j) = 1
        enddo
      enddo
c
c  ---- if the entire shipping lane is on one cell,
c       put all of the area in that cell and return ---
c
      if( iclxbeg .EQ. iclxend .AND. jclybeg .EQ. jclyend ) then
        if( iclxbeg .GT. 0 .AND. iclxbeg .LE. nxcell .AND. 
     &                 jclybeg .GT. 0 .AND. jclybeg .LE. nycell ) then
            gridarea(iclxbeg,jclybeg) = shiparea
        endif
        goto 9999
      endif
c
c ---- adjust the region coordinates to be in the extended domain ---
c
      xmin = xmin - MXEXTENDX * dxcell
      ymin = ymin - MXEXTENDY * dycell
c
c ---- determine order to scan including polygon vertices ---
c
      okdir = clock(ierr, xvert, yvert, nvert)
      if( ierr .NE. ISUCES ) goto 9999
c
c ---- calculate partial grid cells for the including polygon ---
c
      call part(ierr, xmin, xinc, ymin, yinc, tmparea, igrid, 
     &           3*MXEXTENDX, 3*MXEXTENDY, xvert, yvert, nvert, okdir)
      if( ierr .NE. ISUCES ) goto 9999
c
c ---- first fill in the output array ---
c
      do i=1,3*MXEXTENDX
         do j=1,3*MXEXTENDY
c
c  ---- adjust the cell for the regular domain ---
c
             icladj = i - MXEXTENDX
             jcladj = j - MXEXTENDY
c
c  ---- if inside the domain, fill in the area ---
c
              if( icladj .GT. 0 .AND. icladj .LE. nxcell .AND. 
     &                     jcladj .GT. 0 .AND. jcladj .LE. nycell ) then
                  gridarea(icladj,jcladj) = tmparea(i,j)
              endif
         enddo
      enddo
c
c ---- now fill in the non partial grid cells ----
c
      area = xinc * yinc
c
      do 10 i=1,3*MXEXTENDX
c
c  --- adjust the cell for the normal domain ---
c
        icladj = i - MXEXTENDX
c
c  --- figure out if column only has "inside" cells ---
c
        ncount = 0
        do j=1,3*MXEXTENDY
           if( igrid(i,j) .LT. 0 ) ncount = ncount + 1
        enddo
c
c  --- if less than 2 "inside" cells, just return ---
c
        if( ncount .LT. 2 ) goto 10
c
c  --- fill in areas of inside cells ---
c
        in = -1
        do 20 j=1,3*MXEXTENDY
          in = igrid(i,j) * in
c
c  --- adjust the cell for the normal domain ---
c
          jcladj = j - MXEXTENDY
c
c  --- check if this is a complete cell ---
c
          if( in .EQ. 1 ) then
c
c  --- only fill if the cell is in the regular domain ----
c
            if( icladj .GT. 0 .AND. icladj .LE. nxcell .AND. 
     &                     jcladj .GT. 0 .AND. jcladj .LE. nycell ) then
                gridarea(icladj,jcladj) = gridarea(icladj,jcladj) + area
            endif
          endif
   20   continue 
   10 continue
c
c ---- calculate the are of the polygon ----
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
 7000 continue
      write(ioerr,'(/,2A,/,2A)',ERR=9999) 'ERROR:  Number of cells ',
     &                        'used for the extended domain in GRAREA ',
     &                          '        is not large enough ',
     &                     'to support some of the shipping channels.'
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameters ',
     &             'MXEXTENDX and MXEXTENDY in GRAREA and recompile.'
      ierr = IFAIL
      goto 9999
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
