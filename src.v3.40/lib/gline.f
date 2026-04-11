C**** GLINE.F 
c
      subroutine gline(ierr,kelktr,kcell,frac,ux1,uy1,ux2,uy2)
c
c-----------------------------------------------------------------------
c
c     Determine the distribution of links in the grid cells
c
c     Argument description:
c       Outputs:
c         ierr   I   error flag
c         kelktr I   number of cells crossed by link
c         kcell  I   cells crossed by the link
c         frac   R   fraction of total distance in the cell
c       Inputs:
c         ux1    R   beginning X corrdinate
c         uy1    R   beginning Y coordinate
c         ux2    R   ending X corrdinate
c         uy2    R   ending Y coordinate
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 kelktr
      integer*4 kcell(MXLKCL)
      real*4    frac(MXLKCL)
      real*4    ux1
      real*4    uy1
      real*4    ux2
      real*4    uy2
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 ns, nss, imin, imax, jmin, jmax, k, jt, it, ncol
      integer*4 n, ii, jmxi, jmni, nrs, joff, m, mmm, jj
      real*4    x1gc, x2gc, y1gc, y2gc, dxe, dye, sl, cpty, xt, yt, dxi
      real*4    y1, y2, col, dyi, dyii
      logical*4 lden, lden2
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- get grid coordinates of nodes and associated quantities ---
c
      kelktr = 1
      x1gc = (ux1 - xorig) / dxcell
      x2gc = (ux2 - xorig) / dxcell
      y1gc = (uy1 - yorig) / dycell
      y2gc = (uy2 - yorig) / dycell
c
      dxe = x2gc - x1gc
      dye = y2gc - y1gc
      sl = 1.0e+04
      if (ABS(dxe) .GE. 0.001) sl = dye / dxe
      ns = 0
      nss = 1
      if (sl .LT. 0.) ns = 1
      if (sl .LT. 0.) nss = 0
      cpty = y1gc - sl * x1gc
      dxe = ABS(dxe)
      dye = ABS(dye)
c
c   ---- zero fractional contribution array and cell ----
c   ---- location array ----
c
      do k=1,MXLKCL
        frac(k) = 0.
        kcell(k) = 0
      enddo
c
c   ---- determine minimum and maximum coordinates for the link ----
c
      imin = INT(x1gc) + 1
      if (x1gc .LT. 0.) imin = imin - 1
      imax = INT(x2gc) + 1
      if (x2gc .LT. 0.) imax = imax - 1
      jmin = INT(y1gc) + 1
      if (y1gc .LT. 0.) jmin = jmin - 1
      jmax = INT(y2gc) + 1
      if (y2gc .LT. 0.) jmax = jmax - 1
      if (y2gc .GE. y1gc) goto 111
      jt = jmax
      jmax = jmin
      jmin = jt
c
 111  continue
      if (x2gc .GE. x1gc) goto 222
      it = imax
      imax = imin
      imin = it
      xt = x2gc
      x2gc = x1gc
      x1gc = xt
      yt = y2gc
      y2gc = y1gc
      y1gc = yt
c
 222  continue
c
c   ---- determine fraction of link in each column ----
c
      ncol = imax - imin + 1
      dxi = 1.0 / FLOAT(ncol)
      lden = dxe .GE. 0.001
      if (lden) dxi = 1.0 / dxe
c
      do 10 n=1,ncol
        ii = imin + n - 1
        if ((ii .LT. 1) .OR. (ii .GT. nxcell)) goto 10
        if (n .GT. 1) goto 333
        y1 = y1gc
        col = dxi
        if (lden) col = (FLOAT(imin) - x1gc) * dxi
        y2 = sl * FLOAT(imin) + cpty
        if (ncol .EQ. 1) col = 1.0
        if (ncol .EQ. 1) y2 = y2gc
        goto 444
c
 333     continue
        if (n .EQ. ncol) goto 555
        y1 = sl * FLOAT(ii - 1) + cpty
        col = dxi
        y2 = sl * FLOAT(ii) + cpty
        goto 444
c
 555     continue
        y1 = sl * FLOAT(imax - 1) + cpty
        col = dxi
        if (lden) col = (x2gc - FLOAT(imax - 1)) * dxi
        y2 = y2gc
 444     continue
c
c   ---- for the portion of a link in each column, determine the ----
c   ---- fraction in each row ----
c
        jmxi = INT(y2) + 1
        if (y2 .LT. 0.) jmxi = jmxi - 1
        jmni = INT(y1) + 1
        if (y1 .LT. 0.) jmni = jmni - 1
        if (y2 .GE. y1) goto 666
        jt = jmxi
        jmxi = jmni
        jmni = jt
        yt = y2
        y2 = y1
        y1 = yt
c
 666     continue
        nrs = jmxi - jmni + 1
        dyi = y2 - y1
        dyii = 1.0 / FLOAT(nrs)
        lden2 = dyi .GE. 0.001
        if (lden2) dyii = 1.0 / dyi
        joff = jmni - jmin
        do m=1,nrs
           mmm = joff + nss * m + ns * (nrs - m + 1)
           jj = jmin + mmm - 1
           if ((jj .LT. 1) .OR. (jj .GT. nycell)) goto 777
           if (((m .GT. 1) .AND. (nss .EQ. 1)) .OR.
     &                     ((m .LT. nrs) .AND. (ns .EQ. 1)))  goto 888
           frac(kelktr) = col * dyii
           if (lden2) frac(kelktr) = col * (FLOAT(jmni) - y1) * dyii
           if (nrs .EQ. 1) frac(kelktr) = col
           if (frac(kelktr) .GT. 4.9e-5) then
              kcell(kelktr) = ii*1000 + jj
              kelktr = kelktr + 1
              if( kelktr .GT. MXLKCL ) goto 7000
           endif
           goto 777
c
 888       continue
           if (((m .EQ. nrs) .AND. (nss .EQ. 1)) .OR.
     &                     ((m .EQ. 1) .AND. (ns .EQ. 1)))  goto 122
           frac(kelktr) = col * dyii
           if (frac(kelktr) .GT. 4.9e-5) then
              kcell(kelktr) = ii*1000 + jj
              kelktr = kelktr + 1
              if( kelktr .GT. MXLKCL ) goto 7000
           endif
           goto 777
 122       continue
           frac(kelktr) = col * dyii
           if (lden2) frac(kelktr) = col*(y2 - FLOAT(jmxi - 1)) * dyii
           if (nrs .EQ. 1) frac(kelktr) = col
           if (frac(kelktr) .GT. 4.9e-5) then
              kcell(kelktr) = ii*1000 + jj
              kelktr = kelktr + 1
              if( kelktr .GT. MXLKCL ) goto 7000
           endif
 777       continue
        enddo
 10   continue
c
      kelktr = kelktr - 1
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of cells',
     &                                 ' crossed by links exceeds max.'
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXLKCL ',
     &                                                 'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
