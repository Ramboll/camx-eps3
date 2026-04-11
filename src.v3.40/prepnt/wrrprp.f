C**** WRRPRP.F 
c
      subroutine wrrprp( ierr,
     &                         nfac )
c
c-----------------------------------------------------------------------
c
c    Writes the EMBR records for the current facility and sets
c    the arrays to missing
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       nfac   I number of records currently in arrays
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
      include 'embrdat.inc'
      include 'sccxref.inc'
      include 'ioprepnt.inc'
      include 'usrprpnt.inc'
      include 'prpntstk.inc'
      include 'prepntot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 nfac
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 ifac, jerr, i
      real*4    tottmp
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL

c   --- write each record to EMBR file ---
c
      do 10 ifac = 1,nfac
c
c   --- skip records with no emissions ---
c
         tottmp = 0.
         do i=1,MXCRIT
            if( crtpol(i,ifac) .GT. 0.) tottmp = tottmp + crtpol(i,ifac)
         enddo
c
c   --- set all EMBR common variables ---
c
         if( tottmp .LE. 0.0 ) goto 10
         fipin = infips(1)
         sbrgn = insbrg(1)
         siccd = insic(ifac)
         scccd = inscc(ifac)
         if( plmhit(ifac) .GE. pcutof ) then
             rectyp = 'E'
             nrec(IDXELE) = nrec(IDXELE) + 1
         else
             rectyp = 'L'
             nrec(IDXLOW) = nrec(IDXLOW) + 1
         endif
         invbeg = ibegdt(ifac)
         invend = ienddt(ifac)
         plant = inplnt(1)
         stkid = instck(ifac)
         point = inpnt(ifac)
         segno = inseg(ifac)
         utme = xloc(ifac)
         utmn = yloc(ifac)
         iutmzn = inzone(1)
         stkgev = ABS(stexvl(ifac))
         stkdia = ABS(stdiam(ifac))
         stktmp = ABS(stextp(ifac))
         stkhgt = ABS(stkht(ifac))
         do i=1,ncrthd
            if( crtpol(i,ifac) .GT. 0. ) then
                emout(i) = crtpol(i,ifac)
            else
                emout(i) = RMISS
            endif
c
c  --- set stack array vairables to missing ---
c
            crtpol(i,ifac) = 0.
         enddo
c
c   --- call routine to write EMBR record ---
c
         call wrembr( jerr,
     &                      IOWMBR, nrec(IDXOUT) )
         if( jerr .NE. ISUCES ) goto 9999
         nrec(IDXOUT) = nrec(IDXOUT) + 1
c
   10 continue
c
c   --- set the id arrays to missing ---
c
      infips(1) = '     '
      insbrg(1) = '     '
      inplnt(1) = '          '
      do i=1,nfac
         instck(i) = '          '
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
