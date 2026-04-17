C**** WRFASC.F 
c
      subroutine wrfasc( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the FIPS/ASC totals file to be read by the GRDCTL program.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
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
      include 'msgtot.inc'
      include 'iogrdem.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*5  fiptmp
      character*10 asctmp
      integer*4    i, jspc
      real*4       tottmp
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- write the header that defines the species list ---
c
      write(IOWASC,ERR=7000) ncrthd, nspchd
      write(IOWASC,ERR=7000) (crtnam(i),isarod(i),i=1,ncrthd)
      write(IOWASC,ERR=7000 ) (spclst(i),molwt(i),spcrit(i),i=1,nspchd)
c
c   --- loop over all county/source category pairs ---
c
      do i=1,nmsgfcd
c
c   --- see if there is non-zero emissions ---
c
         tottmp = 0.
         do jspc=1,ncrthd
            if( crtofcd(i,jspc) .GT. 0. )
     &                                tottmp = tottmp + crtofcd(i,jspc)
         enddo
c
c   ---- write the record ---
c
         if( tottmp .GT. 0 ) then
            fiptmp = msgfcd(i)(1:5)
            asctmp = msgfcd(i)(7:16)
            write(IOWASC,ERR=7000) fiptmp, asctmp, 
     &                               (crtofcd(i,jspc),jspc=1,ncrthd)
         endif
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing FIPS/ASC totals file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',ascfl(:strmin(ascfl))
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
