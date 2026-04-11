C**** WRESPC.F 
c
      subroutine wrespc( ierr,
     &                         irec )
c
c-----------------------------------------------------------------------
c
c    Writes the error records to the EMAR file.  Some header data is
c    written before the first record.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       irec   I record number
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
      include 'iospcems.inc'
      include 'factors.inc'
      include 'totspc.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 irec
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
      integer*4 i, jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- if this is the first record, write the header data ---
c
      if( irec .EQ. 1 ) then
         write(IOWERR,'(A)',ERR=7000) filtyp(:strmin(filtyp))
         write(IOWERR,9000,ERR=7000) rgutme, rgutmn, irgzn, nxrgn,
     &                            nyrgn, dxrgn, dyrgn
         write(IOWERR,'(2I10)',ERR=7000) ncrthd, nspchd
         write(IOWERR,'(100(:,A10,I10))',ERR=7000) (crtnam(i),
     &                                       isarod(i),i=1,ncrthd)
      endif
c
c   --- set all EMBR common variables ---
c
      do i=1,MXCRIT
         emout(i) = emin(i)
      enddo
      do i=1,MXSPEC
         spcout(i) = spcpol(i)
      enddo
c
c   --- call the routine to write the data record ---
c
      call wremar( jerr,
     &                   IOWERR, nrec(IDXERR) )
      if( jerr .NE. ISUCES ) goto 9999
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
     &                       'ERROR:  Writing header to EMAR file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',errfl(:strmin(errfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(2F10.4,3I10,2F10.4)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
