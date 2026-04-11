C***** WRTUAM.F 
c
      subroutine wrtuam( ierr,
     &                         icurdt )
c
c-----------------------------------------------------------------------
c
c    Writes one hour of gridded emissions to UAM file
c
c    Argument Declaration.
c     Outputs:
c       ierr     I  error flag
c     Inputs:
c       icurhr   I  current date and hour
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
      include 'uamcntl.inc'
      include 'uamregn.inc'
      include 'iogrdem.inc'
      include 'grdemtot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 icurdt(MXHOUR)
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
      character*4 ispecs(10,MXSPEC)
      integer*4   icurhr(MXHOUR), idate, jdate, i, j, k
      integer*4   ihr, nseg
      real*4      btime, etime
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data nseg /1/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- set species list as integers ---
c
      do j=1,nspecs
        read(spcnam(j),'(10A1)') (ispecs(i,j),i=1,10)
      enddo
c
c   ---- do both hours ----
c
      do 10 ihr = 1,MXHOUR
c
c   ---- get date and time ---
c
         idate = icurdt(ihr) / 100
         jdate = idate
         icurhr(ihr) = MOD( icurdt(ihr), 100)
         btime = FLOAT(icurhr(ihr))
         etime = btime + 1
         if( icurhr(ihr) .GE. 23 ) then
            jdate = jdate + 1
            etime = 0.
         endif
c
c   --- if date is not in epsiode, skip write ---
c
         if( jdate .EQ. iendat .AND. etime .GT. endtim ) goto 10
c
c   --- write the emissions ---
c
         write(IOWEMS,ERR=7000) idate, btime, jdate, etime
         do k=1,nspecs
            write(IOWEMS,ERR=7000) nseg,(ispecs(i,k),i=1,10),
     &                     ((emsgrd(ihr,k,j,i),i=1,nxcell),j=1,nycell)
         enddo
c
c   --- do next hour ---
c
   10 continue
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
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oemsfl(:strmin(oemsfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statement:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
