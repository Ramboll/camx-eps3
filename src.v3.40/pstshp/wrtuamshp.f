C***** WRTUAMSHP.F 
c
      subroutine wrtuamshp( ierr,
     &                         icurdt )
c
c-----------------------------------------------------------------------
c
c    Writes the hourly gridded surface emissions to UAM file
c
c    Argument Declaration.
c     Outputs:
c       ierr     I  error flag
c     Inputs:
c       icurhr   I  array of current date and hour
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
      include 'msgtot.inc'
      include 'layers.inc'
      include 'iopstshp.inc'
      include 'pstshptot.inc'
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
      integer*4   icurhr(MXHOUR), idate, jdate, ispc, i, j
      integer*4   ihr, nseg
      real*4      emisstmp(MXCELX,MXCELY)
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
c  --- initialize error code ---
c
      ierr = IFAIL
c
c  --- set species list as integers ---
c
      do j=1,nspecs
        read(spcnam(j),'(10A1)') (ispecs(i,j),i=1,10)
      enddo
c
c  --- do all hours ----
c
      do 10 ihr = 1,MXHOUR
c
c  --- get date and time ---
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
c  --- if date is not in epsiode, skip write ---
c
         if( jdate .EQ. iendat .AND. etime .GT. endtim ) goto 10
c
c  --- write the emissions ---
c
         write(IOWSRF,ERR=7000) idate, btime, jdate, etime
         do ispc=1,nspecs
            emisstmp = 0.
            do i=1,nxcell
               do j=1,nycell
                  if( ijcell3d(i,j,1) .GT. 0 ) then
                      emisstmp(i,j) = emiss3d(ihr,ispc,ijcell3d(i,j,1))
                  endif
               enddo
            enddo
            write(IOWSRF,ERR=7000) nseg,(ispecs(i,ispc),i=1,10),
     &                      ((emisstmp(i,j),i=1,nxcell),j=1,nycell)
         enddo
c
c  --- do next hour ---
c
   10 continue
c
c  --- set error flag to sucess ---
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
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',osrffl(:strmin(osrffl))
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
