C***** NCF_WRT_DATA_HOURLY.F 
c
      subroutine ncf_wrt_data_hourly( ierr,
     &                                      icurdt )
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
      include 'ncf_iodat.inc'
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
      character*200 action
      character*4   ispecs(10,MXSPEC)
      integer*4     icurhr(MXHOUR), idate, jdate, i, j, k
      integer*4     ihr, nseg, jerr
      real*4        btime, etime
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
c  --- write the data for this hour ---
c
         ncf_cur_tstep = ihr
         action = 'Writing timestamp to output NetCDF file.'
         call ncf_wrt_data_tstep(jerr,action,ncf_iowemis,nspecs)
         if( jerr .NE. ISUCES ) goto 9999
c
         action = 'Writing hourly data to output NetCDF file.'
         call ncf_wrt_data_species(jerr,action,ncf_iowemis,ihr,
     &                              nxcell,nycell,1,nspecs,spcnam)
         if( jerr .NE. ISUCES ) goto 9999
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
