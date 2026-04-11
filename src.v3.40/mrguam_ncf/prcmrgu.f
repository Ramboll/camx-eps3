C**** PRCMRGU.F 
c
      subroutine prcmrgu( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine,for each hour the hourly emissions from
c    each input file is read and adjusted if necessary.  The total
c    merged is then calculated and written to the new file in the
c    requested species order.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I  error flag
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
      include 'uamcntl.inc'
      include 'msgtot.inc'
      include 'iomrguam.inc'
      include 'mrguemis.inc'
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
      integer*4 ifile, iounit, ihour, ispc, i, j, jerr
      integer*4 icurdt(MXHOUR), nhours, ibegdt, ibeghr, iendhr, ienddt
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure
c
      ierr = IFAIL
c
c  --- initialize arrays with dates and hours for this epsiode ----
c
      nhours = 0
 111  continue
      nhours = nhours + 1
      ibeghr = INT( begtim / 100 ) + nhours - 1
      iendhr = INT( begtim / 100 ) + nhours
      ibegdt = ibgdat * 100 + ibeghr
      ienddt = ibgdat * 100 + ibeghr
      if( ibeghr .GT. 24 ) then
          ibeghr = ABS(24 - ibeghr)
          ibegdt = (ibgdat+1) * 100 + ibeghr
      endif
      if( iendhr .GT. 24 ) then
          iendhr = ABS(24 - iendhr)
          ienddt = (ibgdat+1) * 100 + iendhr
      endif
      if( ( INT(ienddt/100.) .LT. iendat) .OR.
     &          (INT( ienddt/100.)  .EQ. iendat .AND.
     &                     iendhr .LE. INT( endtim/ 100 ) ) ) then
        if( nhours .GT. MXHOUR ) goto 7001
        icurdt(nhours) = ibegdt
        goto 111
      endif
      nhours = nhours - 1
c
c   --- loop over hours ---
c
      do ihour=1,nhours
c
c      --- initialize global array ---
c
          do ispc=1,nspecs
              do i=1,MXCELX
                  do j=1,MXCELY
                      emisout(i,j,ispc) = 0.0
                  enddo
              enddo
          enddo
c
c   ---- process all files ---
c
          nmsgasc = iflcnt 
          do 10 ifile=1,iflcnt
c
c   --- set the source category code to the file number ---
c
              if( iflcnt .LT. 100 ) then
                 write(msgasc(ifile),'(A,I2.2)') 'File ',ifile
              else
                 write(msgasc(ifile),'(A,I5)') 'File ',ifile
              endif
c
              iounit = ncf_mrgemis(ifile)
              if( is_netcdf(ifile) ) then
                   call ncf_rdemis(jerr,ifile,iounit,ihour)
                   if( jerr .NE. ISUCES ) goto 7000
              else
                   call rdemis(jerr,ifile,iounit,ihour)
                   if( jerr .EQ. IEOF ) goto 10
                   if( jerr .NE. ISUCES ) goto 7000
              endif
   10     continue
c
c ---- write out all of the hours ---
c
          call ncf_wrt_data_hourly(jerr,icurdt,ihour)
      enddo
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading hourly data of ',
     &                                        'input emissions file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                              innm(ifile)(:strmin(innm(ifile)))
      write(ioerr,'(A,I5,//)',ERR=9999) 'Reading hour: ',ihour
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: EPS cannot process an ',
     &                      'inventory of longer than a 24 hour period.'
      write(ioerr,'(2A,//)',ERR=9999) 'Please update the USERIN ',
     &                                'to reflect a single day epsiode.'
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
