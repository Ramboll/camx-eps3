C**** NCF_RDEMIS.F 
c
      subroutine ncf_rdemis (ierr,
     &                            ifile, iounit, ihour )
c
c
c-----------------------------------------------------------------------
c
c    Reads the currents hour's emissions from NetCDF file attached to file
c    unit "iounit".  The emissions are placed in the array in the order
c    of the ewquested species.
c
c-----------------------------------------------------------------------
c
c    Argument Declaration.
c     Outputs:
c       ierr   I  error code
c     Inputs:
c       ifile  I  file identifier
c       iounit I  unit number for reading file
c       ihour  I  hour (1-24)
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
      include 'uamcntl.inc'
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'ncf_iodat.inc'
      include 'iomrguam.inc'
      include 'mrguemis.inc'
      include 'usrmrguam.inc'
      include 'netcdf.inc'
c
c-----------------------------------------------------------------------
c  Argument Declaration
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 ifile
      integer*4 iounit
      integer*4 ihour
c
c-----------------------------------------------------------------------
c  External functions:
c-----------------------------------------------------------------------
c
      integer*4 ncf_get_tstep
      integer*4 strmin            
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*10 this_var
      integer*4    this_date, this_tstep, data_start(4), data_count(4)
      integer*4    this_varid, ispc, jerr, idxpol, ispdex, i, j
      real*4       this_time, tonsin, tonsadj
c
      real*4, allocatable, dimension(:,:) :: emisin
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure
c
      ierr = IFAIL
      allocate( emisin(nxcell,nycell) )
c
c  ---- get the index to this timestep ---
c
      this_date = ncf_tflag(1,ihour)
      this_time = REAL( ncf_tflag(2,ihour) )
      this_tstep = ncf_get_tstep(iounit,this_date,this_time)
      if( this_tstep .LE. 0 ) then
         ierr = ISUCES
         goto 9999
      endif
c
c   --- set the indexes for what to read ---
c
      data_start(1) = 1
      data_count(1) = nxcell
      data_start(2) = 1
      data_count(2) = nycell
      data_start(3) = 1
      data_count(3) = 1
      data_start(4) = this_tstep
      data_count(4) = 1
c
c   --- loop over species to be written to output ---
c
      do ispc=1,iscnt(ifile)
c
c   --- get data for this variable ---
c
          this_var = table(ifile,ispc)
          jerr = nf_inq_varid(iounit, this_var, this_varid)
          if( jerr .NE. NF_NOERR) cycle
          jerr = nf_get_vara_real(iounit,this_varid,data_start,
     &                                               data_count,emisin)
          if( jerr .NE. NF_NOERR) goto 7000
c
c  --- set the indexes ---
c
          ispdex = idxspc(ifile, ispc)
          if( ispdex .LE. 0 ) cycle
          idxpol = ispcrt(ispdex)
          do j=1,nycell
            do i=1,nxcell
              tonsin = emisin(i,j) * molwt(ispdex) * CVTTON
              emisout(i,j,ispdex) = emisout(i,j,ispdex) + emisin(i,j) * 
     &                                              adjfac(ifile,ispdex)
              tonsadj = tonsin * adjfac(ifile,ispdex)
c
c   --- add totals to grand totals of emissions processed ---
c
              totems(IDXIN,idxpol) = totems(IDXIN,idxpol) + DBLE(tonsin)
              totspc(IDXIN,ispdex) = totspc(IDXIN,ispdex) + DBLE(tonsin)
              totems(IDXOUT,idxpol) = totems(IDXOUT,idxpol) 
     &                                                + DBLE(tonsadj)
              totspc(IDXOUT,ispdex) = totspc(IDXOUT,ispdex) 
     &                                                + DBLE(tonsadj)
c
c   --- add totals to totals by file ---
c
              crtiasc(ifile,idxpol) = crtiasc(ifile,idxpol) + tonsin
              spciasc(ifile,ispdex) = spciasc(ifile,ispdex) + tonsin
              crtoasc(ifile,idxpol) = crtoasc(ifile,idxpol) + tonsadj
              spcoasc(ifile,ispdex) = spcoasc(ifile,ispdex) + tonsadj
c
c   --- add totals to totals by hour ----
c
              crtihrs(ihour,idxpol) = crtihrs(ihour,idxpol) + tonsin
              spcihrs(ihour,ispdex) = spcihrs(ihour,ispdex) + tonsin
              crtohrs(ihour,idxpol) = crtohrs(ihour,idxpol) + tonsadj
              spcohrs(ihour,ispdex) = spcohrs(ihour,ispdex) + tonsadj
c
c   --- add totals to totals by hour and file ----
c
              crtihcd(ifile,ihour,idxpol) =
     &                            crtihcd(ifile,ihour,idxpol) + tonsin
              spcihcd(ifile,ihour,ispdex) =
     &                            spcihcd(ifile,ihour,ispdex) + tonsin
              crtohcd(ifile,ihour,idxpol) =
     &                            crtohcd(ifile,ihour,idxpol) + tonsadj
              spcohcd(ifile,ihour,ispdex) =
     &                            spcohcd(ifile,ihour,ispdex) + tonsadj
c
            enddo
          enddo
c
c  --- end of parallelized loop ---
c
      enddo
      
c
c  --- normal execution, set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c  --- endo of file reached , set error code and return
c
  111 continue
      deallocate( emisin )
      ierr = IEOF
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
      write(ioerr,'(A,I5)',ERR=9999) 'Reading hour: ',ihour
      write(ioerr,'(A,I5,//)') 'NetCDF error code: ',ierr
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
