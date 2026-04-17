C**** RDEMIS.F 
c
      subroutine rdemis (ierr,
     &                         ifile, iounit, ihour )
c
c
c-----------------------------------------------------------------------
c
c    Reads the currents hour's emissions from UAM file attached to file
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
      include 'msgtot.inc'
      include 'embrdat.inc'
      include 'iomrguam.inc'
      include 'mrguemis.inc'
      include 'usrmrguam.inc'
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
      integer*4 strmin            
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   bt     R  beginning time
c   et     R  ending time
c   ibd    I  beginning date
c   ied    I  ending date
c   inttmp I  temporary integer for type conversions
c   ispc   I  species loop
c   ispdex I  species index
c   itmpnm I  species name in integer format
c   nseg   I  number of segments
c   string C  for holding species name
c   tempch C  single character
c
      character*10 code
      character*4  itmpnm(10)
      integer*4    ibd, ied, ispc, ispdex, inttmp, idxpol
      integer*4    jerr, nseg, i, j
      real*4       emisin(MXCELX,MXCELY), tonsin, tonsadj, bt, et
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure
c
      ierr = IFAIL
c
c   --- check for time record
c       et, converted to integer, should equal ihour
c       bt, converted to integer, should be 1 less ----
c
      read(iounit,END=111,ERR=7000) ibd, bt, ied, et
c
c   --- compare times ---
c
      inttmp = INT(et)
      if( inttmp .EQ. 0 .AND. ibd .LT. ied ) inttmp = 24
      if( inttmp .NE. ihour ) goto 7001
      inttmp = MIN(INT(bt)+1,24)
      if( inttmp .NE. ihour ) goto 7001
c
c   --- iscnt is number of species for each input file ---
c
      do 10 ispc=1,iscnt(ifile)
          read(iounit,ERR=7000) nseg, (itmpnm(i),i=1,10),
     &                         ((emisin(i,j),i=1,nxcell),j=1,nycell)
c
c   --- we know the species are in the same order
c       as the header records, so just take the next index ---
c
          ispdex = idxspc(ifile, ispc)
          if( ispdex .LE. 0 ) goto 10
          idxpol = ispcrt(ispdex)
c
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
   10 continue
      
c
c  --- normal execution, set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c  --- endo of file reached , set error code and return
c
  111 continue
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
      write(ioerr,'(A,I5,//)',ERR=9999) 'Reading hour: ',ihour
      goto 9999
c
 7001 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Reading hourly data of ',
     &                                        'input emissions file.'
      write(ioerr,'(2A)',ERR=9999) 'File: ',
     &                              innm(ifile)(:strmin(innm(ifile)))
      write(ioerr,'(A,I5)',ERR=9999) 'Reading hour: ',ihour
      write(ioerr,'(2A)',ERR=9999) 'Time is out of sync with episode.'
      write(ioerr,'(10X,A,I5)',ERR=9999) 'Beginning date read: ',ibd
      write(ioerr,'(10X,A,F7.2)',ERR=9999) 'Beginning time read: ',bt
      write(ioerr,'(10X,A,I5)',ERR=9999) 'Ending date read   : ',ied
      write(ioerr,'(10X,A,F7.2)',ERR=9999) 'Ending time read   : ',et
      write(ioerr,'(//)',ERR=9999)
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
