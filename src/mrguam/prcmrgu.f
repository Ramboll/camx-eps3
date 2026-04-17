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
      integer*4 ifile, iounit, ihour, ispc, i, j, ibegti, iendti, jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure
c
      ierr = IFAIL
c
c   --- figure out hours, assume even hours ---
c
      ibegti = int(begtim/100) + 1
      iendti = int(endtim/100)
c
c    --- make sure the beginning hour is not 24 and the end hour
c        is not 0 ---
c
       if( ibegti .GE. 24) ibegti = abs(24 - ibegti) + 1
       if( iendti .EQ. 0) iendti = 24
c
c   --- loop over hours ---
c
      do ihour=ibegti,iendti
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
          iounit = MXUNIT
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
              iounit=iounit+1
              call rdemis(jerr,ifile,iounit,ihour)
              if( jerr .EQ. IEOF ) goto 10
              if( jerr .NE. ISUCES ) goto 7000
   10     continue
          call wrtout(jerr,ihour)
          if( jerr .NE. ISUCES ) goto 9999
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
