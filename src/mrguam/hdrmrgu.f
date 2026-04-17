C**** HDRMRGU.F 
c
      subroutine hdrmrgu( ierr )
c
c-----------------------------------------------------------------------
c
c    Call the routines to read and check the headers for all of the
c    input files.
c    (assume the input files are already open.)
c    When finished, call the routine to write the new header.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I  error indicator
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
      include 'iomrguam.inc'
      include 'usrmrguam.inc'
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
c   jerr    I  error flag for subroutine calls
c   iounit  I  calculated for input files
c
      integer*4 jerr, iounit, i, j
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
c
      iounit=MXUNIT
      do i=1,iflcnt
          iounit=iounit+1
c
c  --- call routine to read header ---
c
          call rdehdr(jerr,iounit,i)
          if(jerr .NE. ISUCES) goto 9999
c
c  --- call routine to check header for correct data ---
c
          call ckhdr(jerr,i)
          if(jerr .NE. ISUCES) goto 9999
      enddo
c
c   --- echo the species list from each file ---
c
      write(IOWMSG,'(/,20X,A)',ERR=7000) 'Species List from Each File'
      write(IOWMSG,'(70A)',ERR=7000) ('-',i=1,70)
      write(IOWMSG,9000,ERR=7000) ' File',' Species  ','Merged',
     &                                             'Control Factor'
      do i=1,iflcnt
         write(IOWMSG,'(/,A)',ERR=7000) innm(i)(:strmin(innm(i)))
         do j=1,iscnt(i)
             if( idxspc(i,j) .GT. 0 ) then
                write(IOWMSG,9001,ERR=7000) table(i,j),'YES',
     &                                          adjfac(i,idxspc(i,j))
             else
                write(IOWMSG,9001,ERR=7000) table(i,j),'NO'
             endif
         enddo
      enddo
      write(IOWMSG,'(/,70A)',ERR=7000) ('-',i=1,70)
c
c  --- call routine to write the new header ---
c
      call wrthdr(jerr)
      if(jerr .NE. ISUCES) goto 9999
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
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A,22X,A,3X,A,3X,A)
 9001 format(30X,A,1X,A,8X,F8.4)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
