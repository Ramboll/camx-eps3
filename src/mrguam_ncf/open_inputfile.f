c**** OPEN_INPUTFILE
c
      subroutine open_inputfile(ierr,fname,iounit,lnetdf,ifile)
      implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c      Checks a file for NetCDF or binary and opens it accordinly.
c
c     Copyright 1996 - 2016
c     Ramboll Environ
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'   
      include 'iomrguam.inc'   
      include 'netcdf.inc'   
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      integer       ierr
      character*200 fname
      integer       iounit
      logical       lnetdf
      integer       ifile
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer ncf_chkfile
      integer strmin
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 action
      character*10  cname
      integer       jerr
      logical       lexist
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ierr = IFAIL
c
      action = 'Opening input file: '//fname(:strmin(fname))
      cname = 'EMISSIONS '
      inquire(file=fname,exist=lexist)
      if( .NOT. lexist ) goto 7000
      jerr = ncf_chkfile(iounit,fname,action,cname)
      if( jerr .EQ. NF_NOERR ) then
          iounit = MXUNIT + ifile
          lnetdf = .FALSE.
          open(unit=iounit,file=fname,
     &                    ERR=7001,form='UNFORMATTED',status='UNKNOWN')
      else
          lnetdf = .TRUE.
          jerr = nf_open(fname,NF_NOWRITE,iounit)
          if( jerr .NE. NF_NOERR ) goto 7001
      endif
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(//,a)') 'ERROR in OPEN_INPUTFILE: '
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A,//)') 'Input file does not exist: ',
     &                                   fname(:strmin(fname))
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(*,'(//,a)') 'ERROR: '
      write(*,'(A)') action(:strmin(action))
      write(*,'(2A)') 'Could not open file: ',
     &                                   fname(:strmin(fname))
      write(*,'(A,I5,//)') 'NetCDF error code: ',ierr
      ierr = IFAIL
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
