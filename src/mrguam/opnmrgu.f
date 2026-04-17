**** OPBMRGU.F 
c
      subroutine opnmrgu( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the MRGUAM program
c    and opens all files.  Because we get control factors from 
c    the job script, this routine must know the species list.
c    This routine will open the USERIN file and call the routine
c    to read it, before reading the other filenames.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
      integer*4 fndchr
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   iounit  I   for unit numbers
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR)  fname, action, string
      character*(KEYLEN) keywrd, keyfil
      character*40       cdate
      character*10       name
      integer*4          iounit, jerr, idxem, i, j
      real*4             factor
      logical*4          lcheck

c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume failure ---
c
      ierr = IFAIL
      ioerr = IOWSTD
      iflcnt = 0
c
c  --- initialize the factors array ---
c
      do i=1,MXMRG
        do j=1,MXSPEC
           adjfac(i,j) = 1.0
        enddo
      enddo
c
c   --- read USERIN filename from standard input ---
c
      action = 'Reading USERIN filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) uinpfl
      call lftjst( uinpfl )
      write(IOWSTD,'(2A)') 'Read: ',uinpfl(:strmin(uinpfl))
c
c   --- check that the file exists and open it ---
c
      action = 'Reading USERIN filename.'
      fname = uinpfl
      inquire(file=fname,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
      action = 'Opening USERIN file.'
      fname = uinpfl
      open(unit=IORINP,file=uinpfl,status='OLD',ERR=7003)
c
c   --- call the routine to read the USERIN file ---
c
      call rdmrgu(jerr)
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- read all other filenames from standard input ---
c
      write(IOWSTD,'(/,A,I5,A)',ERR=9999) 'MRGUAM will merge up to ',
     &                                                 MXMRG,' files'
      write(IOWSTD,'(A)',ERR=9999)
     &               'Enter each file name, then enter /END/ to end.'
      write(IOWSTD,'(2A)',ERR=9999) 'To apply controls to this file, ',
     &             'enter the species name and the control factor.'
  111 continue
      write(action,'(A,I5)') 
     &            'Reading input emissions filename for file:',iflcnt+1
      write(IOWSTD,'(A,I5,A)') 'Enter file ',iflcnt+1,' :'
      read(IORSTD,'(A20,A)',ERR=7000,END=7001) keyfil, string
c
c  --- check if all files have been supplied ---
c
      keywrd = string(:KEYLEN)
      call lftjst( keywrd )
      call low2up( keywrd )
      if( keywrd .EQ. KEYEND ) goto 222
c
c  --- if this is new file add it to the list ---
c
      call lftjst( keyfil )
      call low2up( keyfil )
      if( keyfil(1:4) .EQ. 'FILE' ) then
         iflcnt = iflcnt + 1
         if( iflcnt .GT. MXMRG ) goto 7004
         call lftjst( string )
         innm(iflcnt) = string
         write(IOWSTD,'(2A)') 'Read: ',
     &                      innm(iflcnt)(:strmin(innm(iflcnt)))
         goto 111
c
c  --- otherwise, this is a species cotrol record ---
c
      else
         write(action,'(A,I5)') 
     &                 'Reading control factors for file:',iflcnt
         read(string,'(A10,F10.0)',ERR=7005) name, factor
         idxem = fndchr( name, 10, spcnam, nspecs )
         if( idxem .LE. 0 ) goto 7006
         adjfac(iflcnt,idxem) = factor
      endif
c
c   --- get the next line ----
c
      goto 111
c
c   --- check that number of files is valid ---
c
  222 continue
      if( iflcnt .LT. 1 ) goto 7007
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      write(IOWSTD,'(2A)') 'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output emissions filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter output UAM filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ouamfl
      write(IOWSTD,'(2A)') 'Read: ',ouamfl(:strmin(ouamfl))
c
c   --- check for existence of input files ---
c
      do i=1,iflcnt
          fname = innm(i)
          write(action,'(A,I5)') 
     &            'Reading input emissions filename for file:',iflcnt
          inquire(file=fname,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      enddo
c
c   --- check for output files that already exist ---
c
      action = 'Reading output message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7008
c
      action = 'Reading output emissions filename.'
      fname = ouamfl
      inquire(file=ouamfl,exist=lcheck)
      if( lcheck ) goto 7008

c   --- open files ---
c
      action = 'Opening output message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      action = 'Opening output emissions file.'
      fname = ouamfl
      open(unit=IOWUAM,file=ouamfl,form='UNFORMATTED',
     &                                       status='UNKNOWN',ERR=7003)
c
c   --- generate unit numbers ---
c
      iounit=MXUNIT
      do i=1,iflcnt
        iounit=iounit+1
        fname = innm(i)
        write(action,'(A,I5)') 
     &            'Opening input emissions filename for file:',iflcnt
        open(unit=iounit,file=innm(i),form='UNFORMATTED',
     &                                       status='UNKNOWN',ERR=7003)
      enddo
c
c   --- echo program version and filenames to message file ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7009) VERSON, cdate
      write(IOWMSG,9000,ERR=7009) '    Input Files'
c
      write(IOWMSG,9000,ERR=7009) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      write(IOWMSG,'(/,A)',ERR=7009) 'Input UAM files'
      do i=1,iflcnt
          write(IOWMSG,9001,ERR=7009) i, innm(i)(:strmin(innm(i)))
          do j=1,MXSPEC
             if( adjfac(i,j) .NE. 1.0 ) write(IOWMSG,9002,ERR=7009) 
     &                          'Controlling ',spcnam(j),':',adjfac(i,j)
          enddo
      enddo
c
      write(IOWMSG,9000,ERR=7009)
      write(IOWMSG,9000,ERR=7009) '    Output Files'
      write(IOWMSG,9000,ERR=7009) 'Output UAM file',
     &                                       ouamfl(:strmin(ouamfl))
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999) 'Not enough filenames supplied.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,2A)',ERR=9999)
     &            'ERROR: Input file not found: ',fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7003 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7004 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) 'Maximum number of input ',
     &                                 'emissions files exceeded.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXMRG
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXMRG ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,/,A)',ERR=9999) 'Line read: ',
     &                                        string(:strmin(string))
      write(ioerr,'(2A,//)',ERR=9999) 'Make sure the first word on ',
     &                   'the lines with filenames is the word "File".'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(2A)',ERR=9999) 'Species listed in job script ',
     &                              'is not in the /MRGUAM/ packet.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Species listed: ',name
      write(ioerr,'(2A,//)',ERR=9999) 'Either remove the species from ',
     &              'the job script or add it to the /MRGUAM/ packet.'
      goto 9999
c
 7007 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR reading filenames.'
      write(ioerr,'(A,//)',ERR=9999) 'No input emissions filenames ',
     &                                               'were supplied.'
      write(ioerr,'(A,//)',ERR=9999) 'Please put filenames in the ',
     &                            'in the job script and try again.'
      goto 9999
c
 7008 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Output file exists: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999)
     &                          'Please remove the file and try again.'
      goto 9999
c
 7009 continue
      ioerr = IOWSTD
      write(IOWSTD,'(/,A)',ERR=9999) 'ERROR: Writing to message file.'
      write(IOWSTD,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(T10,A,T40,:,':',A)
 9001 format(1X,I4,1X,A)
 9002 format(10X,3A,F8.5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
