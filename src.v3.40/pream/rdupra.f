C**** RDUPRA.F 
c
      subroutine rdupra( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /PREAM/
c    packet in common variables to be used by the PREAM program.
c
c    Argument Declaration.
c     Outputs:
c       ierr  I error flag
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
      include 'uamregn.inc'
      include 'cntylst.inc'
      include 'iopream.inc'
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
c   strmin  I   returns the actual length of a string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   lngfip  C   long fip variable
c   respon  C   character string for temporary storage
c
      character*(KEYLEN)  keywrd
      character*40        cdate
      character*10        module
      integer*4           jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/PREAM/'/
      data module /'PREAM'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
c
c   --- call routine to read region packet ---
c
      call rdcrit( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region packet ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables requested ---
c
      call rdotbl( jerr, 
     &                   IORINP, module, .FALSE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9001,ERR=7001) VERSON, cdate
      write(IOWMSG,9002,ERR=7001) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9004,ERR=7001) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9003,ERR=7001) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9003,ERR=7001) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9003,ERR=7001) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9003,ERR=7001) 'Number of counties',ncdfip
c
c   --- call routine to find /PREAM/ packet
c       not finding it is not an error, since it is optional ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 111
c
c   ---- set error flag to success ---
c
  111 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      ierr = IFAIL
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,':',A)
 9003 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9004 format(T10,A,T40,:,':',I6)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
