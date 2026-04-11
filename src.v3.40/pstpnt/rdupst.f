C**** RDUPST.F 
c
      subroutine rdupst( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /PSTPNT/
c    packet in common variables to be used by the PSTPNT program.
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
      include 'cntylst.inc'
      include 'uamregn.inc'
      include 'iopstpnt.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    Subroutines Called:
c-----------------------------------------------------------------------
c
c   rddate  --  reads the /EPISODE/ packet (library)
c   rdcnty  --  reads the /COUNTY/ packet (library)
c   rdregn  --  reads the /UAMREGN/ packet (library)
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
      character*(KEYLEN) module
      character*40       cdate
      integer*4          jerr
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data module /'PSTPNT'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- call routine to read control packet ---
c
      call rddate( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read species list packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read region packet ---
c
      call rdregn( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read county list ---
c
      call rdcnty( jerr,
     &                   IORINP )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to get the output tables to generate ---
c
      call rdotbl( jerr, 
     &                   IORINP, module, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- echo the data to message file ---
c
      call getime( cdate )
      write(IOWMSG,9000,ERR=7000) VERSON, cdate
      write(IOWMSG,9001,ERR=7000) 'File note',fnote(:strmin(fnote))
      write(IOWMSG,9003,ERR=7000) 'Episode date (Calander)',ibgcal
      write(IOWMSG,9002,ERR=7000) 'Episode date (Julian)',ibgdat
      write(IOWMSG,9002,ERR=7000) 'Beginning hour',INT(begtim/100.)
      write(IOWMSG,9002,ERR=7000) 'Ending hour',INT(endtim/100.)
      write(IOWMSG,9004,ERR=7000) 'Grid origin (km)',xorig,yorig
      write(IOWMSG,9002,ERR=7000) 'UTM zone',izone
      write(IOWMSG,9004,ERR=7000) 'Grid cell width (km)',dxcell,dycell
      write(IOWMSG,9005,ERR=7000) 'Number of cells',nxcell,nycell
      write(IOWMSG,9002,ERR=7000) 'Number of counties',ncdfip
c
c  --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
 9002 format(T10,A,T40,:,':',I5,I5)
 9003 format(T10,A,T40,:,':',I6)
 9004 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9005 format(T10,A,T40,:,': (',I5,',',I5,')')
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
