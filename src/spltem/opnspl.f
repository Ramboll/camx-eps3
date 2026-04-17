C**** OPNSPL.F
c
      subroutine opnspl( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the SPLTEM program and opens all
c    files
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c      08/07/97  --mmj--  original development
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iospltem.inc'
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
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string, min of 1
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname
      logical           lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- assume user input file specified ---
c
      lelev = .TRUE.
c
c   --- read all filenames from standard input ---
c
      write(IOWSTD,9000,ERR=9999) 'Enter USERIN filename:'
      read(IORSTD,8000,ERR=7000) uinpfl
      call lftjst(uinpfl)
c
      write(IOWSTD,9000,ERR=9999) 
     &                       'Enter compressed input EMBR filename:'
      read(IORSTD,8000,ERR=7000) infl
      call lftjst(infl)
c
      write(IOWSTD,9000,ERR=9999) 'Enter weight fractions filename:'
      read(IORSTD,8000,ERR=7000) prwtfl
      call lftjst(prwtfl)
c
      write(IOWSTD,9000,ERR=9999)
     &      'Enter ASC-SCC/profile code cross reference filename:'
      read(IORSTD,8000,ERR=7000) xrffl
      call lftjst(xrffl)
c
      write(IOWSTD,9000,ERR=9999)
     &                         'Enter compound database filename:'
      read(IORSTD,8000,ERR=7000) cdbfl
      call lftjst(cdbfl)
c
      write(IOWSTD,9000,ERR=9999) 'Enter message filename:'
      read(IORSTD,8000,ERR=7000) msgfl
      call lftjst(msgfl)
c
      write(IOWSTD,9000,ERR=9999) 
     &                     'Enter lowlevel output data filename:'
      read(IORSTD,8000,ERR=7000) outfl
      call lftjst(outfl)
c
      write(IOWSTD,9000,ERR=9999) 
     &                     'Enter elevated output data filename:'
      read(IORSTD,8000,ERR=7000) elefl
      call lftjst(elefl)
      if ( strlen( elefl ) .EQ. 0 )  lelev = .FALSE.
c
c   --- check for existence of input files ---
c
      fname = uinpfl
      inquire(file=uinpfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = infl
      inquire(file=infl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = prwtfl
      inquire(file=prwtfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = xrffl
      inquire(file=xrffl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
      fname = cdbfl
      inquire(file=cdbfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7001
c
c   --- check for output files that already exist ---
c
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      fname = outfl
      inquire(file=outfl,exist=lcheck)
      if( lcheck ) goto 7002
c
      if ( lelev )  then
         fname = elefl
         inquire(file=elefl,exist=lcheck)
         if( lcheck ) goto 7002
      endif
c
c   --- open files ---
c
      fname = uinpfl
      open(unit=IORINP,file=uinpfl,status='OLD',ERR=7003)
c
      fname = infl
      open(unit=IORMBR,file=infl,form='UNFORMATTED',
     &                                        status='OLD',ERR=7003)
c
      fname = prwtfl
      open(unit=IORPRW,file=prwtfl,status='OLD',ERR=7003)
c
      fname = xrffl
      open(unit=IORXRF,file=xrffl,status='OLD',ERR=7003)
c
      fname = cdbfl
      open(unit=IORCDB,file=cdbfl,status='OLD',ERR=7003)
c
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,status='UNKNOWN',ERR=7003)
c
      fname = outfl
      open(unit=IOWDAT,file=outfl,status='UNKNOWN',ERR=7003)
c
      if ( lelev )  then
         fname = elefl
         open(unit=IOWELE,file=elefl,status='UNKNOWN',ERR=7003)
      endif
c
c   --- echo program version and filenames to message file ---
c
      write(IOWMSG,9001,ERR=7004) VERSON
      write(IOWMSG,9002,ERR=7004) '    Input Files'
c
      write(IOWMSG,9002,ERR=7004) 'USERIN file',uinpfl(:strmin(uinpfl))
c
      write(IOWMSG,9002,ERR=7004) 
     &                'Input compressed EMBR file', infl(:strmin(infl))
c
      write(IOWMSG,9002,ERR=7004) 
     &                 'Weight fractions file', prwtfl(:strmin(prwtfl))
c
      write(IOWMSG,9002,ERR=7004)
     &          'ASC-SCC/profile code xref file',xrffl(:strmin(xrffl))
c
      write(IOWMSG,9002,ERR=7004)
     &          'Compound database file', cdbfl(1:strmin(cdbfl))
c
      write(IOWMSG,9002,ERR=7004)
      write(IOWMSG,9002,ERR=7004) '    Output Files'
      write(IOWMSG,9002,ERR=7004) 
     &                     'Output data file', outfl(:strmin(outfl))
c
      if ( lelev )  then
         write(IOWMSG,9002,ERR=7004) 
     &               'Output elevated data file',elefl(:strmin(elefl))
      else
         write(IOWMSG,9002,ERR=7004) 'Output elevated data file',
     &                               '<<  Not provided  >>'
      endif
      write(IOWMSG,9002,ERR=7004)
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR Reading filenames.'
      goto 9999
c
 7001 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Input file not found: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7002 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Output file exists: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      goto 9999
c
 7004 continue
      write(IOWSTD,9000,ERR=9999) 'ERROR: Writing to message file: ',
     &                                       fname(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 8000 format(20X,A)
 9000 format(/,1X,A,A,/)
 9001 format(/,T20,A,/)
 9002 format(T10,A,T40,:,':',A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
