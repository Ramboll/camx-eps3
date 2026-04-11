C**** OPNEVT.F 
c
      subroutine opnevt( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the filenames to be used in the EMSCVT program and opens all
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
c   Apr 2014   Modified to support SPCEMS input formats - GSPRO and
c              GSCNv formatted files
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'ioemscvt.inc'
c
c-----------------------------------------------------------------------
c    Argument Declerations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strlen  I   returns the actual length of a string
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strlen
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fname   C   character string for temporary storage of filenames
c   ilen    C   variable to store length of filename
c   lcheck  L   logical to check for existence of files
c
      character*(MXSTR) fname, action
      character*40      cdate
      logical           lcheck
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      ioerr = IOWSTD
c
c   --- assume using a user input file ---
c
      lxref = .TRUE.
      lsplit = .TRUE.
      lctlfl = .TRUE.
      lgspfl = .TRUE.
c
c   --- read all filenames from standard input ---
c
      action = 'Reading user input filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter user input filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) uinfl
      call lftjst( uinfl )
      write(IOWSTD,'(2A)'),'Read: ',uinfl(:strmin(uinfl))
c
      action = 'Reading weight profile filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter weight profile filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) prwtfl
      call lftjst( prwtfl )
      write(IOWSTD,'(2A)'),'Read: ',prwtfl(:strmin(prwtfl))
c
      action = 'Reading chemical compound database filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &               'Enter chemical compound database filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) cdbsfl
      call lftjst( cdbsfl )
      write(IOWSTD,'(2A)'),'Read: ',cdbsfl(:strmin(cdbsfl))
c
      action = 'Reading input cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                     'Enter input cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) inxrfl
      call lftjst( inxrfl )
      if( strlen(inxrfl) .EQ. 0 ) then
         lxref = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',inxrfl(:strmin(inxrfl))
      endif
c
      action = 'Reading input split factors filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                     'Enter input split factors filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) isplfl
      call lftjst( isplfl )
      if( strlen(isplfl) .EQ. 0 ) then
         lsplit = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',isplfl(:strmin(isplfl))
      endif
c
      action = 'Reading input control factors filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                     'Enter input control factors filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ctlfl
      call lftjst( ctlfl )
      if( strlen(ctlfl) .EQ. 0 ) then
         lctlfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',ctlfl(:strmin(ctlfl))
      endif
c
      action = 'Reading output message filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 'Enter message filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) msgfl
      call lftjst( msgfl )
      write(IOWSTD,'(2A)'),'Read: ',msgfl(:strmin(msgfl))
c
      action = 'Reading output cross reference filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &                 'Enter output cross reference filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) oxrfl
      call lftjst( oxrfl )
      if( strlen(oxrfl) .EQ. 0 ) then
         loxrfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',oxrfl(:strmin(oxrfl))
      endif
c
      action = 'Reading output split factors filename.'
      write(IOWSTD,'(/,A)',ERR=9999)
     &                      'Enter output split factors filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) osplfl
      call lftjst( osplfl )
      write(IOWSTD,'(2A)'),'Read: ',osplfl(:strmin(osplfl))
c
      action = 'Reading output GSPRO formatted profiles filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &           'Enter output GSPRO formatted profiles filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ogspfl
      call lftjst( ogspfl )
      if( strlen(ogspfl) .EQ. 0 ) then
         lgspfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',ogspfl(:strmin(ogspfl))
      endif
c
      action = 'Reading output GSCNV formatted VOC-to-TOG filename.'
      write(IOWSTD,'(/,A)',ERR=9999) 
     &           'Enter output GSCNV formatted VOC-to-TOG filename:'
      read(IORSTD,'(20X,A)',ERR=7000,END=7001) ogscfl
      call lftjst( ogscfl )
      if( strlen(ogscfl) .EQ. 0 ) then
         lgspfl = .FALSE.
         write(IOWSTD,'(A)',ERR=9999) 'No file supplied.'
      else
         write(IOWSTD,'(2A)'),'Read: ',ogscfl(:strmin(ogscfl))
      endif
c
c   --- check for existence of input files ---
c
      action = 'Reading user input filename.'
      fname = uinfl
      inquire(file=uinfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
      action = 'Reading weight profile filename.'
      fname = prwtfl
      inquire(file=prwtfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
      action = 'Reading chemical compound database filename.'
      fname = cdbsfl
      inquire(file=cdbsfl,exist=lcheck)
      if( .NOT. lcheck ) goto 7002
c
      if( lxref ) then
          action = 'Reading input cross reference filename.'
          fname = inxrfl
          inquire(file=inxrfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
      if( lsplit ) then
          action = 'Reading input split factors filename.'
          fname = isplfl
          inquire(file=isplfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
      if( lctlfl ) then
          action = 'Reading input split factors filename.'
          fname = ctlfl
          inquire(file=ctlfl,exist=lcheck)
          if( .NOT. lcheck ) goto 7002
      endif
c
c   --- check for output files that already exist ---
c
      action = 'Reading output message filename.'
      fname = msgfl
      inquire(file=msgfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output cross reference filename.'
      fname = oxrfl
      inquire(file=oxrfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output split factors filename.'
      fname = osplfl
      inquire(file=osplfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output GSPRO split factors filename.'
      fname = ogspfl
      inquire(file=ogspfl,exist=lcheck)
      if( lcheck ) goto 7003
c
      action = 'Reading output GSCNV VOC-to-TOG filename.'
      fname = ogscfl
      inquire(file=ogscfl,exist=lcheck)
      if( lcheck ) goto 7003
c
c   --- open files ---
c
      action = 'Opening user input file.'
      fname = uinfl
      open(unit=IORUIN,file=uinfl,ERR=7004)
c
      action = 'Opening weight profile file.'
      fname = prwtfl
      open(unit=IORPRW,file=prwtfl,ERR=7004)
c
      action = 'Opening chemical compound database file.'
      fname = cdbsfl
      open(unit=IORCDB,file=cdbsfl,ERR=7004)
c
      if( lxref ) then
         action = 'Opening input cross reference file.'
         fname = inxrfl
         open(unit=IORXRF,file=inxrfl,ERR=7004)
      endif
c
      if( lsplit ) then
         action = 'Opening input split factors file.'
         fname = isplfl
         open(unit=IORSPL,file=isplfl,ERR=7004)
      endif
c
      if( lctlfl ) then
         action = 'Opening input control factors file.'
         fname = ctlfl
         open(unit=IORCTL,file=ctlfl,ERR=7004)
      endif
c
      action = 'Opening output message file.'
      fname = msgfl
      open(unit=IOWMSG,file=msgfl,ERR=7004)
c
      if( loxrfl ) then
         action = 'Opening output cross reference file.'
         fname = oxrfl
         open(unit=IOWXRF,file=oxrfl,ERR=7004)
      endif
c
      action = 'Opening output split factors file.'
      fname = osplfl
      open(unit=IOWSPL,file=osplfl,ERR=7004)
c
      if( lgspfl ) then
         action = 'Opening output GSPRO formatted file.'
         fname = ogspfl
         open(unit=IOWPRO,file=ogspfl,ERR=7004)
      endif
c
      if( lgspfl ) then
         action = 'Opening output GSCNV formatted file.'
         fname = ogscfl
         open(unit=IOWCNV,file=ogscfl,ERR=7004)
      endif
c
c   --- echo filenames ---
c
      ioerr = IOWMSG
      call getime( cdate )
      write(IOWMSG,'(/,T20,A,A40,/)',ERR=7005) VERSON, cdate
      write(IOWMSG,9000,ERR=7005) VERSON, cdate
      write(IOWMSG,9001,ERR=7005) '    Input Files'
c
      write(IOWMSG,9001,ERR=7005) 'User input file',
     &                                        uinfl(:strmin(uinfl))
      write(IOWMSG,9001,ERR=7005) 'Weight profiles file',
     &                                        prwtfl(:strmin(prwtfl))
      write(IOWMSG,9001,ERR=7005) 'Compound data base',
     &                                        cdbsfl(:strmin(cdbsfl))
      if( lxref ) then
         write(IOWMSG,9001,ERR=7005) 'Input xref file',
     &                                        inxrfl(:strmin(inxrfl))
      else
         write(IOWMSG,9001,ERR=7005) 'No input xref file.'
      endif
c
      if( lsplit ) then
         write(IOWMSG,9001,ERR=7005)'Splitting factors file',
     &                                        isplfl(:strmin(isplfl))
      else
         write(IOWMSG,9001,ERR=7005) 'No splitting factors file.'
      endif
c
      if( lctlfl ) then
         write(IOWMSG,9001,ERR=7005)'Control factors file',
     &                                        ctlfl(:strmin(ctlfl))
      else
         write(IOWMSG,9001,ERR=7005) 'No control factors file.'
      endif
c
      write(IOWMSG,'(A)',ERR=7005)
      write(IOWMSG,9001,ERR=7005) '    Output Files'
      write(IOWMSG,9001,ERR=7005) 'Output xref file',
     &                                            oxrfl(:strmin(oxrfl))
c
      write(IOWMSG,9001,ERR=7005) 'Output splitting factors',
     &                                          osplfl(:strmin(osplfl))
c
      if ( lgspfl ) then
         write(IOWMSG,9001,ERR=7005) 'Output GSPRO split factors',
     &                                          ogspfl(:strmin(ogspfl))
         write(IOWMSG,9001,ERR=7005) 'Output GSCNV VOC-to-TOG factors',
     &                                          ogscfl(:strmin(ogscfl))
      endif
      write(IOWMSG,'(A)',ERR=7005)
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
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Output file exists: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A)',ERR=9999) action(:strmin(action))
      write(ioerr,'(A,//)',ERR=9999)
     &                          'Please remove the file and try again.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           fname(:strmin(fname))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7005 continue
      ioerr = IOWSTD
      write(IOWSTD,'(/,A)',ERR=9999) 'ERROR: Writing to message file.'
      write(IOWSTD,'(2A)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
