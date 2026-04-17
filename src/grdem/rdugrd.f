C**** RDUGRD.F 
c
      subroutine rdugrd( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file and stores the data in the /GRDEM/
c    packet in common variables to be used by the GRDEM program.
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
      include 'ncf_iodat.inc'
      include 'iogrdem.inc'
      include 'usrgrdem.inc'
      include 'grdemtot.inc'
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
c   fndkey  --  searches a file for a keyword (library)
c   lftjst  --  left justifies a character string (library)
c   low2up  --  converts a character string to upper case (library)
c   rddate  --  reads the /DATE/ packet (library)
c   rdcnty  --  reads the /COUNTY/ packet (library)
c   rdregn  --  reads the /UAMREGN/ packet (library)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c   strmin  I   returns the actual length of a string (min of 1)
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   jerr    I   error flag for subroutine calls
c   keywrd  C   temporary storage for seaching fo keyword
c   respon  C   character string for temporary storage
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, medspc
      character*(MXSTR)   respon, action
      character*40        cdate
      character*10        module
      integer*4           irec, imed, i, jerr
      real*4              factor
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data module  /'GRDEM'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      keywrd = '/GRDEM/'
c
c   --- call routine to read species packet ---
c
      call rdspec( jerr,
     &                   IORINP, .TRUE. )
      if( jerr .NE. ISUCES ) goto 9999
c
c   --- call routine to read date packet ---
c
      call rddate( jerr,
     &                   IORINP )
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
c   --- call routine to find /GRDEM/ packet ---
c
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7001
c
c   --- read the packet variables ---
c
      luam = .FALSE.
      lcomp = .FALSE.
      lmeds = .FALSE.
      lsrgmult = .FALSE.
      irec = irec + 1
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(21:MXSTR)
      call low2up( respon )
      call lftjst( respon )
      if( loutfile ) then
         if( respon(1:3) .EQ. YES ) then
             luam = .TRUE.
             write(IOWMSG,9001,ERR=7000) 'Output is UAM',YES
         else if( respon(1:3) .EQ. NO ) then
             luam = .FALSE.
             write(IOWMSG,9001,ERR=7000) 'Output is UAM',NO
         else if( respon(1:4) .EQ. 'MEDS' ) then
             lmeds = .TRUE.
             write(IOWMSG,9001,ERR=7000) 'Output is MEDS',YES
         else if( respon(1:4) .EQ. 'COMP' ) then
             lcomp = .TRUE.
             write(IOWMSG,9001,ERR=7000) 'Output is UAM',NO
         else
             goto 7004
         endif
      endif
c
      read(IORINP,'(A)',ERR=7002,END=7003) string
      respon = string(:20)
      call lftjst( respon ) 
      call low2up( respon )
      if( respon .NE. KEYEND .AND. lsurg ) then
         respon = string(21:MXSTR)
         call low2up( respon )
         call lftjst( respon )
         if( respon(1:3) .EQ. YES ) then
            lsrgmult = .TRUE.
         else if( respon(1:3) .EQ. NO ) then
            lsrgmult = .FALSE.
         else
            goto 7009
         endif
      endif
c
c   --- call routine to find /MEDS/ packet ---
c
      scen = ' ' 
      do i=1,MXSPEC
         imapmed(i) = 0
      enddo
      if( lmeds ) then
c
c --- close the output file and re-open formatted ---
c
          close(IOWEMS)
          open(unit=IOWEMS,file=oemsfl,form='FORMATTED',
     &                                      status='UNKNOWN',ERR=7005)
c
c --- read the packet data ---
c
          irec = 0
          keywrd = '/MEDS/'
          call fndkey( jerr,
     &                       IORINP, keywrd )
          if( jerr .EQ. IRDERR ) goto 9999
          if( jerr .NE. ISUCES ) goto 222
          irec = irec + 1
          read(IORINP,'(20X,A)',ERR=7006,END=7007) string
          scen = string(21:28)
          call lftjst( scen )
          imed = 0
  111     continue
          imed = imed + 1
          irec = irec + 1
          read(IORINP,'(20X,A)',ERR=7006,END=7007) string
          read(string,'(20X,A10,F10.0)',ERR=7008) medspc,factor
          call lftjst( medspc )
          call low2up( medspc )
          if( medspc .EQ. KEYEND ) goto 222
          do i=1,nspecs
              if( medspc(:10) .EQ. spcnam(i) ) then 
                imapmed(imed) = i
                facmed(imed) = factor
              endif
          enddo
          goto 111
      endif
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
      if( luam .AND. .NOT. lnetcdf_out ) then
          write(IOWMSG,9001,ERR=7000) 'Output file format','UAM'
      else if( luam .AND. lnetcdf_out ) then
          write(IOWMSG,9001,ERR=7000) 'Output file format','NetCDF'
      else if( .NOT. luam .AND. .NOT. lmeds .AND. .NOT. lcomp ) then
          write(IOWMSG,9001,ERR=7000) 'Output file format','EMBR'
      else if( .NOT. luam .AND. lmeds .AND. .NOT. lcomp ) then
          write(IOWMSG,9001,ERR=7000) 'Output file format','MEDS'
      else if( .NOT. luam .AND. .NOT. lmeds .AND. lcomp ) then
          write(IOWMSG,9001,ERR=7000) 'Output file format','Compressed EMBR'
      endif
      if( lsrgmult ) then
         write(IOWMSG,9001,ERR=7000) 
     &                    'Using secondary surrogate codes',YES
      else
         write(IOWMSG,9001,ERR=7000) 
     &                    'Using secondary surrogate codes',NO
      endif
c
c  --- set error flag to sucess ---
c
  222 continue
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
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'Reading the USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999)
     &                     'ERROR: This program requires the ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      ierr = IFAIL
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(2A)',ERR=9999)
     &                 'Invalid response to output format flag: ',
     &                                         string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      write(ioerr,'(10X,A,//)',ERR=9999) 'MEDS'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Opening file: ',
     &                                           oemsfl(:strmin(oemsfl))
      write(ioerr,'(A,//)',ERR=9999) action(:strmin(action))
      goto 9999
c
 7006 continue
      keywrd = '/MEDS/'
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7007 continue
      keywrd = '/MEDS/'
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
c
 7008 continue
      keywrd = '/MEDS/'
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A)',ERR=9999) 'Line read:'
      write(ioerr,'(A,//)',ERR=9999) string(:strmin(string))
      goto 9999
c
 7009 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(3A)',ERR=9999) 'Invalid response to flag for ',
     &    'using secondary surrogate codes: ',string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
 7010 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &      keywrd(:strmin(keywrd)),' packet of USERIN file at line.'
      write(ioerr,'(3A)',ERR=9999) 'Invalid response to flag for ',
     &    'using NetCDF output: ',string(21:strmin(string))
      write(ioerr,'(A)',ERR=9999) 'Valid responses are:'
      write(ioerr,'(10X,A)',ERR=9999) YES
      write(ioerr,'(10X,A)',ERR=9999) NO
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(/,T20,A,A40,/)
 9001 format(T10,A,T40,:,':',A)
 9002 format(T10,A,T40,:,':',I5)
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
