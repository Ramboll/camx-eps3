C**** RDURET.F 
c
      subroutine rduret( ierr )
c
c-----------------------------------------------------------------------
c
c    Reads the user input file for the EMBRET program.  It is not
c    necessary for any packet to be included
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
      include 'ioembret.inc'
      include 'embret.inc'
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
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYFIP  C   keyword for the /COUNTY/ packet
c   KEYPLT  C   keyword for the /PLANT/ packet
c   KEYSCC  C   keyword for the /SCC/ packet
c   KEYSIC  C   keyword for the /SIC/ packet
c   KEYSUB  C   keyword for the /SUBGRID/ packet
c
      character*(KEYLEN) KEYFIP
      character*(KEYLEN) KEYPLT
      character*(KEYLEN) KEYSCC
      character*(KEYLEN) KEYSIC
      character*(KEYLEN) KEYSUB
c
      parameter( KEYFIP = '/COUNTY/'   )
      parameter( KEYPLT = '/PLANT/'    )
      parameter( KEYSCC = '/SCC/'      )
      parameter( KEYSIC = '/SIC/'      )
      parameter( KEYSUB = '/SUBGRID/'  )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   fiptmp  C   temporary storage for reading FIPS code
c   ibegtm  I   temporary storage for reading subgrid data
c   iendtm  I   temporary storage for reading subgrid data
c   jbegtm  I   temporary storage for reading subgrid data
c   jendtm  I   temporary storage for reading subgrid data
c   jerr    I   error flag for subroutine calls
c   keytmp  C   character string for temporary storage
c   keywrd  C   temporary storage for seaching fo keyword
c   plntmp  C   temporary storage for reading plant code
c   sbrtmp  C   temporary storage for reading FIPS code
c   scctmp  C   temporary storage for reading SCC code
c   sictmp  C   temporary storage for reading SIC code
c   string  C   character string for temporary storage
c
      character*(5*MXSTR) string, instr
      character*(KEYLEN)  keywrd, keytmp
      character*10        scctmp, plntmp
      character*5         fiptmp
      character*4         sictmp
      integer*4           ibegtm, iendtm, jbegtm, jendtm, irec, jerr
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- assume success ---
c
      ierr = IFAIL
c
c   --- call routine to find /COUNTY/ packet ---
c
      keywrd = KEYFIP
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 221
c
c   --- read the packet variables ---
c
      irec = 0
  111 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7000,END=7001 ) instr
      keytmp = instr(:20)
      string = instr(21:)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 221
      call lftjst( string )
      fiptmp = string(1:5)
      ncdcty = ncdcty + 1
      if( ncdcty .GT. MXFIP ) goto 7002
      ctyscd(ncdcty) = fiptmp
      goto 111
c
c   --- call routine to find /SIC/ packet ---
c
  221 continue
      irec = 0
      keywrd = KEYSIC
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 222
c
c   --- read the packet variables ---
c
  112 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7000,END=7001 ) instr
      keytmp = instr(:20)
      string = instr(21:)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222
      call lftjst( string )
      sictmp = string(1:4)
      ncdsic = ncdsic + 1
      if( ncdsic .GT. MXSIC ) goto 7003
      rsiccd(ncdsic) = sictmp
      goto 112
c
c   --- call routine to find /SCC/ packet ---
c
  222 continue
      irec = 0
      keywrd = KEYSCC
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 223
c
c   --- read the packet variables ---
c
  113 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7000,END=7001 ) instr
      keytmp = instr(:20)
      string = instr(21:)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 223
      call lftjst( string )
      scctmp = string(1:10)
      ncdscc = ncdscc + 1
      call rgtjst( scctmp )
      if( ncdscc .GT. MXSCC ) goto 7004
      rscccd(ncdscc) = scctmp
      goto 113
c
c   --- call routine to find /PLANT/ packet ---
c
  223 continue
      irec = 0
      keywrd = KEYPLT
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 224
c
c   --- read the packet variables ---
c
  114 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7000,END=7001 ) instr
      keytmp = instr(:20)
      string = instr(21:)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 224
      call lftjst( string )
      plntmp = string(1:10)
      call lftjst(plntmp)
      ncdpln = ncdpln + 1
      if( ncdpln .GT. MXPLNT ) goto 7005
      plntcd(ncdpln) = plntmp
      goto 114
c
c   --- call routine to find /SUBGRID/ packet ---
c
  224 continue
      irec = 0
      keywrd = KEYSUB
      call fndkey( jerr,
     &                   IORINP, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 225
c
c   --- read the packet variables ---
c
  115 continue
      irec = irec + 1
      read(IORINP,'(A)',ERR=7000,END=7001 ) instr
      keytmp = instr(:20)
      string = instr(21:)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 225
      read( string,'(4I10)',ERR=7006 ) ibegtm, jbegtm, iendtm, jendtm
      nsub = nsub + 1
      if( nsub .GT. MXSUB ) goto 7007
      ibegcl(nsub) = ibegtm*1000 + jbegtm
      iendcl(nsub) = iendtm*1000 + jendtm
      goto 115
c
c   --- call routine to find /RECTYP/ packet ---
c
  225 continue
      write(IOWMSG,'(/,A,I8)',ERR=7008) 
     &                     'Number of /COUNTY/ records read:',ncdcty
      write(IOWMSG,'(/,A,I8)',ERR=7008) 
     &                     'Number of /SIC/ records read:',ncdsic
      write(IOWMSG,'(/,A,I8)',ERR=7008) 
     &                     'Number of /SCC/ records read:',ncdscc
      write(IOWMSG,'(/,A,I8)',ERR=7008) 
     &                     'Number of /PLANT/ records read:',ncdpln
      write(IOWMSG,'(/,A,I8)',ERR=7008) 
     &                     'Number of /SUBGRID/ records read:',nsub
c
c   ---- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     &                                      keywrd(:strmin(keywrd)),
     &                    ' packet of user input file at line: ',irec
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     &                                      keywrd(:strmin(keywrd)),
     &                    ' packet of user input file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of user input file.'
      write(ioerr,'(3A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFIP
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFIP ',
     &                                                  'and recompile.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of user input file.'
      write(ioerr,'(3A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSIC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSIC ',
     &                                                  'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of user input file.'
      write(ioerr,'(3A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSCC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSCC ',
     &                                                  'and recompile.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of user input file.'
      write(ioerr,'(3A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPLNT
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPLNT ',
     &                                                  'and recompile.'
      goto 9999
c
 7006 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     &                                      keywrd(:strmin(keywrd)),
     &                    ' packet of user input file at line: ',irec
      write(ioerr,'(2A)',ERR=9999) 'File: ',uinpfl(:strmin(uinpfl))
      write(ioerr,'(A)',ERR=9999)  'Line read:'
      write(ioerr,'(2A,//)',ERR=9999) instr(:strmin(instr))
      goto 9999
c
 7007 continue
      write(ioerr,'(/,3A)',ERR=9999) 'ERROR: Reading ',
     &        keywrd(:strmin(keywrd)),' packet of user input file.'
      write(ioerr,'(3A)',ERR=9999) 'Number of records exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXSUB
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSUB ',
     &                                                  'and recompile.'
      goto 9999
c
 7008 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
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
