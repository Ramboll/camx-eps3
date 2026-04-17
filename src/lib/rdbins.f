c**** RDBINS.F
c
      subroutine rdbins( ierr,
     &                         iounit )
c
c-----------------------------------------------------------------------
c
c    Reads the USERIN input file for the /FIRE BINS/ packet and stores
c    in common variables to be used by the PREFIR program.
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
      include 'firebin.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
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
c   keywrd  C   temporary storage for seaching for keyword
c   respon  C   character string for temporary storage
c   ndefb   I   number of default fire bin classes
c   sizdef  R   array of default fire sizes
c   buoydef R   buoyancy efficient by fire class defaults
c   topdef  R   Ptop maximum by fire class defaults   
c   botdef  R   Pbot maximum by fire class defaults   
c
      character*(5*MXSTR) string
      character*(KEYLEN)  keywrd, keytmp
      character*(MXSTR)   respon
      real*4              sizdef(MXBIN), sizin
      real*4              buoydef(MXBIN), bein
      real*4              topdef(MXBIN), topin
      real*4              botdef(MXBIN), botin
      integer*4           ndefb
      integer*4           irec, jerr, i
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/FIRE BINS/'/
c
      data ndefb /5/
      data sizdef /10., 100., 1000., 5000., 9999./
      data buoydef / 0.40, 0.60, 0.75, 0.85, 0.90 /
      data topdef /160., 2400., 6400., 7200., 8000./
      data botdef /0., 900., 2200., 3000., 3000./
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = 0
      nfbin = 0
      lbindef = .FALSE.
c
c     --- call routine to find /FIRE BINS/ packet ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
c
c     --- if packet not found set bins to defaults ---
c
      if( jerr .NE. ISUCES ) then 
          lbindef = .TRUE.
          nfbin = ndefb
          do i = 1, ndefb
             sizfbin(i) = sizdef(i)
             befbin(i) = buoydef(i)
             topfbin(i) = topdef(i)
             botfbin(i) = botdef(i)
          enddo
          goto 222
      endif
c
c     --- read the packet variables ---
c
  111 continue
      irec = irec + 1
      read(iounit,'(A)',ERR=7002,END=7003) string
      keytmp = string(1:19)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222

      respon = string(21:MXSTR)
      read(respon,*,ERR=7002,END=7003) sizin, bein, topin, botin

      nfbin = nfbin + 1
      if ( nfbin .GT. MXBIN ) goto 7001
      sizfbin(nfbin) = sizin
      befbin(nfbin) = bein
      topfbin(nfbin) = topin
      botfbin(nfbin) = botin
c
c     --- check that classes are defined by increasing fire size ---
c
      if ( nfbin .GT. 1 .AND.
     &         sizfbin(nfbin) .LE. sizfbin(nfbin-1) ) goto 7004
c
c     --- get the next fire class ---
c
      goto 111
c
c     --- finished reading packet ---
c
  222 continue
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
 7001 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(A)',ERR=9999) 'Number of fire bins exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXBIN
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXBIN ',
     &                                                 'and recompile.'
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
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'Fire Bins (classes) must be ',
     &                   'specified in increasing fire size order.'
      write(ioerr,'(A,//)',ERR=9999) 
     &                'From smallest virtual acreage to largest.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
