c**** RDLAYERS.F
c
      subroutine rdlayers( ierr, iounit )
c
c-----------------------------------------------------------------------
c
c    Calls routines to read the /LAYERS/ packet of the USERIN file
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit  I  logical unit number of file to read
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
       include 'layers.inc'
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
c
      character*(MXSTR)   string
      character*(KEYLEN)  keywrd, keytmp
      character*(MXSTR)   respon
      character*40        cdate
      character*10        module
      integer*4           jerr, i, irec
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keywrd /'/LAYERS/'/
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
      irec = 0
c
c   --- call routine to find /LAYERS/ packet ---
c
      call fndkey( jerr,
     &                   iounit, keywrd )
      if( jerr .EQ. IRDERR ) goto 9999
      if( jerr .NE. ISUCES ) goto 7000
c
c     --- read the packet variables, vertical layer structure ---
c
      aglht(0) = 0.
      nlayer = 0
  111 continue
      irec = irec + 1
      read(iounit,'(A)',ERR=7001,END=7002) string
      keytmp = string(1:19)
      call low2up( keytmp )
      call lftjst( keytmp )
      if( keytmp .EQ. KEYEND ) goto 222

      nlayer = nlayer + 1
      if ( nlayer .GT. MXLAY ) goto 7003
      read(string,'(20X,F10.0)',ERR=7001,END=7002) aglht(nlayer)
c
c     --- check that layers are defined by increasing heights ---
c
      if ( nlayer .GT. 1 .AND.
     &         aglht(nlayer) .LE. aglht(nlayer-1) ) goto 7004
c
c     --- get the next layer ---
c
      goto 111
c
 222  continue   
      if ( nlayer .LE. 0 ) goto 7005
c
c     --- determine the vertical layer midpoints and thickness ---
c
      ctrlayr(1) = .5 * aglht(1)
      dzlayr(1) = aglht(1)
      do i = 2, nlayer
         ctrlayr(i) = ( ( aglht(i) - aglht(i-1) ) * .5 ) + aglht(i-1)
         dzlayr(i) =   aglht(i) - aglht(i-1) 
      enddo
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
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Reading the USERIN file.'
      write(ioerr,'(3A,//)',ERR=9999) 'This program requires the ',
     &                     keywrd(:strmin(keywrd)),' packet of USERIN.'
      ierr = IFAIL
      goto 9999
c
 7001 continue
      write(ioerr,'(/,3A,I8,//)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      goto 9999
c
 7002 continue
      write(ioerr,'(/,3A,I8)',ERR=9999) 'ERROR: Reading ',
     & keywrd(:strmin(keywrd)),' packet of USERIN file at line: ',irec
      write(ioerr,'(A,//)',ERR=9999) 'Premature end-of-file reached.'
      goto 9999
c
 7003 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(A)',ERR=9999) 'Number of layers exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXLAY
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXLAY ',
     &                                                 'and recompile.'
      goto 9999
c
 7004 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(2A)',ERR=9999) 'Layers must be ',
     &                   'specified in increasing AGL heights (m).'
      write(ioerr,'(A,//)',ERR=9999) 
     &                'From lowest to highest.'
      goto 9999
c
 7005 continue
      write(ioerr,'(/,3A,I5)',ERR=9999) 'ERROR: Reading ',
     &                keywrd(:strmin(keywrd)),' packet of USERIN file.'
      write(ioerr,'(A)',ERR=9999) 'No layers specified in packet.'
      write(ioerr,'(A,//)',ERR=9999) 
     &                'A minimum of a single layer is required.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9001 format(/,T20,A,A40,/)
 9002 format(T10,A,T40,:,':',A)
 9003 format(T10,A,T40,:,':',I5,:,I5,:,I5)
 9005 format(T10,A,T40,:,': (',F10.3,',',F10.3,')')
 9006 format(T10,A,T40,:,': (',I5,',',I5,')')
 9007 format(T10,A,T40,:,':',F12.2,1X,'(',I2,')')
 9008 format(T10,A,T40,:,':',I6)
 9009 format(T40,:,' ',I5,2X,F8.1,1X,F8.3,1X,2(F8.1,1X))
 9010 format(T40,:,A,A)
c
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
