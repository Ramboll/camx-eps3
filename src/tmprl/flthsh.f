C**** FLTHSH.F 
c
      function flthsh( 
     &                 scccd )
      integer*4 flthsh
c
c-----------------------------------------------------------------------
c
c     This routine call the hash function and then searches through
c     searches through the link list of equpment types to find a match.
c     The return value is set to negative if no match is
c     found in which case the absolute value of the return value 
c     is the index of the link list array element to be filled (i.e.
c     the previous link).  
c
c   Arguments:
c
c     Inputs:
c       scccd   C   ASC-SCC code to match
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'iotmprl.inc'
      include 'tmprlfac.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*10 scccd
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c   ihash  I  hash function
c
      integer*4 ihash
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4    ihshvl, iptr
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ----
c
      flthsh = 0
c
c   --- call function to get hash value ---
c
      ihshvl = ihash( scccd )
c
c   ---- if initial value is zero, set initial value, increment
c        counter and fill arrays ---
c
      if( ihshpt(ihshvl) .LE. 0 ) then
          ntpfac = ntpfac + 1
          if( ntpfac .GT. MXFAC ) goto 7000
          ihshpt(ihshvl) = ntpfac
          flthsh = 0
          goto 9999
      endif
c
c   ---- loop through the link list ---
c
      iptr = ihshpt(ihshvl)
c
c   --- check for initial pointer null, if so look for match ---
c
  111 continue
c
      if( ilkptr(iptr) .LE. 0 ) then
         flthsh = iptr
         goto 9999
      endif
c
c  --- reset link list pointer, and check again ---
c
      iptr = ilkptr(iptr) 
c
      goto 111
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &        'ERROR:  Number of temporal profiles exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXFAC
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXFAC ',
     &                                                  'and recompile.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end


