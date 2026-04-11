C**** SETMON.F 
c
      subroutine setmon( ierr, imonth,
     &                                 pctin )
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of existing monthly
c     profiles searching for the a match with the input monthly
c     profile and source identification data.  If no match is made
c     the data is added to the arrays.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c       imonth   I   monthly profile code
c     Inputs:
c       pctin    R   array of percentages representing the profiles
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'tpfacprf.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 imonth
      real*4    pctin(MXDAY)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j
      real*4    total, valmin
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- initialize return value ---
c
      ierr = IFAIL
c
c   ---- if profile is zero then just exit ---
c
      total = 0
      valmin = 0.
      do i=1,MXMON
         total = total + pctin(i)
         valmin = MIN(valmin,pctin(i))
      enddo
      if( total .LE. 0. .OR. valmin .LT. 0. ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- loop through the array searching for match of percentages ---
c
      do 10 i=1,nmncd
         do j=1,MXMON
c
c   --- skip to next if profile does not match ---
c
            if( pctin(j) .LT. pctmon(i,j) - (BFUZZ/2.0) .OR.
     &         pctin(j) .GT. pctmon(i,j) + (BFUZZ/2.0) ) goto 10
         enddo
c
c   --- profile matches, set code to current value ---
c
         imonth = imncd(i)
         goto 111
   10 continue
c
c   --- no codes matched, add this one to arrays ---
c
      nmncd = nmncd + 1
      if( nmncd .GT. MXPRF ) goto 7000
      imxmon = imxmon + 1
      imonth = imxmon
      imncd(nmncd) = imxmon
      imnfac(nmncd,MXMON+1) = 0
      do i=1,MXMON
         imnfac(nmncd,i) = INT( pctin(i)*1000 )
         imnfac(nmncd,MXMON+1) = imnfac(nmncd,MXMON+1) +
     &                                         INT( pctin(i)*1000 )
         pctmon(nmncd,i) = pctin(i)
      enddo
c
c  --- set return flag to sucess ---
c
  111 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of monthly ',
     &                                 'profile codes in exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maximum allowed: ',MXPRF
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXPRF ',
     &                                                  'and recompile.'
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
