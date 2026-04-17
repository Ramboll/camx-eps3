C***** SETWEK.F 
c
      subroutine setwek( ierr, iweek,
     &                                pctin )
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of existing weekly
c     profiles searching for the a match with the input weekly
c     profile and source identification data.  If no match is made
c     the data is added to the arrays.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c       iweek    I   weekly profile code
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
      integer*4 iweek
      real*4    pctin(MXDAY)
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j
      real*4    total
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
      do i=1,MXWEK
         total = total + pctin(i)
      enddo
      if( total .LE. 0. ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- loop through the array searching for match of percentages ---
c
      do 10 i=1,nwkcd
         do j=1,MXWEK
c
c   --- skip to next if profile does not match ---
c
            if( pctin(j) .LT. pctwek(i,j) - (BFUZZ/2.0) .OR.
     &        pctin(j) .GT. pctwek(i,j) + (BFUZZ/2.0) ) then
                  goto 10
             endif
         enddo
c
c   --- profile matches, set code to current value ---
c
         iweek = iwkcd(i)
         goto 111
   10 continue
c
c   --- no codes matched, add this one to arrays ---
c
      nwkcd = nwkcd + 1
      if( nwkcd .GT. MXPRF ) goto 7000
      imxwek = imxwek + 1
      iweek = imxwek
      iwkcd(nwkcd) = imxwek
      iwkfac(nwkcd,MXWEK+1) = 0
      do i=1,MXWEK
         iwkfac(nwkcd,i) = INT( pctin(i)*100 )
         iwkfac(nwkcd,MXWEK+1) = iwkfac(nwkcd,MXWEK+1) +
     &                                           INT( pctin(i)*100 )
         pctwek(nwkcd,i) = pctin(i)
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
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of day-of-week ',
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
