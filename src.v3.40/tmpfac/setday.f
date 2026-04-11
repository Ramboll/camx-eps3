C***** SETDAY.F 
c
      subroutine setday( ierr, iday,
     &                               pctin )
c
c-----------------------------------------------------------------------
c
c     This routine searches through the array of existing diurnal
c     profiles searching for the a match with the input diurnal
c     profile and source identification data.  If no match is made
c     the data is added to the arrays.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c       iday     I   diurnal profile code
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
      integer*4 iday
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
      do i=1,MXDAY
         total = total + pctin(i)
      enddo
      if( total .LE. 0. ) then
          ierr = ISUCES
          goto 9999
      endif
c
c   ---- loop through the array searching for match of percentages ---
c
      do 10 i=1,ndycd(IDXMID)
         do j=1,MXDAY
c
c   --- skip to next if profile does not match ---
c
            if( pctin(j) .LT. pctday(IDXMID,i,j) - (BFUZZ/2.0) .OR.
     &       pctin(j) .GT. pctday(IDXMID,i,j) + (BFUZZ/2.0) ) goto 10
         enddo
c
c   --- profile matches, set code to current value ---
c
         iday = idycd(IDXMID,i)
         goto 111
   10 continue
c
c   --- no codes matched, add this one to arrays ---
c
      ndycd(IDXMID) = ndycd(IDXMID) + 1
      if( ndycd(IDXMID) .GT. MXPRF ) goto 7000
      imxday(IDXMID) = imxday(IDXMID) + 1
      iday = imxday(IDXMID)
      idycd(IDXMID,ndycd(IDXMID)) = imxday(IDXMID)
      idyfac(IDXMID,ndycd(IDXMID),MXDAY+1) = 0
      do i=1,MXDAY
         idyfac(IDXMID,ndycd(IDXMID),i) = INT( pctin(i)*100 )
         idyfac(IDXMID,ndycd(IDXMID),MXDAY+1) =
     &                      idyfac(IDXMID,ndycd(IDXMID),MXDAY+1)
     &                                            + INT( pctin(i)*100 )
         pctday(IDXMID,ndycd(IDXMID),i) = pctin(i)
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
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR:  Number of diurnal ',
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
