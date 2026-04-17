C**** WRFPRF.F 
c
      subroutine wrfprf( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the newly computed temporal profiles to the output temporal
c    pofiles file
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
      include 'iotmpfac.inc'
      include 'tpfacprf.inc'
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
c   strmin   I  returns actual length of string
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Parameters:
c-----------------------------------------------------------------------
c
c   KEYMON  C   keyword used for search of /MONTHLY/ packet
c   KEYWEK  C   keyword used for search of /WEEKLY/ packet
c   KEYWKD  C   keyword used for search of /DIURNAL WEEKDAY/ packet
c   KEYWKE  C   keyword used for search of /DIURNAL WEEKEND/ packet
c
      character*(KEYLEN) KEYMON
      character*(KEYLEN) KEYWEK
      character*(KEYLEN) KEYWKD
      character*(KEYLEN) KEYWKE
c
      parameter( KEYMON = '/MONTHLY/' )
      parameter( KEYWEK = '/WEEKLY/' )
      parameter( KEYWKD = '/DIURNAL WEEKDAY/' )
      parameter( KEYWKE = '/DIURNAL WEEKEND/' )
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j
      logical*4 large, lsmall
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- write monthly profiles ---
c
      write(IOWPRF,'(A)',ERR=7000) KEYMON
      do i=1,nmncd
c
c   --- check for overflow (only have 3 digits accuracy for weights) ---
c
  111    continue
         large = .FALSE.
         lsmall = .TRUE.
         do j=1,MXMON
            if( imnfac(i,j) .GE. 1000 ) large = .TRUE.
            if( MOD(imnfac(i,j),10) .NE. 0 ) lsmall = .FALSE.
         enddo
         if( large .OR. lsmall .AND. imnfac(i,MXMON+1) .GT. 0.) then
            imnfac(i,MXMON+1) = 0
            do j=1,MXMON
               imnfac(i,j) = NINT( FLOAT(imnfac(i,j)/10) )
               imnfac(i,MXMON+1) = imnfac(i,MXMON+1) + imnfac(i,j)
            enddo
            goto 111
         endif
         write(IOWPRF,9000,ERR=7000) imncd(i),
     &                                     (imnfac(i,j),j=1,MXMON+1)
      enddo
      write(IOWPRF,'(A)',ERR=7000) KEYEND
c
c   --- write day of week profiles ---
c
      write(IOWPRF,'(A)',ERR=7000) KEYWEK
      do i=1,nwkcd
c
c   --- check for overflow (only have 3 digits accuracy for weights) ---
c
  222    continue
         large = .FALSE.
         lsmall = .TRUE.
         do j=1,MXWEK
            if( iwkfac(i,j) .GE. 1000 ) large = .TRUE.
            if( MOD(iwkfac(i,j),10) .NE. 0 ) lsmall = .FALSE.
         enddo
         if( large .OR. lsmall .AND. iwkfac(i,MXWEK+1) .GT. 0.) then
            iwkfac(i,MXWEK+1) = 0
            do j=1,MXWEK
               iwkfac(i,j) = NINT( FLOAT(iwkfac(i,j)/10) )
               iwkfac(i,MXWEK+1) = iwkfac(i,MXWEK+1) + iwkfac(i,j)
            enddo
            goto 222
         endif
         write(IOWPRF,9001,ERR=7000) iwkcd(i),
     &                                    (iwkfac(i,j),j=1,MXWEK+1)
      enddo
      write(IOWPRF,'(A)',ERR=7000) KEYEND
c
c   --- write weekday diurnal profiles ---
c
      write(IOWPRF,'(A)',ERR=7000) KEYWKD
      do i=1,ndycd(IDXMID)
c
c   --- check for overflow (only have 3 digits accuracy for weights) ---
c
  333    continue
         large = .FALSE.
         lsmall = .TRUE.
         do j=1,MXDAY
            if( idyfac(IDXMID,i,j) .GE. 1000 ) large = .TRUE.
            if( MOD(idyfac(IDXMID,i,j),10) .NE. 0 ) lsmall = .FALSE.
         enddo
         if( large .OR. lsmall .AND.
     &                           idyfac(IDXMID,i,MXDAY+1) .GT. 0.) then
            idyfac(IDXMID,i,MXDAY+1) = 0
            do j=1,MXDAY
               idyfac(IDXMID,i,j) = NINT( FLOAT(idyfac(IDXMID,i,j)/10) )
               idyfac(IDXMID,i,MXDAY+1) = idyfac(IDXMID,i,MXDAY+1)
     &                                           + idyfac(IDXMID,i,j)
            enddo
            goto 333
         endif
         write(IOWPRF,9002,ERR=7000) idycd(IDXMID,i),
     &                               (idyfac(IDXMID,i,j),j=1,MXDAY+1)
      enddo
      write(IOWPRF,'(A)',ERR=7000) KEYEND
c
c   --- write weekend diurnal profiles ---
c
      write(IOWPRF,'(A)',ERR=7000) KEYWKE
      do i=1,ndycd(IDXEND)
c
c   --- check for overflow (only have 3 digits accuracy for weights) ---
c
  444    continue
         large = .FALSE.
         lsmall = .TRUE.
         do j=1,MXDAY
            if( idyfac(IDXEND,i,j) .GE. 1000 ) large = .TRUE.
            if( MOD(idyfac(IDXEND,i,j),10) .NE. 0 ) lsmall = .FALSE.
         enddo
         if( large .OR. lsmall .AND.
     &                           idyfac(IDXEND,i,MXDAY+1) .GT. 0.) then
            idyfac(IDXEND,i,MXDAY+1) = 0
            do j=1,MXDAY
               idyfac(IDXEND,i,j) = NINT( FLOAT(idyfac(IDXEND,i,j)/10) )
               idyfac(IDXEND,i,MXDAY+1) = idyfac(IDXEND,i,MXDAY+1)
     &                                           + idyfac(IDXEND,i,j)
            enddo
            goto 444
         endif
         write(IOWPRF,9002,ERR=7000) idycd(IDXEND,i),
     &                               (idyfac(IDXEND,i,j),j=1,MXDAY+1)
      enddo
      write(IOWPRF,'(A)',ERR=7000) KEYEND
c
c   --- echo totals to message file ---
c
      write(IOWMSG,'(/,T10,3A,I8)',ERR=7001) 'Number of ',KEYMON,
     &                                ' profiles written: ',nmncd
      write(IOWMSG,'(/,T10,3A,I8)',ERR=7001) 'Number of ',KEYWEK,
     &                                 ' profiles written: ',nwkcd
      write(IOWMSG,'(/,T10,3A,I8)',ERR=7001) 'Number of ',KEYWKD,
     &                              ' profiles written: ',ndycd(IDXMID)
      write(IOWMSG,'(/,T10,3A,I8)',ERR=7001) 'Number of ',KEYWKE,
     &                              ' profiles written: ',ndycd(IDXEND)
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                'ERROR:  Writing output temporal profiles file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',oprfl(:strmin(oprfl))
      goto 9999
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,A)',ERR=9999) 'ERROR:  Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999)  'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(I5,12(I4),I5)
 9001 format(I5,7(I4),I5)
 9002 format(I5,24(I4),I5)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
