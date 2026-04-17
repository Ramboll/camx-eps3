C**** PRCEVT.F 
c
      subroutine prcevt( ierr )
c
c-----------------------------------------------------------------------
c
c    Main processing routine for EMSCVT program.  Each compound in the
c    current profile is found in the compound data base and the
c    contribution to each CBIV species is calculated based on the
c    carbon split values for the compound.  The percent reactive and
c    toxic values are also calculated.  Once the splits values
c    are calculated, the cross reference arrays are searched for any
c    profile code matching the current value and all of the data for
c    the profile is updated.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'sccxref.inc'
      include 'ioemscvt.inc'
      include 'emcvtprf.inc'
      include 'emcvtspl.inc'
      include 'usremcvt.inc'
      include 'cmpdbase.inc'
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
c   fndchr   I   returns the index of string in an array of string
c   fidint   I   returns the index of integer in array of integers
c
      integer*4 fndchr
      integer*4 fndint
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   idxprf   I    index of profile in splitting factors file
c   splval   R    splitting factors for current profile
c   tmpvoc   R    temporary variable for percent reactive
c   wtvoc    R    adjusted weight fractions (VOC only)
c   wttog    R    adjusted weight fractions (all compounds)
c   wtrhc    R    adjusted weight fractions (non-renomalized VOC)
c   totvoc   R    total weight for entire profile (VOC only)
c   tottog   R    total weight for entire profile (all compounds)
c   totrhc   R    total weight for entire profile (non-renomalized VOC)
c
      integer*4  idxprf, icmp, idxcdb, ixrf, idxctl, i
      real*4     tmpvoc, totvoc, totrhc, splval(MXSPEC)
      real*4     tottog, wtvoc(MXWPRF), wttog(MXWPRF), wtrhc(MXWPRF)
      real*4     usewt
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- find index of profile in splits array ---
c
       idxprf = fndchr ( prfcod, LENPRF, prfspl, nfac )
       if( idxprf .LE. 0 ) then
           nfac = nfac + 1
           if(nfac .GT. MXSPL) goto 7000
           prfspl(nfac) = prfcod
           idxprf = nfac
       endif
c
c   --- initialize temp variables ---
c
       totvoc = 0.
       tottog = 0.
       totrhc = 0.
       tmpvoc = 1.0
c
c   --- initialize split values ---
c
       do i=1,MXSPEC
          splval(i) = 0
       enddo
c
c   --- load the weight fractions into a local array ---
c
       do icmp=1,nwate
          wtvoc(icmp) = wtfrac(icmp)
          wttog(icmp) = wtfrac(icmp)
          wtrhc(icmp) = wtfrac(icmp)
       enddo
c
c   --- if computing splits for VOC, zero out all
c       nonreactive compounds ----
c
       do icmp=1,nwate
c
c   --- find index of compound in data base ---
c
          idxcdb = fndint ( icmpcd(icmp), ncmpnd, isarod )
          if( idxcdb .LE. 0 ) goto 7001
c
c   --- if nonreactive, zero out weight fraction ---
c
          if( .NOT. lreact(idxcdb) ) wtvoc(icmp) = 0.
          if( .NOT. lreact(idxcdb) ) wtrhc(icmp) = 0.
c
c   --- add to total for profile (will only be total reactive) ---
c
          totvoc = totvoc + wtvoc(icmp)
          tottog = tottog + wttog(icmp)
          totrhc = totrhc + wtfrac(icmp)
       enddo
c
c   --- renormalize weight with just reactive compounds ---
c
       tmpvoc = totvoc
       do icmp=1,nwate
          if( totvoc .GT. 0. ) wtvoc(icmp) = wtvoc(icmp) / totvoc
          if( tottog .GT. 0. ) wttog(icmp) = wttog(icmp) / tottog
          if( totrhc .GT. 0. ) wtrhc(icmp) = wtrhc(icmp) / totrhc
       enddo
c
c   --- if doing controls make the adjustments after the renormalization ---
c
      if( lctlfl ) then
         do icmp=1,nwate
             idxctl =  fndint( icmpcd(icmp), nctlfac, ictlcod )
             if( idxctl .GT. 0 ) wtfrac(icmp) = wtfrac(icmp) *
     &                                              ctlfac(idxctl)
             if( idxctl .GT. 0 ) wttog(icmp) = wttog(icmp) *
     &                                              ctlfac(idxctl)
             if( idxctl .GT. 0 ) wtrhc(icmp) = wtrhc(icmp) *
     &                                              ctlfac(idxctl)
         enddo
      endif
c
c   --- now compute splits with adjusted weight profile ---
c
      do 10 icmp=1,nwate
c
c   --- find index of compound in data base ---
c
          idxcdb = fndint ( icmpcd(icmp), ncmpnd, isarod )
          if( idxcdb .LE. 0 ) goto 7001
c
c   ---- add contribution from this compound for each species in
c        compound data base ----
c
          if( molcmp(idxcdb) .EQ. 0. ) goto 10
          do i=1,nspcmp
c
c   --- if doing THC splits, adjust the split factors ---
c
              if( lvocin .AND. lvocout ) then
                  usewt = wtvoc(icmp)
              else if( lvocin .AND. .NOT. lvocout ) then
                  if( .NOT. lreact(idxcdb) ) then
                      if( totvoc .GT. 0. ) then
c modified, this was being renormalized a second time
cccc                  ccc   usewt = wttog(icmp) / totvoc
                         usewt = wtfrac(icmp) / totvoc
                      else
                         usewt = wttog(icmp)
                      endif
                  else
                      usewt = wtvoc(icmp)
                  endif
              else if( .NOT. lvocin .AND. lvocout ) then
                  usewt = wtrhc(icmp)
              else if( .NOT. lvocin .AND. .NOT. lvocout ) then
                  usewt = wttog(icmp)
              endif
              splval(i) = splval(i) + bndfrc(idxcdb,i) * 
     &                                   usewt * (1/molcmp(idxcdb))
          enddo
c
c   --- next compound ---
c
   10 continue
c
c   --- update splits file for this profile ---
c
      do i=1,nspecs
         if( idxcmp(i) .GT. 0 ) then
              spltab(idxprf,i) = splval(idxcmp(i))
         else
              spltab(idxprf,i) = defspl(i)
         endif
      enddo
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
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Processing input data.'
      write(ioerr,'(A)',ERR=9999) 'Number of profiles exceeds max.'
      write(ioerr,'(10X,A,I10)',ERR=9999) 'Maxumium allowed : ',MXSPL
      write(ioerr,'(2A,//)',ERR=9999) 'Increase the parameter MXSPL ',
     &                                                  'and recompile.'
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999) 'ERROR: Processing input data.'
      write(ioerr,'(2A)',ERR=9999) 'Compound in weight profile ',
     &                          'is not in the compound database.'
      write(ioerr,'(10X,2A)',ERR=9999) 'Profile code          : ',prfcod
      write(ioerr,'(10X,A,I5)',ERR=9999) 'Chemical compound code: ',
     &                                                     icmpcd(icmp)
      write(ioerr,'(2A,//)',ERR=9999) 'Update the database or remove ',
     &                            'the profile from the weights file.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
