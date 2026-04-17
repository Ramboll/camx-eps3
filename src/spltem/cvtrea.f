C**** CVTREA.F
c
      subroutine cvtrea( ierr )
c
c-----------------------------------------------------------------------
c
c    Routine to adjust/renormalize profile weight fractions based on
c    compound database reactivity flag.
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
      include 'iospltem.inc'
      include 'spltem.inc'
      include 'wgtfrac.inc'
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
c   fndint   I   returns the index of integer in array of integers
c
      integer*4 fndint
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
c   tmpvoc   R    temporary variable for percent reactive
c   wtvoc    R    adjusted weight fractions (VOC only)
c   wttog    R    adjusted weight fractions (all compounds)
c   wtrhc    R    adjusted weight fractions (non-renomalized VOC)
c   totvoc   R    total weight for entire profile (VOC only)
c   tottog   R    total weight for entire profile (all compounds)
c   totrhc   R    total weight for entire profile (non-renomalized VOC)
c
      integer*4  np, icmp, idxcdb, i
      real*4     tmpvoc, totvoc, totrhc
      real*4     tottog, wtvoc(MXWGT), wttog(MXWGT), wtrhc(MXWGT)
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
c     --- for each profile ---    ===================================
c
      do 100 np = 1, nprfwf
c
c         --- initialize temp variables ---
          totvoc = 0.
          tottog = 0.
          totrhc = 0.
          tmpvoc = 1.0
c
c         --- load the weight fractions into a local array ---
          do icmp=1,nwtswf(np)
             wtvoc(icmp) = wgtswf(np,icmp)
             wttog(icmp) = wgtswf(np,icmp)
             wtrhc(icmp) = wgtswf(np,icmp)
          enddo
c
c         --- if computing splits for VOC, zero out all nonreactive compounds ----
          do icmp=1,nwtswf(np)
c
c            --- find index of saroad ---
             idxcdb = fndint ( isrdwf(np,icmp), isrdcnt, isrdlst )
             if( idxcdb .LE. 0 ) goto 7001
c
c            --- if nonreactive, zero out weight fraction ---
             if( .NOT. lreact(idxcdb) ) wtvoc(icmp) = 0.
             if( .NOT. lreact(idxcdb) ) wtrhc(icmp) = 0.
c
c            --- add to total for profile (will only be total reactive) ---
             totvoc = totvoc + wtvoc(icmp)
             tottog = tottog + wttog(icmp)
             totrhc = totrhc + wgtswf(np,icmp)
          enddo
c
c         --- renormalize weight with just reactive compounds ---
          tmpvoc = totvoc
          do icmp=1,nwtswf(np)
             if( totvoc .GT. 0. ) wtvoc(icmp) = wtvoc(icmp) / totvoc
             if( tottog .GT. 0. ) wttog(icmp) = wttog(icmp) / tottog
             if( totrhc .GT. 0. ) wtrhc(icmp) = wtrhc(icmp) / totrhc
          enddo
c
c         --- now update weights with adjusted weight profile ---  ==========
c
          do 10 icmp=1,nwtswf(np)
c
c            --- find index of compound in data base ---
             idxcdb = fndint ( isrdwf(np,icmp), isrdcnt, isrdlst )
             if( idxcdb .LE. 0 ) goto 7001
c
c            --- if output is THC splits, adjust the weight factors ---
c
             if( lvocin .AND. lvocout ) then
                 usewt = wtvoc(icmp)
             else if( lvocin .AND. .NOT. lvocout ) then
                 if( .NOT. lreact(idxcdb) ) then
                     if( totvoc .GT. 0. ) then
                        usewt = wttog(icmp) / totvoc
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

             wgtswf(np,icmp) = usewt
c
c         --- next compound ---
   10     continue
c
c     --- next profile ---
  100 continue
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
 7001 continue
      write(IOWSTD,'(/,A)',ERR=9999) 'ERROR: Processing input data.'
      write(IOWSTD,'(2A)',ERR=9999) 'Compound in weight profile ',
     &                          'is not in the compound database.'
      write(IOWSTD,'(10X,2A)',ERR=9999) 
     &                      'Profile code          : ',profwf(np)
      write(IOWSTD,'(10X,A,I5)',ERR=9999) 'Chemical compound code: ',
     &                                              isrdwf(np,icmp)
      write(IOWSTD,'(2A,//)',ERR=9999) 'Update the database or remove ',
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
