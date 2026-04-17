C**** TOTLBS.F 
c
      subroutine totlbs( ierr,
     &                         idxtot, idxfip, emsval, inasc,
     &                                                ibegdt, ienddt)
c
c-----------------------------------------------------------------------
c
c     This routine adds the emissions to the totals array at the index
c     given by the input argument.  The number of records variable is
c     also incremented for that index.
c
c   Arguments:
c     Outputs:
c       ierr     I   error flag
c     Inputs:
c       idxtot   I   index of the record type in the totals array
c       idxfip   I   index of FIPS code in totals array
c                    (0 = do not add to FIPS totals)
c       emsval   R   emissions to add to totals for criteria pollutant
c       inasc    C   ASC code of emissions
c       ibegdt   I   beginning date of emissions record
c       ienddt   I   ending date of emissions record
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
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'lbasetot.inc'
c
c-----------------------------------------------------------------------
c   Argument Declaration:
c-----------------------------------------------------------------------
c
      integer*4    ierr
      integer*4    idxtot
      integer*4    idxfip
      real*4       emsval(*)
      character*10 inasc
      integer*4    ibegdt
      integer*4    ienddt
c
c-----------------------------------------------------------------------
c   External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr, i
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   ---- set error flag ---
c
      ierr = IFAIL
c
c   ---- add emissions to processing array ---
c
      do i=1,ncrthd
          if(emsval(i) .GT. 0.) totems(idxtot,i) = totems(idxtot,i)
     &                                              + DBLE(emsval(i))
      enddo
      nrec(idxtot) = nrec(idxtot) + 1
c
c   --- if not input or output then we are done ----
c
      if( idxtot .NE. IDXIN .AND. idxtot .NE. IDXOUT) then
          ierr = ISUCES
          goto 9999
      endif
c
c  --- loop over pollutants, adding to totals arrays ---
c
      do i=1,ncrthd
         call addtotcty( jerr,idxtot,idxfip,i,0,emsval(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotasc( jerr,idxtot,inasc,i,0,emsval(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotfcd( jerr,idxtot,idxfip,inasc,i,0,emsval(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothrs( jerr,ibegdt,ienddt,idxtot,i,0,emsval(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothcd( jerr,inasc,ibegdt,ienddt,idxtot,i,0,emsval(i) )
         if( jerr .NE. ISUCES ) goto 9999
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Statements:
c-----------------------------------------------------------------------
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
