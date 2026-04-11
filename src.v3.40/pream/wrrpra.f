C*** WRRPRA.F 
c
      subroutine wrrpra( ierr,
     &                         idxfip )
c
c-----------------------------------------------------------------------
c
c    Writes the EMBR records for all pollutants
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       idxfip I index of county in county list
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
      include 'embrdat.inc'
      include 'msgtot.inc'
      include 'iopream.inc'
      include 'pream.inc'
      include 'preamtot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 idxfip
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 jerr, i
      real*4    emstot
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c
c   --- set all EMBR common variables ---
c
      fipin = infips
      sbrgn = insbrg
      if( insic .EQ. '    ' ) then
          siccd = inasc(1:4)
      else
          siccd = insic
      endif
      invbeg = ibegdt
      invend = ienddt
      emstot = 0.
      do i=1,ncrthd
        emout(i) = crtpol(i)
        if( emout(i) .GT. 0. ) emstot = emstot + emout(i)
      enddo
      scccd = inasc
c
c  --- if the record is all zero, skip this ---
c
      if( emstot .LE. 0 ) then
        ierr = ISUCES
        goto 9999
      endif
c
c  --- write out EMBR record and put emissions in totals ---
c
      rectyp = ' '
      curofl = outmfl
      call wrembr( jerr, IOWMBR, nrec(IDXOUT)+1 )
      if( jerr .NE. ISUCES ) goto 9999
      do i=1,ncrthd
         call totpra( jerr,IDXOUT,i,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotcty( jerr,ICDOUT,idxfip,i,0,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotasc( jerr,ICDOUT,scccd,i,0,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtotfcd( jerr,ICDOUT,idxfip,scccd,i,0,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothrs( jerr,ibegdt,ienddt,ICDOUT,i,0,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         call addtothcd( jerr,scccd,ibegdt,ienddt,ICDOUT,i,0,emout(i) )
         if( jerr .NE. ISUCES ) goto 9999
c
         emout(i) = RMISS
      enddo
c
c   --- set pollutant arrays to missing value ---
c
      do i=1,ncrthd
         crtpol(i) = RMISS
      enddo
c
c   --- set error flag to sucess ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
