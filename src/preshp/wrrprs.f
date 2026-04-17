C**** WRRPRS.F 
c
      subroutine wrrprs( ierr,
     &                         nshps, idxfip )
c
c-----------------------------------------------------------------------
c
c    Writes the EMBR records for the current facility and sets
c    the arrays to missing
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       nshps   I number of records currently in arrays
c       idxfip  I index of county in totals arrays
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
      include 'uamregn.inc'
      include 'sccxref.inc'
      include 'msgtot.inc'
      include 'iopreshp.inc'
      include 'prshpdat.inc'
      include 'preshptot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 nshps
      integer*4 idxfip
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 ishp, jerr, icl, jcl, i
      real*4    tottmp, totused(MXCRIT)
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL

c   --- write each record to EMBR file ---
c
c
      do 10 ishp = 1,nshps
c
c   --- skip records with no emissions ---
c
         tottmp = 0.
         do i=1,MXCRIT
            if( crtpol(i,ishp) .GT. 0.) tottmp = tottmp + crtpol(i,ishp)
         enddo
         if( tottmp .LE. 0.0 ) goto 10
c
c   --- set all EMBR common variables ---
c
         fipin = infips(1)
         sbrgn = insbrg(1)
         siccd = inscc(ishp)(1:4)
         scccd = inscc(ishp)
         rectyp = 'S'
         invbeg = ibegdt(ishp)
         invend = ienddt(ishp)
         plant = invesl(ishp)
         stkid = ' '
         point = ' '
         segno = '   '
         iutmzn = inzone(1)
         stkgev = RMISS
         stkdia = RMISS
         stktmp = RMISS
         stkhgt = RMISS
c
c  --- intialize the array that tracks the emissions gridded ---
c
         do i=1,MXCRIT
          totused(i) = 0.
         enddo
c
c  --- loop over cells intersecting this channel ---
c
         do icl=1,nxcell
            do jcl=1,nycell
              if( gridarea(icl,jcl) .GT. 0. ) then
                 ijcell = icl * 1000 + jcl
                 utme = xorig + (icl -0.5) * dxcell
                 utmn = yorig + (jcl -0.5) * dycell
                 do i=1,ncrthd
                    if( crtpol(i,ishp) .GT. 0. ) then
                      emout(i) = crtpol(i,ishp) * gridarea(icl,jcl)
                      totused(i) = totused(i) + emout(i)
c
c   --- add emissions to output totals ---
c
                      call totprs( jerr,IDXOUT,i,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtotcty( jerr,ICDOUT,idxfip,i,0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtotasc( jerr,ICDOUT,scccd,i,0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtotpid( jerr,ICDOUT,idxfip,plant,stkid,
     &                                             point,i,0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtotfcd( jerr,ICDOUT,idxfip,scccd,i,
     &                                                      0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtothrs( jerr,invbeg,invend,ICDOUT,
     &                                                    i,0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
c
                      call addtothcd( jerr,scccd,invbeg,invend,ICDOUT,
     &                                                    i,0,emout(i) )
                      if( jerr .NE. ISUCES ) goto 9999
                    else
                       emout(i) = RMISS
                    endif
                 enddo
c
c   --- call routine to write EMBR record ---
c
                 call wrembr( jerr,
     &                          IOWMBR, nrec(IDXOUT) )
                 if( jerr .NE. ISUCES ) goto 9999
                 nrec(IDXOUT) = nrec(IDXOUT) + 1
              endif
c
c   --- next cell ---
c
            enddo
         enddo
c
c   --- add un-gridded portion to totals array ----
c
      do i=1,MXCRIT
         if( crtpol(i,ishp)-totused(i) .GT. 0.0001 ) then
            call totprs( jerr,IDXREG,i,crtpol(i,ishp)-totused(i) )
            if( jerr .NE. ISUCES ) goto 9999
         endif
         crtpol(i,ishp) = 0.
      enddo
c
   10 continue
c
c   --- set the id arrays to missing ---
c
      infips(1) = '     '
      insbrg(1) = '     '
      invesl(1) = '          '
      nshps = 0
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
