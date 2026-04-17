C**** WRRFIR.F
c
      subroutine wrrfir( ierr)
c
c-----------------------------------------------------------------------
c
c    Writes the EMBR records for either the current set of data 
c    or all combined fire sources.
c
c    Argument Declaration.
c     Outputs:
c       ierr   I error flag
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
      include 'ioprefir.inc'
      include 'usrprfir.inc'
      include 'prfirtot.inc'
      include 'prfirpnt.inc'
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
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*10   string
      integer*4      ipnt, jerr, i
      integer*4      icur, jcur, irg, jrg
      real*4         tottmp, x, y
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c     --- initialize error flag ---
c
      ierr = IFAIL
c
c     --- if points have been combined, write all EMBR records now ---
c
      if ( lcomb ) then
         do 10 ipnt = 1,ncmb
c
c           --- skip records with no emissions ---
c
            tottmp = 0.
            do i=1,MXCRIT
               if( emiscmb(i,ipnt) .GT. 0.) tottmp = tottmp
     &                                            + emiscmb(i,ipnt)
            enddo
c
c           --- set all EMBR common variables ---
c
            if( tottmp .LE. 0.0 ) goto 10
            fipin = cmbfips(ipnt)
            scccd = cmbscc(ipnt)
            invbeg = ibegcmb(ipnt)
            invend = iendcmb(ipnt)
            string = '          '
            write(string,9000) 'F',ipnt
            plant = string
            string = '          '
            write(string,9001) 'FC', pntscmb(ipnt)
            stkid = string
c           --- set point location to cell center ---
            icur = INT(ijcmb(ipnt)/10000)
            jcur = MOD( ijcmb(ipnt), 10000 )
            utme = rgutme 
     &             + ( icur * dxcomb )
     &             - ( dxcomb * .5 )
            utmn = rgutmn
     &             + ( jcur * dxcomb )
     &             - ( dxcomb * .5 )
            iutmzn = irgzn
            stkdia = ibincmb(ipnt)
            do i=1,ncrthd
               if( emiscmb(i,ipnt) .GT. 0. ) then
                   emout(i) = emiscmb(i,ipnt)
               else
                   emout(i) = RMISS
               endif
            enddo
c
c           --- call routine to write EMBR record ---
c
            call wrembr( jerr,
     &                      IOWMBR, nrec(IDXMBR) )
            if( jerr .NE. ISUCES ) goto 9999
            nrec(IDXMBR) = nrec(IDXMBR) + 1
c
   10    continue
c
c     --- write each record to EMBR file ---
c
      else
         tottmp = 0.
         do i=1,MXCRIT
            if( curems(i) .GT. 0.) tottmp = tottmp + curems(i)
         enddo
c
c        --- set all EMBR common variables ---
c
         if( tottmp .LE. 0.0 ) goto 20
         fipin = curfips
         scccd = curscc
         invbeg = curbegdt
         invend = curenddt
         plant = curplnt
         stkid = curstck
         utme = xloccur
         utmn = yloccur
         iutmzn = irgzn
         stkdia = curbin
         do i=1,ncrthd
            if( curems(i) .GT. 0. ) then
                emout(i) = curems(i)
            else
                emout(i) = RMISS
            endif
         enddo
c
c        --- call routine to write EMBR record ---
c
         call wrembr( jerr,
     &                   IOWMBR, nrec(IDXMBR) )
         if( jerr .NE. ISUCES ) goto 9999
         nrec(IDXMBR) = nrec(IDXMBR) + 1
      endif
c
c     --- set error flag to sucess ---
c
   20 continue
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A1,I8.8)
 9001 format(A2,I4.4)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
