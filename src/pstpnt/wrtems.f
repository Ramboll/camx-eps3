C**** WRTEMS.F  
c
      subroutine wrtems( ierr,
     &                         icurdt, emscrt, emsgrd, nhours )
c
c-----------------------------------------------------------------------
c
c    Writes the emissions data for one hour to the output file
c
c    Argument Declaration.
c     Outputs:
c       ierr     I  error flag
c     Inputs:
c       icurdt   I  current date and time
c       emscrt   R  daily criteria emissions for each stack (tons)
c       emsgrd   R  hourly speciated emissions for each stack (moles)
c       nhours   I  number of hours prcoessed in this episode
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
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'iopstpnt.inc'
      include 'pstpntstk.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 nhours
      integer*4 icurdt(MXHOUR)
      real*4    emscrt(MXCRIT,MXSTK)
      real*4    emsgrd(MXSPEC,MXSTK,MXHOUR)
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
c    julcal  I   converts julian date to calender date
c    strmin  I   returns the true length of a string
c
      integer*4 julcal
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      character*(2*MXSTR) emsstr
      character*10        fiptmp
      integer*4           idxhr, i, j, ipos, idate
      integer*4           icount, iend, k
      real*4              xcoord, ycoord, tot
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
c             
c   --- write emissions values ---
c
         idate = julcal( icurdt(1)/100 )
         icount = 0
         do 20 i=1,nstack
            if( ptflag(i) .NE. 'E' ) goto 20
            icount = icount + 1

            do 30 j=1,nspecs
               tot = 0.0
               emsstr = ' '
               do 40 idxhr=1,nhours
c
c  ---  write hourly emissions to temporary string ---
c
                  ipos = (idxhr-1)*12 + 1
                  if( emsgrd(j,i,idxhr) .GT. 0. ) then
                      tot = tot + emsgrd(j,i,idxhr)
                      if( emsgrd(j,i,idxhr) .GT. 10.0**6 ) then
                          write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7000) 
     &                                               emsgrd(j,i,idxhr) 
                      else if( emsgrd(j,i,idxhr) .LT. 1.0 ) then
                          write(emsstr(ipos:ipos+11),'(F10.8)',ERR=7000) 
     &                                               emsgrd(j,i,idxhr) 
                      else
                          write(emsstr(ipos:ipos+11),'(F10.3)',ERR=7000) 
     &                                               emsgrd(j,i,idxhr) 
                      endif
                  else
                          write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7000)
     &                                                 0.0
                  endif
  40           continue
c
c --- write stack parameters, pollutant, and 24-hourly 
c     emissions estimates ---
c
               if ( tot .GT. 0.0 ) then
                 fiptmp = infips(i)
                 call lftjst( fiptmp )
                 call lftjst( inplnt(i) )
                 call lftjst( stckin(i) )
                 if( izone .GE. 0 ) then
                  xcoord = xloc(i) * 1000
                  ycoord = yloc(i) * 1000
                  write(IOWEMS,9000,ERR=7000) fiptmp, inplnt(i), 
     &                    stckin(i), stkht(i), stdiam(i), stextp(i), 
     &                     stexvl(i), xcoord, ycoord, idate, spcrit(j),
     &                                   emscrt(ispcrt(j),i),spclst(j), 
     &                                           emsstr(:strmin(emsstr))
                 else    
                  xcoord = xloc(i) 
                  ycoord = yloc(i) 
                  write(IOWEMS,9001,ERR=7000) fiptmp, inplnt(i), 
     &                    stckin(i), stkht(i), stdiam(i), stextp(i), 
     &                     stexvl(i), xcoord, ycoord, idate, spcrit(j),
     &                                    emscrt(ispcrt(j),i),spclst(j), 
     &                                           emsstr(:strmin(emsstr))
                 endif
               endif
  30        continue
  20     continue
c
c  --- write the binary file containing the stack/SCC emissions totals ---
c
      if( lstksfl ) then
          do i=1,nstack
            if( i .LT. nstack ) then
               iend = iptscc(i+1)-1
            else
               iend = nptscc
            endif
            do j=iptscc(i),iend
              write(IOWSTK,ERR=7001) infips(i), inplnt(i), stckin(i),
     &              ptsic(j), ptscc(j), ptflag(i), xloc(i), yloc(i),
     &                                       (emptscc(j,k),k=1,ncrthd)
             enddo
          enddo
      endif
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
     &                       'ERROR:  Writing output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oemsfl(:strmin(oemsfl))
      goto 9999
c
 7001 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output STACK/SCC file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',stksfl(:strmin(stksfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format Statements:
c-----------------------------------------------------------------------
c
 9000 format(3(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X),I6,1X,A10,1X,
     &                                               F10.3,1X,A10,1X,A)
 9001 format(3(A10,1X),F10.1,1X,F10.3,1X,2(F10.0,1X),2(F10.4,1X),I6,1X,
     &                                        A10,1X,F10.3,1X,A10,1X,A)
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
