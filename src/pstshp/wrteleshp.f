C**** WRTELESHP.F
c
      subroutine wrteleshp( ierr,
     &                         icurdt, emscrt, emsgrd, nhours )
c
c-----------------------------------------------------------------------
c
c    Write the ship emissions data to the elevated file and header file.
c
c    The facility ID is assigned "S" with sequential number.
c    The stack ID is assigned "SL" with vertical layer number.
c    The stack height is assigned the negative (-) of the layer midpoint.
c
c    Updates: June 2015
c             SL is not layer dependent
c             stack height assigned (-) plume height
c             velocity assigned (-) plume bottom
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
c    june 2015 mjimenez Updated to support the plume rise
c                       distribution override feature of CAMx
c
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'uamregn.inc'
      include 'embrdat.inc'
      include 'uamcntl.inc'
      include 'layers.inc'
      include 'iopstshp.inc'
      include 'pstshpstk.inc'
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
      character*(12*MXHOUR) emsstr
      character*10          fiptmp, oplnt, ostck,  keypts
      integer*4             idxhr, i, j, ipos, idate, ii
      integer*4             icount, icell, jcell
      real*4                xcoord, ycoord, tot, sumstk
      logical*4             lflag(MXLAY)
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data keypts /'ORIGINAL '/    
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- initialize error code ---
c
      ierr = IFAIL
c             
      idate = julcal( icurdt(1)/100 )
      icount = 0
c
c  --- for each unique stack ---
c
      do i=1,nstack
         icount = icount + 1
         sumstk = 0.0
c
c  --- loop over species ---
c
         do j=1,nspecs
            tot = 0.0
            emsstr = ' '
c
c  --- write hourly emissions to temporary string ---
c
            do idxhr=1,nhours
               ipos = (idxhr-1)*12 + 1
               if( emsgrd(j,i,idxhr) .GT. 0. ) then
                   tot = tot + emsgrd(j,i,idxhr)
                   if( emsgrd(j,i,idxhr) .GT. 10.0**6 ) then
                       write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7000) 
     &                                             emsgrd(j,i,idxhr) 
                   elseif( emsgrd(j,i,idxhr) .LT. 1.0 ) then
                        write(emsstr(ipos:ipos+11),'(F10.8)',ERR=7000) 
     &                                             emsgrd(j,i,idxhr) 
                   else
                       write(emsstr(ipos:ipos+11),'(F10.3)',ERR=7000) 
     &                                             emsgrd(j,i,idxhr) 
                   endif
               else
                       write(emsstr(ipos:ipos+11),'(F10.0)',ERR=7000)
     &                                                 0.0
               endif
            enddo
c
c   --- write non-zero emissions estimates ---
c
            if( tot .GT. 0.0 ) then
              sumstk = sumstk + tot
              fiptmp = fipstk(i)
              call lftjst( fiptmp )
              write(oplnt,9002) 'S', i
              write(ostck,9003) 'SL',1
              icell = INT( ijclstk(i) / 1000 )
              jcell = MOD( ijclstk(i), 1000 )
              xcoord = xorig + (FLOAT( icell ) - 0.5 ) * dxcell
              ycoord = yorig + (FLOAT( jcell ) - 0.5 ) * dycell
              if( izone .GE. 0 ) then
                  write(IOWELV,9000,ERR=7000) fiptmp, oplnt,
     &                   ostck, -1.*ptopstk(i), 1.0, 1.0, 
     &                   -1.*pbotstk(i), xcoord*1000.0, ycoord*1000.0, 
     &                         idate, spcrit(j),
     &                               emscrt(ispcrt(j),i),spclst(j), 
     &                                        emsstr(:strmin(emsstr))
              else    
                  write(IOWELV,9001,ERR=7000) fiptmp, oplnt, 
     &                    ostck, -1.*ptopstk(i), 1.0, 1.0, 
     &                     -1.*pbotstk(i), xcoord, ycoord, 
     &                         idate, spcrit(j),
     &                                emscrt(ispcrt(j),i),spclst(j), 
     &                                        emsstr(:strmin(emsstr))
              endif
            endif

c        --- next species ---
         enddo
c
c   --- write header record  for this stack ---
c
         if ( sumstk.GT.0.0 ) then
           icell = INT( ijclstk(i) / 1000 )
           jcell = MOD( ijclstk(i), 1000 )
           xcoord = xorig + (FLOAT( icell ) - 0.5 ) * dxcell
           ycoord = yorig + (FLOAT( jcell ) - 0.5 ) * dycell
           if( izone .GE. 0 ) then
            write(IOWHDR,9004,ERR=7000) keypts, fiptmp, oplnt, 
     &                ostck, -1.*ptopstk(i), 1.0, 1.0, -1.*pbotstk(i),
     &                            xcoord*1000.0, ycoord*1000.0
           else
            write(IOWHDR,9005,ERR=7000) keypts, fiptmp, oplnt, 
     &                ostck, -1.*ptopstk(i), 1.0, 1.0, -1.*pbotstk(i),
     &                                       xcoord, ycoord
           endif
         endif
c     --- next stack ---
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
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output emissions file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oelvfl(:strmin(oelvfl))
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
 9002 format(A1,I8.8)
 9003 format(A2,I3.3)
 9004 format(4(A10,1X),F10.1,1X,F10.3,1X,4(F10.0,1X))
 9005 format(4(A10,1X),F10.1,1X,F10.3,1X,2(F10.0,1X),2(F10.4,1X))
c
c-----------------------------------------------------------------------
c   Return Point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
