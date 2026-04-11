C***** WRMEDS.F 
c
      subroutine wrmeds( ierr,
     &                         iounit, irec )
c
c-----------------------------------------------------------------------
c
c    Writes one record of the output MEDS file
c
c    Argument declaration.
c     Outputs:
c       ierr   I error flag
c     Inputs:
c       iounit I logical unit number of file to read
c       irec   I count of record for messages
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'embrdat.inc'
      include 'usrgrdem.inc'
      include 'iogrdem.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 iounit
      integer*4 irec
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 julcal
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   NMEDS   I    number of MEDS species to output
c
      integer*4 NMEDS 
c
      parameter( NMEDS = 8 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 scc
      character*4  sic
      character*3  basin
      integer*4    icell, jcell, iyear, imonth, iday, idatjul, idatcal
      integer*4    ibeghr, iendhr, icnty, istate, i
      real*4       medems(NMEDS), valmax
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      irec = irec + 1
c
c   --- set up some variables needed for MEDS ---
c
      icell = INT( ijcell / 1000 )
      jcell = MOD( ijcell, 1000 )
      idatjul = invbeg / 100
      idatcal = julcal( idatjul )
      iyear = idatcal / 10000
      idatcal = MOD( idatcal , 10000 )
      imonth = idatcal / 100
      iday = MOD( idatcal , 100 )
      ibeghr = MOD( invbeg , 100 )
      iendhr = MOD( invend , 100 )
      if( iendhr .EQ. 0 ) iendhr = 24
      basin = ' '
      read(fipin(1:2),'(I2)') istate 
      if( istate .GE. 90 ) then
         icnty = 99
      else
         read(fipin(4:5),'(I2)') icnty
      endif
      scc = scccd
      call lftjst( scc )
      sic = siccd
      call lftjst( sic )
c
c  --- fill the local array of emissions ---
c
      do i=1,NMEDS
         medems(i) = 0.
      enddo
      valmax = 0.
      do i=1,NMEDS
         if( imapmed(i) .GT. 0 ) medems(i) = 
     &                          spcout(imapmed(i)) * facmed(i)
         valmax = MAX( valmax,medems(i) )
      enddo
c
c   --- write the data stored in the common block for this record ---
c
      if( valmax .LT. 100000.0 ) then
         write(iounit,9000,ERR=7000) scen, sic, scc, icell, jcell,
     &                            iyear, imonth, iday, ibeghr, iendhr,
     &                            icnty, basin, (medems(i),i=1,NMEDS)
      else if( valmax .LT. 1000000.0 ) then
         write(iounit,9002,ERR=7000) scen, sic, scc, icell, jcell,
     &                            iyear, imonth, iday, ibeghr, iendhr,
     &                            icnty, basin, (medems(i),i=1,NMEDS)
      else
         write(iounit,9003,ERR=7000) scen, sic, scc, icell, jcell,
     &                            iyear, imonth, iday, ibeghr, iendhr,
     &                            icnty, basin, (medems(i),i=1,NMEDS)
      endif
c
c   --- set error flag to success ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,A)',ERR=9999)
     &                       'ERROR:  Writing output MEDS file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',oemsfl(:strmin(oemsfl))
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A8,A4,A8,2I3,5I2,13X,I3,A3,5X,8F10.3)
 9002 format(A8,A4,A8,2I3,5I2,13X,I3,A3,5X,8F10.2)
 9003 format(A8,A4,A8,2I3,5I2,13X,I3,A3,5X,8F10.1)
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
