C**** WRESPL.F 
c
      subroutine wrespl( ierr )
c
c-----------------------------------------------------------------------
c
c    Writes the new splits file.  The species order is the requested
c    order.
c
c    Argument Declaration.
c     Inputs:
c     Outputs:
c       ierr  I error flag
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c  Apr 2014 included the additional output option for SPCEMS input;
c           gspro and gscnv smoke formatted files
c-----------------------------------------------------------------------
c    Include Files:
c-----------------------------------------------------------------------
c
      include 'param.inc'
      include 'ioemscvt.inc'
      include 'emcvtspl.inc'
      include 'usremcvt.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer*4 strmin
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j
      character*1 COMMA
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c   --- set error flag ---
c
      ierr = IFAIL
      COMMA = ','

c
c  --- CHMSPL input format - write species information ---
c
      write(IOWSPL,'(I5)',ERR=7000) nspecs
      do i=1,nspecs
          write(IOWSPL,'(A10,1X,F5.1,1X,A5)',ERR=7000) spclst(i), 
     &                                          molwt(i), spcrit(i)
      enddo
c
c  --- write the splits values ---
c
      do i=1,nfac
         write(IOWSPL,'(A,300(:,E10.3))',ERR=7000) prfspl(i),
     &                                   (spltab(i,j),j=1,nspecs)
      enddo

c
c  --- SPCEMS input format - gspro formatted file

      do i=1,nfac
         do j = 1, nspecs

            if ( spltab(i,j) .GT. 0.0 ) then
              write(IOWPRO,'(2A,"TOG",3A,3(E10.3,A))',ERR=7002) 
     &                                     prfspl(i),COMMA,
     &                               COMMA,spclst(j),COMMA,
     &                                     spltab(i,j),COMMA,
     &                                     1.0, COMMA,
     &                                     spltab(i,j), COMMA
            endif
         enddo
      enddo

c  --- gscnv formatted file
       do i = 1, nfac
          write (IOWCNV,'("VOC,TOG,",2A,F10.1)',ERR = 7003) 
     &                              prfspl(i), COMMA, 1.0
       enddo
c
c  --- write count to message file ---
c
      write(IOWMSG,'(/,A,I8)',ERR=7001)
     &           'Number of profiles written to splits file:', nfac
c
c  --- set error flag to success ----
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing the output split',
     &                                                 ' factors file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',osplfl(:strmin(osplfl))
      goto 9999
c
 7001 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing message file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',msgfl(:strmin(msgfl))
      goto 9999
c
 7002 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing the GSPRO file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',ogspfl(:strmin(ogspfl))
      goto 9999
c
 7003 continue
      ioerr = IOWSTD
      write(ioerr,'(/,2A)',ERR=9999) 'ERROR: Writing the GSCNV file.'
      write(ioerr,'(2A,//)',ERR=9999) 'File: ',ogscfl(:strmin(ogscfl))
      goto 9999
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
