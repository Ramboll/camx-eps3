c*** IOSPCEMS.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for SPCEMS program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater conatining version of the
c                SPCEMS program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 SPCEMS module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORCNV   I   unit number for input pollutant conversion factors file
c   IOREMB   I   unit number for input EMBR file
c   IORINP   I   unit number for user input file
c   IORSPL   I   unit number for input splits file
c   IORXRF   I   unit number for SCC cross reference file
c   IOWEMB   I   unit number for output EMBR file
c   IOWERR   I   unit number for EMAR error file
c   IOWMSG   I   unit number for message file
c
      integer*4 IORCNV
      integer*4 IOREMB
      integer*4 IORINP
      integer*4 IORSPL
      integer*4 IORXRF
      integer*4 IOWEMB
      integer*4 IOWERR
      integer*4 IOWMSG
c
      parameter ( IORINP = 10 )
      parameter ( IOREMB = 11 )
      parameter ( IORSPL = 12 )
      parameter ( IORXRF = 13 )
      parameter ( IORCNV = 14 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWEMB = 21 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   cnvfl    C   pollutant conversion factors filename
c   errfl    C   EMAR error filename
c   infl     C   input EMBR filename
c   msgfl    C   message filename
c   outfl    C   output EMBR filename
c   spltfl   C   input splits filename
c   uinpfl   C   user input filename
c   xrffl    C   scc cross reference filename
c
      character*(MXSTR) cnvfl
      character*(MXSTR) errfl
      character*(MXSTR) infl
      character*(MXSTR) msgfl
      character*(MXSTR) outfl
      character*(MXSTR) spltfl
      character*(MXSTR) uinpfl
      character*(MXSTR) xrffl
c
      common /iofil/ uinpfl, infl, spltfl, xrffl, cnvfl,
     &               msgfl, outfl, errfl
c
c-----------------------------------------------------------------------
c   Flags to indicate if files are used:
c-----------------------------------------------------------------------
c
c   lcnvfl   L  .TRUE. if pollutant conversion factors file is provided
c
      logical*4 lcnvfl
c
      common /ioflg/ lcnvfl
