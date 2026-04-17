c*** IOTMPFAC.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for TMPFAC program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Parameter for program version number:
c-----------------------------------------------------------------------
c
c   VERSON   C   program version number
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 TMPFAC utility v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORAFS   I   unit number for input AFS file
c   IORPRF   I   unit number for input temporal profiles file
c   IORXRF   I   unit number for input XREF file
c   IOWMSG   I   unit number for message file
c   IOWPRF   I   unit number for output temporal profiles file
c   IOWXRF   I   unit number for output XREF file
c
      integer*4 IORAFS
      integer*4 IORPRF
      integer*4 IORXRF
      integer*4 IOWMSG
      integer*4 IOWPRF
      integer*4 IOWXRF
c
      parameter ( IORXRF = 10 )
      parameter ( IORPRF = 11 )
      parameter ( IORAFS = 12 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWXRF = 21 )
      parameter ( IOWPRF = 22 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   afsfl    C   input AFS filename
c   inprfl   C   input temporal profiles filename
c   inxrfl   C   input XREF filename
c   msgfl    C   message output filename
c   oprfl    C   output temporal filename
c   oxrfl    C   output XREF filename
c
      character*(MXSTR) afsfl
      character*(MXSTR) inprfl
      character*(MXSTR) inxrfl
      character*(MXSTR) msgfl
      character*(MXSTR) oprfl
      character*(MXSTR) oxrfl
c
      common /iofil/ inxrfl, inprfl, afsfl, msgfl, oxrfl, oprfl
c
c-----------------------------------------------------------------------
c    Parameters for index of days of week in arrays:
c-----------------------------------------------------------------------
c
      integer*4   IDXMON
      integer*4   IDXTUE
      integer*4   IDXWED
      integer*4   IDXTHU
      integer*4   IDXFRI
      integer*4   IDXSAT
      integer*4   IDXSUN
c
      parameter ( IDXMON = 1 )
      parameter ( IDXTUE = 2 )
      parameter ( IDXWED = 3 )
      parameter ( IDXTHU = 4 )
      parameter ( IDXFRI = 5 )
      parameter ( IDXSAT = 6 )
      parameter ( IDXSUN = 7 )
c
c-----------------------------------------------------------------------
c   Other variables:
c-----------------------------------------------------------------------
c
c   lafs   L    true if using a AFS file
c   lprof  L    true if using an existing temporal profiles file
c   lxref  L    true if using an existing XREF file
c
      logical lxref, lprof, lafs
c
      common /facdat/ lxref, lprof, lafs
