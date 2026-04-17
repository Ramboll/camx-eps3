c*** IOGRDEM.INC 
c
c-----------------------------------------------------------------------
c
c    Include file for I/O parameters and variables for GRDEM program
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c   jun 2008  mjimenez   Added variables for optional output
c                        gridded surg report file
c-----------------------------------------------------------------------
c    Program version parameter:
c-----------------------------------------------------------------------
c
c   VERSON   C   character string paramater conatining version of the
c                grdem program
c
      character*40 VERSON
c
      parameter ( VERSON = 'EPS3 GRDEM module v. 3.40 Apr 2022' )
c
c-----------------------------------------------------------------------
c    I/O unit  parameters
c-----------------------------------------------------------------------
c
c   IORINP   I   unit number for user input file
c   IORLNK   I   unit number for link definition file
c   IORMBR   I   unit number for input EMBR file
c   IORSRG   I   unit number for gridded surrogate file
c   IORXRF   I   unit number for surrogate cross reference file
c   IOWEMS   I   unit number for output emissions file
c   IOWASC   I   unit number for ASC/FIPs emissions file
c   IOWERR   I   unit number for EMAR error file
c   IOWMSG   I   unit number for message file
c   IOWSRG   I   unit number for output surrogate report file
c
      integer*4 IORINP
      integer*4 IORLNK
      integer*4 IORMBR
      integer*4 IORSRG
      integer*4 IORXRF
      integer*4 IOWEMS
      integer*4 IOWASC
      integer*4 IOWERR
      integer*4 IOWMSG
      integer*4 IOWSRG
c
      parameter ( IORINP = 10 )
      parameter ( IORMBR = 11 )
      parameter ( IORXRF = 12 )
      parameter ( IORSRG = 13 )
      parameter ( IORLNK = 14 )
      parameter ( IOWMSG = 20 )
      parameter ( IOWEMS = 21 )
      parameter ( IOWASC = 22 )
      parameter ( IOWSRG = 23 )
      parameter ( IOWERR = 30 )
c
c-----------------------------------------------------------------------
c   I/O filenames:
c-----------------------------------------------------------------------
c
c   errfl    C   EMAR error filename
c   ascfl    C   ASC/FIPS emissions filename
c   imbrfl   C   input EMBR filename
c   lnkfl    C   link definition filename
c   msgfl    C   message filename
c   oemsfl   C   output EMBR filename
c   oncffl   C   output gridded NetCDF filename
c   srgfl    C   gridded surrogate filename
c   srptfl   C   output surrogate report filename
c   uinpfl   C   user input filename
c   xrffl    C   SCC cross reference filename
c
      character*(MXSTR) errfl
      character*(MXSTR) ascfl
      character*(MXSTR) imbrfl
      character*(MXSTR) lnkfl
      character*(MXSTR) msgfl
      character*(MXSTR) oemsfl
      character*(MXSTR) oncffl
      character*(MXSTR) srgfl
      character*(MXSTR) srptfl
      character*(MXSTR) uinpfl
      character*(MXSTR) xrffl
c
      common /iofil/ uinpfl, imbrfl, xrffl, srgfl, lnkfl, msgfl, ascfl,
     &               oemsfl, oncffl, errfl, srptfl
c
c-----------------------------------------------------------------------
c    Flags for files not used
c-----------------------------------------------------------------------
c
c   lnkdef        L   flag for link definition data provided
c   lsurg         L   flag for surrogate data provided
c   lascfl        L   flag for output FIPS/ASC emissions file requested
c   lsrpt         L   flag for output surrogate report file requested
c   lscsv         L   flag for output surrogate report file requested as CSV
c   loutfile      L   flag for legacy output file requested as CSV
c   lnetcdf_out   L   flag for output urrogate report file requested as CSV
c   ncf_iowemis   I   file handle for output NetCDF file
c
      logical*4 lnkdef
      logical*4 lsurg
      logical*4 lascfl
      logical*4 lsrpt
      logical*4 lscsv
      logical*4 loutfile
      logical*4 lnetcdf_out
      integer*4 ncf_iowemis
c
      common /usedat/ lsurg, lnkdef, lascfl, lsrpt, lscsv, loutfile, 
     &                lnetcdf_out, ncf_iowemis
