c*** WGTFRAC.INC
c
c-----------------------------------------------------------------------
c
c    Include file for the weight fraction data used by the SPLTEM module
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     08/12/97  --mmj--   original development
c
c-----------------------------------------------------------------------
c   list of all saroad codes in weight fraction data:
c   and species names and reactivity flag from compound database
c-----------------------------------------------------------------------
c
c   srdnam   C   list of corresponding species/compound names
c   srdlng   C   list of corresponding species/compound names (optional full name)
c   isrdcnt  I   count of saroad code entries
c   isrdlst  I   list of saroad codes
c   lreact   L   list of corresponding reactivity flags
c
      character*20      srdnam(MXSRD)
      character*(MXSTR) srdlng(MXSRD)
      integer*4         isrdcnt
      integer*4         isrdlst(MXSRD)
      logical*4         lreact(MXSRD)
c
      common /srdchr/ srdnam, srdlng
      common /srddat/ isrdcnt, isrdlst, lreact
c
c-----------------------------------------------------------------------
c   profile weight data:
c-----------------------------------------------------------------------
c
c   isrdwf   I   list of saroad codes in each profile 
c   nprfwf   I   number of profile codes
c   nwtswf   I   number of chemicals making up a profile
c   profwf   C   list of profile codes
c   wgtswf   R   weight fraction of chemical in profile
c
      character*(LENPRF) profwf(MXPRF)
      integer*4          isrdwf(MXPRF,MXWGT)
      integer*4          nprfwf
      integer*4          nwtswf(MXPRF)
      real*4             wgtswf(MXPRF,MXWGT)
c
      common /wgtchr/ profwf
      common /wgtdat/ nprfwf, isrdwf, nwtswf, wgtswf
