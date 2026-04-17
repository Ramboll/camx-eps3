C**** PIGTHRESH.F version 2.0 09/01/2007
c
      subroutine pigthresh( )
c
c-----------------------------------------------------------------------
c
c    This routine walks though the list of stacks and finds the 
c    sources that are over any threshold.  If a mask file is used,
c    the identification of the source is based on the value in
c    the mask.  Otherwise it's based on the best match of county
c    code in the /THRESHOLD/ packet.
c
c    Argument Declaration.
c     Outputs:
c     Inputs:
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
      include 'iopigems.inc'
      include 'usrpigems.inc'
      include 'stkpigems.inc'
      include 'pigemstot.inc'
c
c-----------------------------------------------------------------------
c    Argument Declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    External Functions:
c-----------------------------------------------------------------------
c
      integer*4 fndmap
c
c-----------------------------------------------------------------------
c    Local Variables:
c-----------------------------------------------------------------------
c
      integer*4 icell, jcell, imapcd, idxpt, idxmap, ispc, i
      logical   lmaskit
c
c-----------------------------------------------------------------------
c   Entry Point:
c-----------------------------------------------------------------------
c
c  --- loop over all stacks ---
c
      do idxpt=1,nstklist
        idxpigcrt(idxpt) = IMISS
c
c   --- initialize the threshold code to missing ---
c
        imapcd = IMISS
        lmaskit = .FALSE.
c
c   --- if there is a mask file, get the threshold code from that ---
c
        if( lmaskfl ) then
            icell = INT( (xloclist(idxpt)/1000. - xorig) / dxcell ) + 1
            jcell = INT( (yloclist(idxpt)/1000. - yorig) / dycell ) + 1
            if( icell .GT. 0 .AND. icell .LE. nxcell .AND.
     &                  jcell .GT. 0 .AND. jcell .LE. nycell ) 
     &                                imapcd = imskval(icell,jcell)
            lmaskit = .TRUE.
        endif
c
c  --- we have to try PiG threshold for all species ---
c
        do 10 ispc=1,ncrthd
c
c  --- look for a match of this county in the map packet ---
c
           idxmap = fndmap( ctylist(idxpt), ispc )
           if( idxmap .LE. 0 ) goto 10
c
c  --- if the match is not a global, it overrides the 
c
           if( imapcd .LE. 0 ) then 
               imapcd = ithrshmap(idxmap)
           else
               if( thrshcty(idxmap) .NE. '00000' ) then
                   imapcd = ithrshmap(idxmap)
               else
                   if( lmaskit ) ntotstk(ifilestk(idxpt),ICDMSK) = 
     &                            ntotstk(ifilestk(idxpt),ICDMSK) + 1
               endif
           endif
c
c  --- if the threshold code is not found, skip this stack ---
c
          if( imapcd .LT. 0 ) goto 10
c
c  --- find this code in the array and compare the emissions value ---
c
          do i=1,nthresh
             if( imapcd .EQ. ithrshcd(i) .AND. 
     &                               idxthspc(i) .EQ. ispc) then
                 if( emsmax(idxpt,ispc) .GT. thrshval(i) ) then
                     lpiglist(idxpt) = .TRUE.
                     idxpigcrt(idxpt) = ispc
                 endif
                 if( idxpigcrt(idxpt) .LE. 0 ) idxpigcrt(idxpt) = ispc
             endif
          enddo
c
c  --- try the next pollutant ---
c
   10   continue
c
c  --- get the next stack ---
c
      enddo
      goto 9999
c
c-----------------------------------------------------------------------
c   Warning messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error Messages:
c-----------------------------------------------------------------------
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
