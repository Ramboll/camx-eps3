c*** CTLDSC.INC 
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    Include file for descrenary control factor applications:                   
c                                                                               
c-----------------------------------------------------------------------        
c    LOG:                                                                       
c-----------------------------------------------------------------------        
c                                                                               
c-----------------------------------------------------------------------        
c                  County                                                       
c-----------------------------------------------------------------------        
c                                                                               
c  Parameters:                                                                  
c                                                                               
c   MXCTY   I   maximum control strategy  by FIP/subrgn                         
c                                                                               
       integer*4 MXCTY                                                          
c                                                                               
       parameter ( MXCTY = 2400 )                                                
c                                                                               
c  Common block variables:                                                      
c                                                                               
c   ctyfac  R  control strategy code control factors                            
c   ctyfip  C  array for storing FIP/subrgn codes for control strategy          
c   ihrcty  I  beginning and ending hour for applying controls
c   ncdcty  I  number of input control strategy code controls                   
c                                                                               
       character*10 ctyfip(MXCTY)                                               
       integer*4    ihrcty(MXCTY,2)
       integer*4    ncdcty                                                      
       real*4       ctyfac(MXCRIT,MXCTY)                                        
c                                                                               
       common /chrcty/ ctyfip                                                   
       common /datcty/ ncdcty, ctyfac, ihrcty
c
c-----------------------------------------------------------------------
c                    Source Category
c-----------------------------------------------------------------------
c
c  Parameters:
c
c   MXSRC   I   Maximum source category control factors
c    
       integer*4 MXSRC
c       
       parameter ( MXSRC = 450000 )
c     
c  Common block variables:
c
c   ncdsrc  I  number of input subregion code controls
c   srcfac  R  source category code control factors
c   srcfip  C  Array for storing FIP/SUBRGN codes for subregion
c   srcplt  C  Array for storing plant/facility IDs
c   stksrc  C  Array for storing stack IDs
c   srcpnt  C  Array for storing point IDs
c   srcsic  C  Array for storing SIC code
c   srcscc  C  Array for story SCC(ASC) code
c   ihrsrc  I  beginning and ending hour for applying controls
c   ilstidx I  index of last match in arrays 
c
       character*10 srcfip(MXSRC)
       character*10 srcplt(MXSRC)
       character*10 srcpnt(MXSRC)
       character*10 stksrc(MXSRC)
       character*10 srcscc(MXSRC)
       character*4  srcsic(MXSRC)
       integer*4    ihrsrc(MXSRC,2)
       integer*4    ncdsrc
       integer*4    ilstidx
       real*4       srcfac(MXCRIT,MXSRC)
c
       common /chrsrc/ srcfip, srcplt, stksrc, srcpnt, srcscc, srcsic
       common /datsrc/ ncdsrc, srcfac, ihrsrc, ilstidx
c
c-----------------------------------------------------------------------
c    Variables for hash table arrays:
c-----------------------------------------------------------------------
c
c   idxhsh_src_first_pair I index of first record for this hash value
c   idxhsh_src_next_pair  I index of next record in the hash table
c
       integer*4 idxhsh_src_first_pair(MXHASH_PAIR)
       integer*4 idxhsh_src_next_pair(MXSRC)
c
      common /datsrchash/ idxhsh_src_first_pair, idxhsh_src_next_pair
