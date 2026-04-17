c**** IOCNTLEM.INC  
c                                                                               
c-----------------------------------------------------------------------        
c                                                                               
c    Include file for I/O parameters and variables for CNTLEM program           
c                                                                               
c-----------------------------------------------------------------------        
c    Log:                                                                       
c-----------------------------------------------------------------------        
c                                                                               
c-----------------------------------------------------------------------        
c    Program version parameter:                                                 
c-----------------------------------------------------------------------        
c                                                                               
c   VERSON   C   character string parameter containing version of the           
c                CNTLEM program                                                 
c                                                                               
      character*40 VERSON                                                       
c                                                                               
      parameter ( VERSON = 'EPS3 CNTLEM module v. 3.40 Apr 2022' )
c                                                                               
c-----------------------------------------------------------------------        
c    I/O unit  parameters                                                       
c-----------------------------------------------------------------------        
c                                                                               
c   IORINP   I   unit number for user input file                                
c   IORMBR   I   unit number for input EMBR file                                
c   IORCTL   I   unit number for control factors file                           
c   IOWMSG   I   unit number for message file                                   
c   IOWMBR   I   unit number for output EMBR file                               
c                                                                               
      integer*4 IORINP                                                          
      integer*4 IORMBR                                                          
      integer*4 IORCTL                                                          
      integer*4 IOWMSG                                                          
      integer*4 IOWMBR                                                          
      integer*4 IOWERR                                                          
c                                                                               
      parameter ( IORINP = 10 )                                                 
      parameter ( IORMBR = 11 )                                                 
      parameter ( IORCTL = 12 )                                                 
      parameter ( IOWMSG = 20 )                                                 
      parameter ( IOWMBR = 21 )                                                 
      parameter ( IOWERR = 30 )                                                 
c                                                                               
c-----------------------------------------------------------------------        
c   I/O filenames:                                                              
c-----------------------------------------------------------------------        
c                                                                               
c   uinpfl   C   user input filename                                            
c   imbrfl   C   input EMBR filename                                            
c   ctlfl    C   control factors filename                                       
c   msgfl    C   message filename                                               
c   ombrfl   C   output EMBR filename                                           
c                                                                               
      character*(MXSTR) uinpfl                                                  
      character*(MXSTR) imbrfl                                                  
      character*(MXSTR) ctlfl                                                   
      character*(MXSTR) msgfl                                                   
      character*(MXSTR) ombrfl                                                  
c                                                                               
      common /iofil/ uinpfl, imbrfl, ctlfl,
     &               msgfl,  ombrfl
