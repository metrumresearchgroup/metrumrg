C Generates a running table file to summarize the output of NONMEM runs.

      SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)
      INTEGER ICALL,INDXS,NEWIND
      DOUBLE PRECISION THETA
      REAL DATREC
      DIMENSION THETA(*),DATREC(*),INDXS(*)
      include '/path/to/nonmem/SIZES'
      common /cm1/ ntheta,nth
      common /cm2/ neta,neps,nre2
	common /cm34/ ie
	common /rocm6/ thetaf(lth),omegaf(lvr,lvr),sigmaf(lvr,lvr)
	common /rocm7/ seth(lth),seom(lvr,lvr),sesig(lvr,lvr)
	common /rocm9/ iere,ierc
      common /cm18/ spec
      COMMON /CM19/ AVAIL(11)
      common /cm10/ s(lpar), n99, il, iu
	common /rocm8/ object
      double precision s, object

 	real*8 thetaf,omegaf,sigmaf
	real seth,seom,sesig
	real sethl(lth),seoml(lvr,lvr),sesigl(lvr,lvr)
	CHARACTER(100) DESC
	integer unitp, ie, UNITO, IERE, IERC
        unitp=42
	UNITO=43

	  IF(ICALL.EQ.3)THEN 
	    OPEN (UNITO, FILE='FCON2')
		CALL FILES(UNITO)
		READ (UNITO,34) DESC
          OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')
	    call files(unitp)
		write(unitp,35) DESC,IERE,IERC,object
   35 format(/ A100, ",,", I3, ",", I3,",",e15.6,",",$)

C Assign number of theta,eta,omega that will be written to the file:
       NTH = ntheta
C       NTH = 13
       NOM = neta
C       NOM = 6
       NSI = neps
C       NSI = 2

C write out theta estimates       
      do 770 i=1,NTH
	   write(unitp,780)thetaf(i)
  770 continue	

  780	format($,e12.4,",") 
  781   format($,e12.4,/)

C write out omega estimates
      do 771 i=1,NOM
       do 771 j=1,i
           write(unitp,780) omegaf(i,j)
  771 continue

C write out sigma estimates
      do 772 i=1,NSI
      if(i.NE.NSI) THEN
	   write(unitp,780) sigmaf(i,i)
	   ELSE
	   write(unitp,781) sigmaf(i,i)
	   endif 	    
  772 continue	

      CLOSE(unitp)
	    
C Write standard errors of the parameters
C      OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')
C		call files(unitp)
C		write(unitp,36) DESC
C  36 format(/ A100,",RSE,,,,",$)

C write out RSE's for theta estimates
C      do 990 i=1,NTH
C	 if(seth(i).NE.0) THEN
C                  write(unitp,780) 100*seth(i)/thetaf(i)
C       else
C                   write(unitp,780) 0
C         endif
C  990 continue	

C write out RSE's for omega estimates       
C	do 991 i=1,NOM
C	   do 991 j=1,i
C	 if(omegaf(i,j).NE.0) THEN
C                   write(unitp,780) 100*seom(i,j)/omegaf(i,j)
C         else
C                   write(unitp,780) 0
C         endif
C  991 continue

C write out RSE's for sigma estimates
C      do 992 i=1,NSI
C	 if(sigmaf(i,i).NE.0) THEN
C	         write(unitp,780) 100*sesig(i,i)/sigmaf(i,i)
C         else
C                   write(unitp,780) 0
C        endif
C  992 continue	
       
C	CLOSE(unitp)
  
   34	FORMAT( / 8X, 1A)
        ENDIF
      RETURN	
      END

