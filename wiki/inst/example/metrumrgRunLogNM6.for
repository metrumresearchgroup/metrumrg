C Generates a running table file to summarize the output of NONMEM runs.

      SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)
	  INTEGER ICALL,INDXS,NEWIND
      DOUBLE PRECISION THETA
      REAL DATREC
      DIMENSION THETA(*),DATREC(*),INDXS(*)
	  if (ICALL.EQ.3) CALL runlog(NEWIND)
      end
	  
      subroutine runlog(NEWIND)
      include '/common/NONMEM/nm6osx1/SIZES'
      common /cm1/ ntheta,nth
      common /cm2/ neta,neps,nre2
	  common /cm34/ ie
	  common /rocm6/ thetaf(lth),omegaf(lvr,lvr),sigmaf(lvr,lvr)
	  common /rocm7/ seth(lth),seom(lvr,lvr),sesig(lvr,lvr)
	  common /rocm9/ iere,ierc
      common /cm18/ spec
      common /cm19/ AVAIL(11)
      common /cm10/ s(lpar), n99, il, iu
      COMMON /CM12/ COV(LPAR3),INVCOV(LPAR3),STHTA(LTH),                                                                                             
     1              SEN(LVR,LVR),COR(LPAR3),NVLS,VLS(LPAR) 
      
	  common /rocm8/ object
      double precision s, object

      real*8 thetaf,omegaf,sigmaf
      real seth,seom,sesig 

      real sethl(lth),seoml(lvr,lvr),sesigl(lvr,lvr)
      CHARACTER(100) DESC
      integer unitp, ie, UNITO, IERE, IERC,J,I
c      integer lth,lvr,lvr2,lpar,lpar2,lpar3,mthvr,no,lnp4,lws1,ladd,
c     +        lsupp,lim1,lim2,lim3,lim4,lim5,lim6,lim7,lim8,lim11
     
c      integer maxids

      double precision ETA(lvr)
      integer MODE
      integer NTH,NETA,NEPS,NTH1,NETA1,NEPS1,NEIG

      unitp = 42
      UNITO = 43
C Assign number of theta,eta,omega that will be written to the file:
       NTH1 = ntheta
C       NTH1 = 10
       NOM1 = neta
C       NOM1 = 5
       NSI1 = neps
C       NSI1 = 4

C     This INFN provides var-cov results assuming full block omega and sigma
C     elements that are not estimated are output as 0.10000E+11
       NTC = ntheta
       NEC = neta
       NSC = neps
       NPAR = NTC+((NSC*NSC)-NSC)/2+NSC+((NEC*NEC)-NEC)/2+NEC
       NPR = ((NPAR*NPAR)-NPAR)/2+NPAR 
       OPEN(70,FILE='varcov.est')

       write(70,300) (COV(I),I=1,NPR)
  300  format(E12.6,",") 

       CLOSE(70)

C      IF (ICALL.EQ.3) THEN
       OPEN(50,FILE='cwtab1.est')
         MODE=0
         CALL PASS(MODE)
         MODE=1
         WRITE(50,*) 'ETAS'
   20      CALL PASS(MODE)
         IF (MODE.EQ.0) GO TO 30
         IF (NEWIND.NE.2) THEN
           CALL GETETA(ETA)
           WRITE (50,97) (ETA(I),I=1,NETA)
         ENDIF
         GO TO 20
   30    CONTINUE
         WRITE (50,*) 'THETAS'
         WRITE (50,99) (THETAF(J),J=1,NTH)
         WRITE(50,*) 'OMEGAS'
         DO 7000 I=1,NETA
 7000    WRITE (50,99) (OMEGAF(I,J),J=1,NETA)
         WRITE(50,*) 'SIGMAS'
         DO 7999 I=1,NEPS
 7999    WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)
         CLOSE(50)

   99    FORMAT (70E15.7)
   98    FORMAT (2I8)
   97    FORMAT (20E15.7)
   
       OPEN (UNITO, FILE='FCON2')
		CALL FILES(UNITO)
		READ (UNITO,34) DESC
          OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')
	    call files(unitp)
		write(unitp,35) DESC,IERE,IERC,object
   35 format(A100, ",,", I3, ",", I3,",",e15.7,",",$)


C write out theta estimates       
      do 770 i=1,NTH1
	   write(unitp,780)thetaf(i)
  770 continue	

  780	format(e12.4,",",$) 

C write out omega estimates
      do 771 i=1,NOM1
       do 771 j=1,i
           write(unitp,780) omegaf(i,j)
  771 continue

C write out sigma estimates
      do 772 i=1,NSI1
	       write(unitp,780) sigmaf(i,i)
C	  else
C	       write(unitp,781) sigmaf(i,i)
C	  endif	       
  772 continue	
  781	format(e12.4,",")
C   /)
C      CLOSE(unitp)
C  782 format(I1,",",$)
  
C write out eigenvalues
      NEIG = nvls
      do 872 i=1, NEIG
	  if(i.LT.NEIG) THEN
	       write(unitp,780) vls(i)
	  else
	       write(unitp,881) vls(i)
	  endif	       
  872 continue	
  881	format(e12.4,",")
      
      if(NEIG.GE.1) THEN
      CLOSE(unitp)
      else
      write(unitp,*) ','
      CLOSE(unitp)
      endif
  782 format(I1,",",$)  
	    
C Write standard errors of the parameters
      OPEN(unitp,FILE='NonmemRunLog.csv',ACCESS='append')
		write(unitp,36) DESC
   36 format(A100,",RSE,,,,",$)

C write out RSE's for theta estimates
      do 990 i=1,NTH1
	  if(seth(i).NE.0) THEN
                   write(unitp,780) abs(100*seth(i)/thetaf(i))
         else
                   write(unitp,782) 0
         endif
  990 continue	

C write out RSE's for omega estimates       
	  do 991 i=1,NOM1
	   do 991 j=1,i
	  if(omegaf(i,j).NE.0) THEN
                   write(unitp,780) abs(100*seom(i,j)/omegaf(i,j))
         else
                   write(unitp,782) 0
         endif
  991 continue

C write out RSE's for sigma estimates
      do 992 i=1,NSI1
	  if(i.LT.NSI1) THEN
	  if(sigmaf(i,i).NE.0) THEN
	         write(unitp,780) abs(100*sesig(i,i)/sigmaf(i,i))
         else
                   write(unitp,782) 0
         endif
      else
       if(sigmaf(i,i).NE.0) THEN
	         write(unitp,780) abs(100*sesig(i,i)/sigmaf(i,i))
         else
                   write(unitp,782) 0
         endif
      endif
  992 continue	
  
C write out eigenvalue ratio of largest to smallest value
C this works because NONMEM orders eigenvalues from smallest to largest 
C       write(unitp,999) 'Eigen. Ratio'
C  999 format(A15,",",$)
       if(NEIG.GE.1) THEN
       write(unitp,780) abs(vls(NEIG)/vls(1))
C       vls(NEIG)/vls(1))
C	   write(unitp, FMT='( /)') 
C	   CLOSE(unitp)
	   
C write out placeholder zero's below eigenvalues
      
      do 1002 i=1,NEIG-1
      write(unitp, 782) 0
 1002 continue
      else
       write(unitp,*) ','
      endif
	   write(unitp, FMT='( /)') 
	   CLOSE(unitp)
   
   34	FORMAT( / 8X, 1A)
C        ENDIF
       RETURN	
       END

