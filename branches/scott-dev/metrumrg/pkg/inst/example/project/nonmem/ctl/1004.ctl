$PROB 1004 phase1 2 CMT like 1003 but better bounds
$INPUT C ID TIME SEQ=DROP EVID AMT DV
$DATA ../../data/derived/phase1.csv IGNORE=C
$SUBROUTINE ADVAN4 TRANS4
$PK
 CL=THETA(1)*EXP(ETA(1))
 V2 =THETA(2)*EXP(ETA(2))
 KA=THETA(3)*EXP(ETA(3))
 Q  =THETA(4);*EXP(ETA(4))
 V3=THETA(5); *EXP(ETA(5))
 S2=V2
 
$ERROR
 Y=F*EXP(ERR(1)); + ERR(2)
 IPRE=F

$THETA 
(0,10,50)     ;CL
(0,10,100)      ;V
(0,0.2, 5)     ;KA
(0,10,50)      ;Q
(0,10,1000)      ;V3

$OMEGA 0.09 0.09 0.09 ;0.09 0.09 
$SIGMA 0.09 ;0.1
$ESTIMATION MAXEVAL=9999 PRINT=5 NOABORT METHOD=1 INTER MSFO=./1004.msf
$COV PRINT=E
$TABLE NOPRINT FILE=./1004.tab ONEHEADER ID AMT TIME EVID PRED IPRE CWRES
$TABLE NOPRINT FILE=./1004par.tab ONEHEADER ID TIME CL Q V2 V3 KA ETA1 ETA2 ETA3
