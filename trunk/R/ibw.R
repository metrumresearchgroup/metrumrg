`ibw` <-
function(ht,male,floor=FALSE){
#http://www.halls.md/ideal-weight/devine.htm
#men: Ideal Body Weight (in kilograms) = 50 + 2.3 kg per inch over 5 feet.
#women: Ideal Body Weight (in kilograms) = 45.5 + 2.3 kg per inch over 5 feet.
inches <- ht/2.54
if(floor) inches[inches < 60] <- 60
over <- inches - 60
intercept <- ifelse(male,50,45.5)
round(intercept + 2.3*over,1)
}
`bmi` <-
function(wt,ht)return(signif(wt/(ht/100)^2,3))
`bsa` <-
function(wt,ht)signif(exp(-3.751) * ht^0.422 * wt^0.515,3)
`crcl` <-
function(age,wt,male,scr){
clearance <- (140-age)*wt/(72*scr)#scr: mg/dL
clearance[!male] <- clearance[!male] * 0.85
return(signif(clearance,3))
}
`lbm` <-
function(wt,ht,male){
males <- signif(1.10 * wt - 128 * (wt^2/ht^2),3)
females <- signif(1.07 * wt - 148 * (wt^2/ht^2),3)
ifelse(male,males,females)
#http://www.halls.md/body-mass-index/leanbody.htm
#Lean Body Weight (men) = (1.10 x Weight(kg)) - 128 x ( Weight2/(100 x Height(m))2)
#Lean Body Weight (women) = (1.07 x Weight(kg)) - 148 x ( Weight2/(100 x Height(m))2)
}

