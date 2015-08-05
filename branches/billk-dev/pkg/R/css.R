`css` <-
function(cl,v,ka,tau,dose,time,...){
	ke <- ke(cl,v)
	dose*ka/(v*(ka-ke))*(exp(-ke*time)-exp(-ka*time))*acr(cl,v,tau)
}
`acr` <-
function(cl,v,tau,...)1/(1-exp(-ke(cl,v)*tau))
`auc` <-
function(cl,dose,...)dose/cl
`cavg` <-
function(cl,tau,dose,...)dose/cl/tau
`cmax` <-
function(cl,v,ka,tau,dose,...)css(cl,v,ka,tau,dose,tmax(cl,v,ka,tau))
`cmin` <-
function(cl,v,ka,tau,dose,...)css(cl,v,ka,tau,dose,tau)
`ke` <-
function(cl,v,...)cl/v
`tmax` <-
function(cl,v,ka,tau,...){
	ke <- ke(cl,v)
	log(
		(
			ka*
			(1-exp(-ke*tau))
		)/
		(
			ke*
			(1-exp(-ka*tau))
		)
	)/
	(ka-ke)
}

