dadosAtaque <- function(n){
	dados <- 3
	if(n < 4){
		dados <- n - 1
	}
	dados <- sample(6,dados,T)		
	return(sort(dados,T))
}

dadosDefensa <- function(n){
	dados <- n
	if(n > 3){
		dados <- 3
	}
	dados <- sample(6,dados,T)
	return(sort(dados,T))
}

atacar <- function(a,d,log=F){
	while(a > 1 && d > 0){
		ataque <- dadosAtaque(a)
		defensa <- dadosDefensa(d)
		for(i in 1:min(length(ataque),length(defensa))){
			if(ataque[i] > defensa[i]){
				d <- d-1 
			}
			else{
				a <- a-1
			}
		}
		if(log){
			cat("ataque:",ataque,"\n")
			cat("defensa:",defensa,"\n")
			cat("ejercitos atacante:",a,"\n")
			cat("ejercitos defensor:",d,"\n")
		}
	}
	return(a > d)
}

simularAtaque <- function(a,d,n){
	ganadas <- 0	
	for(i in 1:n){
		if(atacar(a,d)){
			ganadas <- ganadas+1
		}	
	}
	return(ganadas / n)
}
