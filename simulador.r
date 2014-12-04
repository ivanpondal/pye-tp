dadosAtaque <- function(n){
	dados <- 3
	switch(n,
		'1'={
			dados <- 0
		},
		'2'={
			dados <- 1
		},
		'3'={
			dados <- 2
		}
		)
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

atacar <- function(a,d){
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
