Proportional <-
function(E,C,Names=NULL){
	
lambda<-E/sum(C)
R<-lambda*C

return(list(Results=R,Claims=C,Lambda=lambda,Method="Proportional Rule")) 	
		
}
