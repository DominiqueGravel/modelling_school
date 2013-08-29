# Détermine la valeur des cellules contenues dans le voisinage 
# de Moore de rayon r autour de la cellule (x,y)
# d'un territoire A 
# avec conditions frontières périodiques
# Elise Filotas 02/03/2013
###########################################################################

Voisins_Moore<-function(A,x,y){
  
# Trouver la dimension de la matrice A  
  m<-dim(A)[1];      
  n<-dim(A)[2];

# Créer un vecteur vide
  v1<-c()                  

# Trouver les voisins
# Note %% est la fonction modulo a%%b = le reste après la division b/a
# Exemple: si (x,y)=(1,1) et que m=n=5  
# alors pour i=-1 et j=-1, on a :
# ((x-1+i)%%m)+1 = ((1-1-1)%%5)+1 = (-1%%5)+1=4+1=5
  
  for(i in -1:1)
    for(j in -1:1)
    {
      cx<-((x-1+i)%%m)+1
      cy<-((y-1+j)%%n)+1
      v1<- c(v1, A[cx,cy]);
    }

#Mettre le vecteur en format matriciel
  v2<-t(matrix(v1,3,3))
  
  return(v2)
}

