# Modèle de feu de foret de Drossel et Schawbl (1992) 
#
# Territoire avec conditions aux frontières périodiques
# Voisinage de Moore
#
# 3 états possibles pour chaque cellule:
#  1: cellule occupée (Contient un arbre)
#  2: cellule en feu (l'arbre est en train de brûler)
#  0: cellule vide (l'arbre a été brûlé)
#
# Règles:
# On visite chaque cellule du paysage (dans un ordre aléatoire)
# 1) si une cellule est en feu elle devient vide
# 2) Si la cellules est un arbre, 
#    a) elle brûle avec probabilité 1 si au moins 1 de ses voisins est en feu
#    b) elle brûle avec probabilité f si aucun voisin n'est en feu
# 3) si la cellule est vide, elle devient un arbre avec probabilité p
##########################################################################
# inclure la librairie pracma
library(pracma)

# inclure le voisinage de Moore
source("Voisins_Moore.R")

# Les paramètres
D = 50    #La taille du territoire
TT = 1000  #Durée de la simulation
p = 0.05  #Probabilité de croissance 0.00005
f = 0.0005  #Probabilité de brûler spontanément

# Créer un territoire avec des proportions de cellules vides, occupees et brulees
a<-runif(D*D)
A<-matrix(a,D,D)
Terr<-matrix(0,D,D)
Terr[A<0.6]<-1               #choisir une proportion 0.6 de cellules occupees
Terr[(A>=0.6)&(A<0.7)]<-2    #choisir une proportion 0.1 de cellules en feu

# Creer une matrice temporaire pour sauvegarder l'etat du territoire
Terr_temp<-Terr

for(t in 1:TT){
  print(t)
  
  #decider d'un ordre aleatoire dans lequel on visitera les cellules
  #cet ordre change a toutes les iterations t 
  order_x <- randperm(1:D)
  order_y <- randperm(1:D)
  
  for(i in 1:D){
    x<- order_x[i]
    
      for(j in 1:D){
        y<- order_y[j]
      
        # Regle 1: si la cellule est en feu elle devient vide
        if(Terr[x,y]==2){
          Terr_temp[x,y]<-0
         }
        
        # Regle 2: si la cellule est occupee
        if(Terr[x,y]==1){
          
          #Determiner son voisinage
          v<-Voisins_Moore(Terr,x,y)
   
          #Determiner si des voisins sont en feu
          v_feu <- which(v==2, arr.ind=TRUE)
          n_feu <-dim(v_feu)[1]
                   
          if(n_feu>0){   #si au moins un voisin est en feu bruler la cellule (x,y)
            Terr_temp[x,y]<-2
          }
          else{         #si aucun voisin n'est en feu, bruler la cellule (x,y) avec un probabilite f
            if(runif(1)<=f){
              
              Terr_temp[x,y]<-2
              
            }
          }
          
        }
          
        # Regle 3: si la cellule est vide faire pousser un arbre avec une probabilite p
        if(Terr[x,y]==0){
          if(runif(1)<=p){
            Terr_temp[x,y]<-1
          } 
          
        }
        
       
    }#for y
  }# for x
  
  #Faire une mise a jour du territoire
  Terr<-Terr_temp
  
  couleur=c("blue","green","red")
  image(1:D, 1:D, Terr, asp=1, col=couleur, xlab="", ylab="", axes = FALSE)
  Sys.sleep(0.5) 
} #for t

