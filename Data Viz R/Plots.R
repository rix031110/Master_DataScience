library(ggplot2)
ds= data(mpg)
mpg
ggplot(mpg) + #it ccreates a hollow plot
  geom_point(mapping = aes(x=displ, y=hwy)) #type of plot wanted, aes is the estetic part
                                            #with variables
ggplot(mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color=class)) #the collor correspond to the class

ggplot(mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, shape=drv)) #the shape correspond to the drv

prova,2
