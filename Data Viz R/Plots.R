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

ggplot(mpg) + 
  geom_smooth(mapping= aes(x=displ, y=hwy, linetype= drv)) 
#in this case there is a line with a variable ina  plot with other 2. It gives also the
# variability

############# YOU CAN SUPERIMPOSE ###########à
ggplot(mpg) + 
  geom_smooth(mapping= aes(x=displ, y=hwy))+
  geom_point(mapping = aes(x=displ, y=hwy))
# If we put the aes argument at the top I don't need to recall it
ggplot(mpg,mapping= aes(x=displ, y=hwy)) + 
  geom_smooth()+
  geom_point(mapping = aes(col= class))



