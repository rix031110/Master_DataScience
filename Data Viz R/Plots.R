library(ggplot2)
ds= data(mpg)
mpg
ggplot(mpg) + #it ccreates a hollow plot
  geom_point(mapping = aes(x=displ, y=hwy)) #type of plot wanted, aes is the estetic part
                                            #with variables

