
## Simple Scatter Plot with colors for each tree
data(Orange)

plot(Orange$age,Orange$circumference,xlab="Age (days)",ylab="Circumference (mm)",pch=19,col=Orange$Tree)

legend('topleft',sort(levels(Orange$Tree)),col=1:length(levels(Orange$Tree)),pch=19)


## Trials with viridis

library(viridis)

Orange$Tree <- factor(Orange$Tree, levels=seq(1,nlevels(Orange$Tree)),ordered=T)
tree.colors <- viridis(nlevels(Orange$Tree))

plot(x=Orange$age,y=Orange$circumference, type ='p',pch=19, col=tree.colors[Orange$Tree])

legend('topleft',legend=levels(Orange$Tree),col=tree.colors,pch=19)


## GGPLOT solution from:
library(ggplot2) 

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_line() + labs(x = "Age (days)", y = "Circumference (mm)", title = "Orange Tree Circumference Growth by Age") +
  theme(plot.title = element_text(hjust = 0.5))




