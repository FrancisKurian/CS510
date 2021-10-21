
## Simple Scatter Plot with colors for each tree

library(viridis)
Orange$Tree <- factor(Orange$Tree, levels=seq(1,nlevels(Orange$Tree)),ordered=T)
tree.colors <- viridis(nlevels(Orange$Tree))

plot(x=Orange$age,y=Orange$circumference, 
     type ='p',pch=19,
     col=tree.colors[Orange$Tree], xlab='Age (days)', ylab='Circumference (mm)')
legend('topleft',legend=levels(Orange$Tree),col=tree.colors,pch=19)


## Generic Line Plot using Viridis Color Maps

#library(viridis)
#Orange$Tree <- factor(Orange$Tree, levels=seq(1,nlevels(Orange$Tree)),ordered=T)
#tree.colors <- viridis(nlevels(Orange$Tree))

tree.shapes <- as.numeric(levels(Orange$Tree))

plot(Orange$age,Orange$circumference,type='n',main = "Trees:Growth by Age", 
     xlim = range(Orange$age), ylim = range(Orange$circumference), 
     xlab='Age (days)', ylab='Circumference (mm)')

for (i in tree.shapes) {
  newdata <- subset(Orange, Tree==i)
  lines(x=newdata$age,y=newdata$circumference, type ='o',pch=tree.shapes[i], col=tree.colors[i])
}

legend('topleft',legend=levels(Orange$Tree),col=tree.colors,pch=tree.shapes)



## GGPLOT solution from:
library(ggplot2) 

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) + 
  geom_line() + labs(x = "Age (days)", y = "Circumference (mm)", title = "Orange Tree Circumference Growth by Age") +
  theme(plot.title = element_text(hjust = 0.5)) +

