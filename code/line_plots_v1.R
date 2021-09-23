data(Orange)

#Simple Scatter Plot with colors for each tree

plot(Orange$age,Orange$circumference,xlab="Age (days)",ylab="Circumference (mm)",pch=19,col=Orange$Tree)
legend('topleft',sort(levels(Orange$Tree)),col=1:length(levels(Orange$Tree)),pch=19)



#GGPLOT solution from:
#https://lost-stats.github.io/Presentation/Figures/line_graphs.html
library(ggplot2) 

ggplot(Orange, aes(x = age, y = circumference, color = Tree)) +
  geom_line() +
  labs(x = "Age (days)", y = "Circumference (mm)", title = "Orange Tree Circumference Growth by Age") +
  theme(plot.title = element_text(hjust = 0.5))


#Trials with viridis

library(viridis)
tree.colors <- viridis(nlevels(Orange$Tree))

plot(x=Orange$age,y=Orange$circumference, type ='l',col=Orange$Tree)
legend('topleft',sort(levels(Orange$Tree)),col=1:length(levels(Orange$Tree)),pch=19)



