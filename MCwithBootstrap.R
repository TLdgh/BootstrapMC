#Bootstrap can be used as a variance reduction method. See the following example of estimating the value of pi:
library(tidyverse)

t=data.frame()
iter_boot=10000
SampleSize=seq(100, 10000, by = 250)

for(ss in SampleSize){
  x=runif(ss) 
  y=runif(ss)
  
  #non-parametric bootstrap sample with replacement:
  x_b=sapply(1:iter_boot, function(i){sample(x, size=ss, replace = TRUE)})
  y_b=sapply(1:iter_boot, function(i){sample(y, size=ss, replace = TRUE)})
  #for better result, do parametric bootstrap:
  #x_b=sapply(1:iter_boot, function(i){runif(ss)})
  #y_b=sapply(1:iter_boot, function(i){runif(ss)})
  
  #Monte Carlo integration:
  z=mean((x^2+y^2<=1))*4
  z_b=colMeans((x_b^2+y_b^2<=1))*4
  
  t=rbind(t, data.frame(l=z, m=mean(z_b)))
}

t %>%
  pivot_longer(cols = c(l, m), cols_vary="slowest", names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=rep(SampleSize,2), y=value, col=variable)) +
  geom_line() +
  labs(x = "SampleSize", y = "Estimated Value", title = expression("Convergence of Estimation of " * pi)) +
  scale_color_discrete(
    name = "Method of Estimation", 
    labels = c("l" = "Without Bootstrap", "m" = "With Bootstrap"))+
  annotate("text", x = median(SampleSize), y = 3.4, label = paste("Final Estimated Value with Bootstrap is", mean(t$m)), size = 5) +
  theme(plot.title = element_text(hjust = 0.5))