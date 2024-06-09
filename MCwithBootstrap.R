#Bootstrap can be used as a variance reduction method. See the following example of estimating the value of pi:

t=data.frame()
iter_boot=500
SampleSize=seq(10, 5000, by = 10)

for(ss in SampleSize){
  x=sapply(1:iter_boot, function(x){runif(ss)})
  y=sapply(1:iter_boot, function(x){runif(ss)})
  
  z=colMeans((x^2+y^2<=1))*4
  
  t=rbind(t, data.frame(l=last(z), m=mean(z)))
}

t %>%
  pivot_longer(cols = c(l, m), cols_vary="slowest", names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=rep(SampleSize,2), y=value, col=variable)) +
  geom_line() +
  labs(x = "SampleSize", y = "Estimated Value", title = expression("Convergence of Estimation of " * pi)) +
  scale_color_discrete(
    name = "Method of Estimation", 
    labels = c("l" = "Without Bootstrap", "m" = "With Bootstrap"))+
  annotate("text", x = 2500, y = 3.4, label = paste("Final Estimated Value with Bootstrap is", mean(t$m)), size = 5) +
  theme(plot.title = element_text(hjust = 0.5))