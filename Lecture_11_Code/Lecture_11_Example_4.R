### Odds ratio vs Risk Ratio ###

#Generate odds and risk ratios for a variety of probabilities
p1 = sample(seq(0,0.999,0.001))


p2 = sample(seq(0,0.999,0.001))

#Calculate risk ratio
RR = p2/p1

#Calculate odds ratio
OR = (p2/(1-p2)) / (p1/(1-p1))

Ratio = c(RR,OR)
Type = c(rep("RR",length(RR)),rep("OR",length(OR)))
df = data.frame(p1,p2,Ratio,Type)


# 3-D visualization of Odds vs Risk Ratio
require(plotly)

fig = plot_ly(df,x=~p1,y=~p2,z=~Ratio,color=~Type)
fig = fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'P1'),
                                   yaxis = list(title = 'P2'),
                                   zaxis = list(title = 'Ratio',range=c(0,20))))
fig

