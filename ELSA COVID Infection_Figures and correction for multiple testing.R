#######################################################################################
######################### PLOTS #######################################################
#######################################################################################

### Plot of main effects of covid infection  

main<-read_excel("Figure1.xlsx", sheet=1)

#column names 
names(main)[2]<-"Phenotype_names"
names(main)[4]<-"Coefficient"
names(main)[6]<-"lower_CI"
names(main)[7]<-"upper_CI"


#rename
allModelFrame <- main

## create new variables
allModelFrame$Coefficient1<-round(allModelFrame$Coefficient,2)

# order pheno names
allModelFrame$Phenotype_names1<- factor (as.factor(allModelFrame$Phenotype_names), 
                                         levels = c("Total score, Nov-Dec 2020 (b)",
                                                    "Total score, June-July 2020 (b)",
                                                    "Friends, Nov-Dec 2020 (OR)",
                                                    "Friends, June-July 2020 (OR)",
                                                    "Family, Nov-Dec 2020 (OR)",
                                                    "Family, June-July 2020 (OR)",
                                                    "Worse off, Nov-Dec 2020 (OR)" ,
                                                    "Worse off, June-July 2020 (OR)",
                                                    "Worried, Nov-Dec 2020 (OR)",
                                                    "Worried, June-July 2020 (OR)", 
                                                    "Loneliness, Nov-Dec 2020 (b)",
                                                    "Loneliness, June-July 2020 (b)",
                                                    "Poor QoL, Nov-Dec 2020 (b)",
                                                    "Poor QoL, June-July 2020 (b)",
                                                    "Anxiety, Nov-Dec 2020 (OR)",
                                                    "Anxiety, June-July 2020 (OR)",
                                                    "Depression, Nov-Dec 2020 (OR)",
                                                    "Depression, June-July 2020 (OR)"))
table(allModelFrame$Phenotype_names1)

# Plot results
new_plot <- ggplot(allModelFrame, aes(shape= Model,col=Model)) +
  scale_color_manual(values = c("#277F8EFF","#440154FF")) 

new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names1, ymin = lower_CI,
                                          ymax = upper_CI),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names1, y = Coefficient, ymin = lower_CI,
                                           ymax = upper_CI),
                                       lwd = 1/2, position = position_dodge(width = 1/2)) 
new_plot <- new_plot + theme(axis.text.x = element_text(size=12, face="bold", color="black" ),axis.text.y = element_text(size=12, face="bold", color="black"))

new_plot <- new_plot  + theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +  coord_flip() + scale_shape_discrete(name  ="")  +geom_hline(yintercept=0,linetype=2) +geom_hline(yintercept=1,linetype=2) + scale_y_continuous(limits = c(-0.8, 2.6))


fig_all<- new_plot + theme_minimal() + theme(legend.position="bottom") + ggtitle("") + theme(plot.title = element_text(hjust =1, size = 12,face = "bold")) + theme(legend.title = element_blank())

print(fig_all)


### labs

labd <- data.frame(V0 = factor(c("A", "B","C","D","E","F","G", "H","I", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "Z"), levels=c("Z","V","U", "T", "S","R","Q", "P","O", "N","M","L", "I","H","G","F","E","D","C","B", "A")),
                   V05 = rep(c(3),each=21),
                   V1 = c( "Mental Health","","", "","","","","","","Financial Hardship","","","","","Infrequent Social Contact","","", "","", "",""))


data_tabled <- ggplot(labd, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size=3.2, hjust=0, vjust=-1, fontface='bold') + theme_bw() +
  # geom_segment(aes(x=3,xend=5, y=14.5, yend=14.5)) + 
  #facet_wrap(~ title, scales = "free_x") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),#element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"), #element_blank(),
        plot.margin = unit(c(-0.25,0,-0.5,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,5), ylim =c(1,22), expand = TRUE) 
data_tabled



library(cowplot)
ggdraw() +
  draw_plot(fig_all, x = 0.25, y = 0, width = 0.69, height = 1) +
  draw_plot(data_tabled, x=-0.23, y=.1,width = 0.5, height = .89)

ggsave("Figure1b.jpeg", device = "jpeg", limitsize = T)

ggsave("Figure1b.jpeg", device = "jpeg",dpi = 300,width =12, height =8, limitsize = T)

## plot without IPTW estimates 
main<-read_excel("Figure1.xlsx", sheet=1)
main<-main[main$Model != "Propensity score weighting",]


str(main$Model)
#column names 
names(main)[2]<-"Phenotype_names"
names(main)[4]<-"Coefficient"
names(main)[6]<-"lower_CI"
names(main)[7]<-"upper_CI"


#rename
allModelFrame <- main

## create new variables
allModelFrame$Coefficient1<-round(allModelFrame$Coefficient,2)

# order pheno names
allModelFrame$Phenotype_names1<- factor (as.factor(allModelFrame$Phenotype_names), 
                                         levels = c("Total score, Nov-Dec 2020 (b)",
                                                    "Total score, June-July 2020 (b)",
                                                    "Friends, Nov-Dec 2020 (OR)",
                                                    "Friends, June-July 2020 (OR)",
                                                    "Family, Nov-Dec 2020 (OR)",
                                                    "Family, June-July 2020 (OR)",
                                                    "Worse off, Nov-Dec 2020 (OR)" ,
                                                    "Worse off, June-July 2020 (OR)",
                                                    "Worried, Nov-Dec 2020 (OR)",
                                                    "Worried, June-July 2020 (OR)", 
                                                    "Loneliness, Nov-Dec 2020 (b)",
                                                    "Loneliness, June-July 2020 (b)",
                                                    "Poor QoL, Nov-Dec 2020 (b)",
                                                    "Poor QoL, June-July 2020 (b)",
                                                    "Anxiety, Nov-Dec 2020 (OR)",
                                                    "Anxiety, June-July 2020 (OR)",
                                                    "Depression, Nov-Dec 2020 (OR)",
                                                    "Depression, June-July 2020 (OR)"))
table(allModelFrame$Phenotype_names1)

allModelFrame$Model

# Plot results
new_plot <- ggplot(allModelFrame, aes(shape= Model,col=Model)) +
  scale_color_manual(values = c("#277F8EFF","#440154FF")) 

new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names1, ymin = lower_CI,
                                          ymax = upper_CI),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names1, y = Coefficient, ymin = lower_CI,
                                           ymax = upper_CI),
                                       lwd = 1/2, position = position_dodge(width = 1/2)) 
new_plot <- new_plot + theme(axis.text.x = element_text(size=12, face="bold", color="black" ),axis.text.y = element_text(size=12, face="bold", color="black"))

new_plot <- new_plot  + theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +  coord_flip() + scale_shape_discrete(name  ="")  +geom_hline(yintercept=0,linetype=2) +geom_hline(yintercept=1,linetype=2) + scale_y_continuous(limits = c(-0.8, 2.6))


fig_all<- new_plot + theme_minimal() + theme(legend.position="bottom") + ggtitle("") + theme(plot.title = element_text(hjust =1, size = 12,face = "bold")) + theme(legend.title = element_blank())

print(fig_all)


### labs

labd <- data.frame(V0 = factor(c("A", "B","C","D","E","F","G", "H","I", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "Z"), levels=c("Z","V","U", "T", "S","R","Q", "P","O", "N","M","L", "I","H","G","F","E","D","C","B", "A")),
                   V05 = rep(c(3),each=21),
                   V1 = c( "Mental Health","","", "","","","","","","Financial Hardship","","","","","Infrequent Social Contact","","", "","", "",""))


data_tabled <- ggplot(labd, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size=3.2, hjust=0, vjust=-1, fontface='bold') + theme_bw() +
  # geom_segment(aes(x=3,xend=5, y=14.5, yend=14.5)) + 
  #facet_wrap(~ title, scales = "free_x") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),#element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"), #element_blank(),
        plot.margin = unit(c(-0.25,0,-0.5,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,5), ylim =c(1,22), expand = TRUE) 
data_tabled



library(cowplot)
ggdraw() +
  draw_plot(fig_all, x = 0.25, y = 0, width = 0.7, height = 1) +
  draw_plot(data_tabled, x=-0.24, y=.1,width = 0.5, height = .88)


ggsave("Figure1c.jpeg", device = "jpeg",dpi = 300,width =12, height =8, limitsize = F)
ggsave("Figure1c.jpeg", device = "jpeg",dpi = 300, limitsize = F)
ggsave("Figure1c.eps", device = "eps",dpi = 300,width =12, height =8, limitsize = F)

### estimated trajectories for change 

traj<-read_excel("s2_predicted values.traj.xls", sheet=2)

g1 <- ggplot(traj[traj$Outcome=="casp",], aes(x = Wave, y = Estimate, group="") )
g2 <- g1 + theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Poor Quality of Life", x = "", y = "Average score" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)
casp1=g4  + scale_color_viridis(discrete = TRUE, begin=.0 ) + ylim(21, 24) +
  theme(plot.title = element_text(face = "bold")) 
casp1


#loneliness

g1 <- ggplot(traj[traj$Outcome=="lone",], aes(x = Wave, y = Estimate, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Loneliness", x = "", y = "Average score" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5),legend.position = "none")  +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(5.2, 6)
lone1=g4  + scale_color_viridis(discrete = TRUE, begin=0.2 )   + theme(plot.title = element_text(face = "bold")) 
lone1

#dep

g1 <- ggplot(traj[traj$Outcome=="dep",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Depression", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(18, 33)
dep1=g4  + scale_color_viridis(discrete = TRUE, begin=0.3 )  + theme(plot.title = element_text(face = "bold")) 
dep1


#Gad 

g1 <- ggplot(traj[traj$Outcome=="anx",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Anxiety", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(7, 13)
gad1=g4  + scale_color_viridis(discrete = TRUE, begin=0.4 )  + theme(plot.title = element_text(face = "bold")) 
gad1

# financial situation - worried 

g1 <- ggplot(traj[traj$Outcome=="worried",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Financial Hardship, Worried", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(20, 35)
fin_worried1=g4  + scale_color_viridis(discrete = TRUE, begin=0.5 )  + theme(plot.title = element_text(face = "bold")) 
fin_worried1

#financial situation - worse off
g1 <- ggplot(traj[traj$Outcome=="worse",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Financial Hardship, Worse Off", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(15, 25)
fin_worse1=g4  + scale_color_viridis(discrete = TRUE, begin=0.6 )  + theme(plot.title = element_text(face = "bold")) 
fin_worse1

# contact - family 
g1 <- ggplot(traj[traj$Outcome=="contact_family",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Infrequent Social Contact, Family", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(5, 15)
contact_family1=g4  + scale_color_viridis(discrete = TRUE, begin=0.7 )  + theme(plot.title = element_text(face = "bold")) 
contact_family1

# contact - friend
g1 <- ggplot(traj[traj$Outcome=="contact_friend",], aes(x = Wave, y = (Estimate-1)*100, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Infrequent Social Contact, Friends", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=(cil-1)*100, ymax=(ciu-1)*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(5, 20)
contact_friend1=g4  + scale_color_viridis(discrete = TRUE, begin=0.8 )  + theme(plot.title = element_text(face = "bold")) 
contact_friend1

# contact - total
g1 <- ggplot(traj[traj$Outcome=="contact_total",], aes(x = Wave, y = Estimate, group="") )
g2 <- g1 +theme_classic2() +aes(color = "", group="") 
g3<- g2 +  labs(title = "Infrequent Social Contact, Total Score", x = "", y = "Average score" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5),legend.position = "none") +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7) + ylim(20, 27)
contact_total1=g4  + scale_color_viridis(discrete = TRUE, begin=0.9 )  + theme(plot.title = element_text(face = "bold")) 
contact_total1



## combine trajectories 

plots.trajectories<- ggarrange( dep1,  gad1,casp1, lone1, fin_worried1, fin_worse1, 
                                contact_family1, contact_friend1, contact_total1,
                                labels = c(""),
                                ncol = 3, nrow = 3)

plots.trajectories

ggsave("s2_plots.trajectories.jpeg", device = "jpeg",dpi = 300, width =13, height =9, limitsize = F)



### Plots of interaction effects 

## Anxiety w2: work status - do not include as it is not sign

anx<-read_excel("s2_predicted values_anx.xls", sheet=2)

anx.work<- ggplot(anx[anx$Group=="work",], aes(x=Wave, y=(Estimate), group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=(cil), ymax=(ciu)), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Anxiety, Nov-Dec 2020 ", x="", y = "Predicted Percentage", color = "Work status")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE, end=0.7 )  # + ylim(0,12)
anx.work
ggsave("anx.work.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


## QoL s1: sex, work wealth 
qol<-read_excel("s2_predicted values_qol.xls", sheet=2)

qol.sex<- ggplot(qol[qol$Group=="sex",], aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Poor Quality of Life, June-July 2020 ", x="", y = "Predicted Score", color = "Sex")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE, begin=0.5, end =0.1)   + ylim(20,28)
qol.sex
ggsave("qol.sex.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


qol.work<- ggplot(qol[qol$Group=="work",], aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Poor Quality of Life, June-July 2020 ", x="", y = "Predicted Score", color = "Work status")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE, end=0.7)  # + ylim(20,28)
qol.work
ggsave("qol.work.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


qol.wealth<- ggplot(qol[qol$Group=="wealth",], aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Poor Quality of Life, June-July 2020 ", x="", y = "Predicted Score", color = "Wealth")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE, end=0.7)  # + ylim(20,28)
qol.wealth
ggsave("qol.wealth.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)

### Loneliness 

## s1: work status 
lone<-read_excel("s2_predicted values_lone.xls", sheet=2)

lone1.work<- ggplot(lone[lone$Group=="work",], aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Loneliness, June-July 2020 ", x="", y = "Predicted Score", color = "Work status")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)  # + ylim(20,28)
lone1.work
ggsave("lone1.work.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


## s2: age 
lone2.age<- ggplot(lone[lone$Group=="age",], aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Loneliness, Nov-Dec 2020 ", x="", y = "Predicted Score", color = "Age")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)  # + ylim(20,28)
lone2.age
ggsave("lone2.age.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


### Financial hardship

fin<-read_excel("s2_predicted values_fin.xls", sheet=2)

fin_worried1.work<- ggplot(fin[fin$Outcome=="fin_worried",], aes(x=Wave, y=Estimate*100, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil*100, ymax=ciu*100), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Financial Hardship - Worried, June-July 2020 ", x="", y = "Predicted Percentage", color = "Work status")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)   #+ ylim(0,4)
fin_worried1.work
ggsave("fin_worried1.work.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)

fin_worse2.work<- ggplot(fin[fin$Outcome=="fin_worse",], aes(x=Wave, y=Estimate*100, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil*100, ymax=ciu*100), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Financial Hardship - Worse off, Nov-Dec 2020 ", x="", y = "Predicted Percentage", color = "Work status")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)   #+ ylim(0,4)
fin_worse2.work
ggsave("fin_worse2.work.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)

### Social connections

## family s1: wealth 

con<-read_excel("s2_predicted values_con.xls", sheet=2)

con_family1.wealth<- ggplot(con[con$Outcome=="con_family",], aes(x=Wave, y=Estimate*100, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil*100, ymax=ciu*100), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Infrequent Social Contact - Family, June-July 2020 ", x="", y = "Predicted Percentage", color = "Wealth")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)   #+ ylim(0,4)
con_family1.wealth

ggsave("con_family1.wealth.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


con_friend1.sex<- ggplot(con[con$Outcome=="con_friend",], aes(x=Wave, y=Estimate*100, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil*100, ymax=ciu*100), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Infrequent Social Contact - Friends, June-July 2020 ", x="", y = "Predicted Percentage", color = "Sex")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  begin=0.5, end =0.1)   #+ ylim(0,4)
con_friend1.sex

ggsave("con_friend1.sex.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)

## combine 
plots.interactions<- ggarrange(  qol.sex, qol.wealth, qol.work, lone1.work, lone2.age,
                                 fin_worried1.work, fin_worse2.work, con_family1.wealth,con_friend1.sex,
                                 labels = c(""),
                                 ncol = 3, nrow = 3)
plots.interactions
ggsave("plots.interactions.jpeg", device = "jpeg",dpi = 300, width =18, height =13, limitsize = F)
ggsave("plots.interactions.eps", device = "eps",dpi = 300, width =18, height =13, limitsize = F)


### change in social contact tot score: covid infection 
con<-read_excel("predicted values.contact.change.xls", sheet=2)


con_change.infection<- ggplot(con, aes(x=Wave, y=Estimate, group=Category, color=Category)) + 
  geom_line(size=0.7) +
  geom_point()+
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02))  + theme_light() +
  labs(title="Infrequent Social Contact - Total Score", x="", y = "Predicted Score", color = "")  +
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = "bold"))+
  scale_color_viridis(discrete = TRUE,  end =0.7)   + ylim(22,25)
con_change.infection

ggsave("con_change.infection.jpeg", device = "jpeg",dpi = 300, width =8, height =5, limitsize = F)


#### estimated trajectories by covid status 

#to change: category, labs, ylim, colours

traj<-read_excel("predicted values_covid.xlsx")
colnames(traj)[3]<- "COVID-19 infection"

g1 <- ggplot(traj[traj$Outcome=="qol",], aes(x = Sweep, y = Estimate, group=`COVID-19 infection`) )
g2 <- g1 + theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Poor Quality of Life", x = "", y = "Average score" , color ="COVID-19 infection" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5) ) +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)
casp1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6) +  ylim(21, 27) +
  theme(plot.title = element_text(face = "bold")) 
casp1


#loneliness

g1 <- ggplot(traj[traj$Outcome=="lone",], aes(x = Sweep, y = Estimate, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Loneliness", x = "", y = "Average score",color ="COVID-19 infection" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5))  +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(5.2, 7)
lone1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )   + theme(plot.title = element_text(face = "bold")) 
lone1

#dep

g1 <- ggplot(traj[traj$Outcome=="dep",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Depression", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(10, 100)
dep1=g4  + scale_color_viridis(discrete = TRUE,  begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
dep1


#Gad 

g1 <- ggplot(traj[traj$Outcome=="anx",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Anxiety", x = "", y = "Percentage (%)" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(0, 25)
gad1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
gad1

# financial situation - worried 

g1 <- ggplot(traj[traj$Outcome=="worried",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Financial Hardship, Worried", x = "", y = "Percentage (%)",color ="COVID-19 infection" ) + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7) + ylim(10, 100)
fin_worried1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
fin_worried1


#financial situation - worse off
g1 <- ggplot(traj[traj$Outcome=="worse",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Financial Hardship, Worse Off", x = "", y = "Percentage (%)" ,color ="COVID-19 infection") + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(10, 60)
fin_worse1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
fin_worse1

# contact - family 
g1 <- ggplot(traj[traj$Outcome=="cont_fam",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Infrequent Social Contact, Family", x = "", y = "Percentage (%)" , color ="COVID-19 infection") + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(0, 15)
contact_family1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
contact_family1

# contact - friend
g1 <- ggplot(traj[traj$Outcome=="cont_fri",], aes(x = Sweep, y = est_OR*100, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Infrequent Social Contact, Friends", x = "", y = "Percentage (%)" ,color ="COVID-19 infection") + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil_OR*100, ymax=ciu_OR*100), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7)  + ylim(0, 30)
contact_friend1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
contact_friend1

# contact - total
g1 <- ggplot(traj[traj$Outcome=="cont_tot",], aes(x = Sweep, y = Estimate, group=`COVID-19 infection`) )
g2 <- g1 +theme_classic2() +aes(color = `COVID-19 infection`, group=`COVID-19 infection`) 
g3<- g2 +  labs(title = "Infrequent Social Contact, Total Score", x = "", y = "Average score" , color ="COVID-19 infection") + 
  theme(axis.text.x = element_text(angle = 0, hjust =0.5, vjust = 0.5, face="bold"), 
        plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(aes(ymin=cil, ymax=ciu), width=.2,
                position=position_dodge(0.02)) 
g4<- g3 + stat_summary(fun = mean, geom = "line", lwd = 0.7) + ylim(22, 27)
contact_total1=g4  + scale_color_viridis(discrete = TRUE, begin=.2 , end =.6 )  + theme(plot.title = element_text(face = "bold")) 
contact_total1



## combine trajectories 

plots.trajectories_covid<- ggarrange( dep1,  gad1,casp1, lone1, fin_worried1, fin_worse1, 
                                      contact_family1, contact_friend1, contact_total1,
                                      labels = c(""), common.legend = T, legend = "bottom",
                                      ncol = 3, nrow = 3)

plots.trajectories_covid

ggsave("s2_plots.trajectories_covid.jpeg", device = "jpeg",dpi = 300, width =13, height =9, limitsize = F)
ggsave("s2_plots.trajectories_covid.eps", device = "eps",dpi = 300, width =13, height =9, limitsize = F)

############################################################################################
################################## Revisions ###############################################
############################################################################################

### 1) FDR correction

library(readxl)
library(writexl)

fdr<-read_excel("fdr correction.xlsx")
fdr$pval_fdr<- p.adjust(fdr$`p-value`, method ="fdr")

write_xlsx(list(fdr = fdr), "fdr correction_pvalues.xlsx")


### 2) Separate figures for ORs and b coefficients 

main<-read_excel("Figure1_rev.xlsx", sheet=1)
main<-main[main$Model != "Propensity score weighting",]


str(main$Model)
#column names 
names(main)[2]<-"Phenotype_names"
names(main)[5]<-"Coefficient"
names(main)[7]<-"lower_CI"
names(main)[8]<-"upper_CI"


#rename
allModelFrame <- main

## create new variables
allModelFrame$Coefficient1<-round(allModelFrame$Coefficient,2)

# order pheno names
allModelFrame$Phenotype_names1<- factor (as.factor(allModelFrame$Phenotype_names), 
                                         levels = c("Infrequent Contact: Total score, Nov-Dec 2020",
                                                    "Infrequent Contact: Total score, June-July 2020",
                                                    "Infrequent Contact: Friends, Nov-Dec 2020",
                                                    "Infrequent Contact: Friends, June-July 2020",
                                                    "Infrequent Contact: Family, Nov-Dec 2020",
                                                    "Infrequent Contact: Family, June-July 2020",
                                                    "Financial Hardship: Worse off, Nov-Dec 2020" ,
                                                    "Financial Hardship: Worse off, June-July 2020",
                                                    "Financial Hardship: Worried, Nov-Dec 2020",
                                                    "Financial Hardship: Worried, June-July 2020", 
                                                    "Mental Health: Loneliness, Nov-Dec 2020",
                                                    "Mental Health: Loneliness, June-July 2020",
                                                    "Mental Health: Poor QoL, Nov-Dec 2020",
                                                    "Mental Health: Poor QoL, June-July 2020",
                                                    "Mental Health: Anxiety, Nov-Dec 2020",
                                                    "Mental Health: Anxiety, June-July 2020",
                                                    "Mental Health: Depression, Nov-Dec 2020",
                                                    "Mental Health: Depression, June-July 2020"))
table(allModelFrame$Phenotype_names1)

allModelFrame$Model

# Plot results, OR
new_plot <- ggplot(allModelFrame[allModelFrame$Type=="OR", ], aes(shape= Model,col=Model)) +
  scale_color_manual(values = c("#277F8EFF")) 

new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names1, ymin = lower_CI,
                                          ymax = upper_CI),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names1, y = Coefficient, ymin = lower_CI,
                                           ymax = upper_CI),
                                       lwd = 1/2, position = position_dodge(width = 1/2)) 
new_plot <- new_plot + theme(axis.text.x = element_text(size=12, face="bold", color="black" ),axis.text.y = element_text(size=12, face="bold", color="black"))

new_plot <- new_plot  + theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +  coord_flip() + scale_shape_discrete(name  ="") +geom_hline(yintercept=1,linetype=2) + scale_y_continuous(limits = c(-0.8, 2.6))


fig_allOR<- new_plot + theme_minimal() + theme(legend.position="bottom") + ggtitle("") + theme(plot.title = element_text(hjust =1, size = 12,face = "bold")) + theme(legend.title = element_blank())

print(fig_allOR)


# Plot results, beta
new_plot <- ggplot(allModelFrame[allModelFrame$Type=="b", ], aes(shape= Model,col=Model)) +
  scale_color_manual(values = c("#440154FF")) 

new_plot <- new_plot + geom_linerange(aes(x = Phenotype_names1, ymin = lower_CI,
                                          ymax = upper_CI),
                                      lwd = 1, position = position_dodge(width = 1/2))
new_plot <- new_plot + geom_pointrange(aes(x = Phenotype_names1, y = Coefficient, ymin = lower_CI,
                                           ymax = upper_CI),
                                       lwd = 1/2, position = position_dodge(width = 1/2)) 
new_plot <- new_plot + theme(axis.text.x = element_text(size=12, face="bold", color="black" ),axis.text.y = element_text(size=12, face="bold", color="black"))

new_plot <- new_plot  + theme(legend.position="top") +
  labs(title = "", x = "", y = "", color = c(""))  +  coord_flip() + scale_shape_discrete(name  ="") +geom_hline(yintercept=0,linetype=2) + scale_y_continuous(limits = c(-0.8, 2.6))


fig_allBETA<- new_plot + theme_minimal() + theme(legend.position="bottom") + ggtitle("") + theme(plot.title = element_text(hjust =1, size = 12,face = "bold")) + theme(legend.title = element_blank())

print(fig_allBETA)


### 
fig.rev<- ggarrange( fig_allOR, fig_allBETA,
                     labels = c(""),
                     ncol = 2, nrow = 1)

fig.rev

ggsave("fig.rev.jpeg", device = "jpeg",dpi = 300, width =13, height =6, limitsize = F)


