# Figure 3 HEART
ttn %>% mutate(dvo2kg = (vo2max_kg_3-vo2max_kg_2)-(vo2max_kg_2-vo2max_kg_1),
               vo2kg_rest = vo2max_kg_2-vo2max_kg_1,
               vo2kg_ex = vo2max_kg_3-vo2max_kg_2) %>% 
  pivot_longer(cols = c(vo2kg_ex, vo2kg_rest), names_to = "time") %>%
  mutate(time = factor(time, levels = c("vo2kg_rest","vo2kg_ex" ), labels = c("Usual care", "Training")),
         time1 = as.numeric(time)* (1 + runif(n=28, min=-0.05, max=0.05)),
         time2 = if_else(time1>1.5, time1-.6, time1),
         time3 = if_else(time1>1.5, time1-.3, time1),
         value1 = if_else(time =='Usual care', value,NA),
         value2 = if_else(time =='Usual care', NA,value)) %>% #select(value1, value,value2)
  ggplot(aes(x=time, y = value, fill = time))+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(x= 2.5,ymin = 1.8, ymax = 3.94), width = .05)+
  geom_point(aes(x= 2.5,y = 2.873), shape = 23, size = 5, fill = "#0C335D")+
  annotate("text", x= 2.4, y = 2.873, label = "Difference", angle = 90, family = "Roboto")+
  #geom_point(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
  #geom_line(aes(group = ID),position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)) +
  
  geom_line(aes(x=time3, y = value, group = ID), 
            position = position_nudge(x=.1))+
  geom_point(aes(x=time3, y = value, group = ID),
             size = 3, show.legend = F, color ="black", 
             position = position_nudge(x=.1))+
  #gghalves::geom_half_point(size = 3, show.legend = F, width =.4, color ="black")+
  #geom_boxplot(width = .2, position = position_nudge(x=-.1), show.legend = F)+
  # geom_point( aes(color =time), size = 3, show.legend = F, position = position_jitter(width = .1), #color = "slategrey"
  #  )+
  gghalves::geom_half_violin(aes(y=value1),show.legend = F, stroke = NULL, color = "black", fill = '#C5C5C5CC')+
  gghalves::geom_half_violin(aes(y=value2, x= 2),show.legend = F, stroke = NULL, color = "black",side = 'r', fill = "#303030CC")+
  scale_y_continuous(breaks = seq(-10,10,1))+
  scale_x_discrete()+
  # ggdist::stat_dist_halfeye(aes(x= time, y = value, group = time))+
  scale_color_scico_d(palette ="corkO")+
  stat_summary(aes(x= time, y = value1), color = "black", fun = "mean", shape = 95, size = 5, show.legend = F, position = position_nudge(x=-.1)
  )+
  stat_summary(aes(x= time, y = value2), color = "black", fun = "mean", shape = 95, size = 5, show.legend = F, position = position_nudge(x=.1)
  )+
  stat_summary(aes(x= time, y = value1), color = "black", fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               position = position_nudge(x=-.1),
               # geom = "crossbar",
               #geom = "pointrange",
               geom = "errorbar",
               width = .1)+
  stat_summary(aes(x= time, y = value2), color = "black", fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               position = position_nudge(x=.1),
               # geom = "crossbar",
               #geom = "pointrange",
               geom = "errorbar",
               width = .1)+
  labs(x= "Study period", y = "Change in peak oxygen uptake (ml 0<sub>2</sub>/kg/min)")+
  theme(panel.background = element_blank(),
        text = element_text(family = "Roboto", color = "black"),
        axis.text = element_text(family = "Roboto", color = "black"),
        axis.title.y = element_markdown(family = "Roboto", color = "black"),
        panel.grid.major.y = element_line(color = "gray79"))+
  scale_fill_scico_d(palette = "grayC", alpha = .8, begin = .2, end = .8)


pixel40(12,16,1150)
ggsave(filename = 'Heart - Figure 3.pdf', device = cairo_pdf , height = 12, width = 16, units = "cm", 
       dpi = 1150)
ggsave('Heart - Figure 3.tiff', dpi = 1159, units = "cm", height = 12, width = 16, compression = "lzw")
