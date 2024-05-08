pixel40 <- function(x,y,dpi ) {
  vuf<- (((x*0.393700787)*dpi)*((y*0.393700787)*dpi))
  miav <- vuf/40000000
  miav <- sqrt(miav)
  print(glue::glue("{round({vuf}/1000000,1)} Megapixel
                   Max DPI {round({dpi}/{miav},0)}"))}


rbind(
  norm_slope2(ttn, "<span style=color:#327FA5>**Peak oxygen uptake**</span><br> (ml O<sup>2</sup>/kg/min)", 2, vo2max_kg_2,one =NA, vo2max_kg_3) %>% mutate(area = "PC"),
  norm_slope2(ttn, "<span style=color:#327FA5>**Maximal workload**</span><br>(Watts)", 2, max_workload_2,one =NA, max_workload_3) %>% mutate(area = "PC"),
  norm_slope2(ttn, "<span style=color:#327FA5>**Maximal cardiac output**</span><br> (L/min)", 2, co_max_2,one =NA, co_max_3) %>% mutate(area = "PC"),
  
  norm_slope2(ttn, "<span style=color:#964A35>**Blood volume**</span><br> (ml)", 2, blood_vol_2,one =NA, blood_vol_3) %>% mutate(area = "blood"),
  norm_slope2(ttn, "<span style=color:#964A35>**Total hemoglobin mass**</span><br> (grams)", 2, hgb_mass_2,one =NA, hgb_mass_3) %>% mutate(area = "blood"),
  
  norm_slope2(ttn, "<span style=color:#4A6833>**Left ventricular<br> ejection fraction**</span> (%)", 2, lvef_1,one =NA, lvef_2) %>% mutate(area = "cardiac"),
  norm_slope2(ttn, "<span style=color:#4A6833>**Global longitudinal<br> strain**</span> (negative %)", 2, gls_2,one =NA, gls_1) %>% mutate(area = "cardiac"),
  
  norm_slope2(ttn, "<span style=color:#E8CF6D>**Creatine kinase MB**</span><br> (µg/L)", 2, ckmb_2,probnp_2, ckmb_3) %>% mutate(area = "damage"),
  norm_slope2(ttn, "<span style=color:#E8CF6D>**Creatine kinase**</span><br> (U/L)", 2, kreatinkinase_2,kreatinkinase_2, kreatinkinase_3) %>% mutate(area = "damage")
) %>% 
  filter(term!="(Intercept)") %>% 
  mutate(r = row_number(),
         term = fct_reorder(term, desc(r)),
         across(.cols =  c("estimate", "conf.low", "conf.high"),~case_when(abs(.x)>5~round(.x,0),
                                                                           abs(.x)>1~round(.x,1),
                                                                           T~round(.x,2))),
         label_text = paste(sep = "", estimate, " (", conf.high,"-", conf.low,")"),
         label_p = case_when(p.value<0.001~"<0.001",
                             T~as.character(round(p.value, 3))),
         per = round(percent_change, 0),
         per = if_else(per==-14, 14, per)
  ) %>%
  mutate() %>% 
  ggplot(aes(x= term, y = z_change, ymin = z_low, ymax = z_hi))+
  geom_segment(aes(x=term, xend=term, y=-2, yend = 1.1), linetype = 3, color = "gray79")+
  geom_segment(aes(x=9.5, xend=9.5, y= -.05, yend = -.5), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=9.5, xend=9.5, y= .05, yend = .5), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_errorbar(width = .1)+ 
  geom_hline(aes(yintercept = 0), linetype =2)+
  geom_point(aes(color = area), size=5, show.legend = F, #shape=21
  )+
  geom_text(aes(x= term, y=1.3, label = label_text), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=1.7, label = per), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=2.1, label = label_p), family = "Roboto", size = 3.5)+
  annotate("text",
           x= c(10,10,10,10,10),
           y = c(-.25,.25, 1.3,1.7,2.1),
           label = c("Deterioration", "Improvement", "Absolute change \n (CI)", "Percent\n change", "P-value"),
           size = c(3.5),
           fontface = "bold", family = "Roboto",
           color = c("#990002", "#82B55A", "black", "black", "black")
  )+
  scale_y_continuous(breaks = seq(-2,1,.2))+
  #  scico::scale_fill_scico_d(palette = "batlow")+
  scale_color_manual(values = c("#964A35", "#4A6833", "#E8CF6D","#327FA5"))+
  labs(y = "**Normalized change from baseline (*z*)**",
       title = "**Exercise in Dilated cardiomyopathy caused by TTNtv**",
       subtitle = "The effect of 8 weeks of exercise on <span style=color:#327FA5>**physical capacity**</span>, 
       <span style=color:#964A35>**blood volume**</span>, 
       <span style=color:#4A6833>**cardiac function**</span> 
       and markers of <span style=color:#E8CF6D>**cardiac or skeletal<br> muscle damage**</span>. Changes for the parameters reflect the raw change during the exercise period")+  
  #  ggthemes::theme_clean()+ 
  coord_flip(ylim = c(-.5,2.3),
             xlim = c(0.6,10.5), 
             expand = F)+
  
  theme(plot.title = element_blank(),#element_markdown(family = "Roboto", hjust = -.3, halign = 0),
        plot.subtitle = element_markdown(family = "Roboto", hjust = 1, halign = 0, color = "black",size =10.5),
        axis.text.y = element_markdown(family = "Roboto", color = "black", size = 10, hjust = .5),
        axis.text.x = element_markdown(family = "Roboto", color = "black", size = 10),
        panel.background = element_rect(fill = "white"),
        axis.text = element_markdown(family = "Roboto", color = "black"),
        axis.title.x = element_markdown(family = "Roboto", color = "black", hjust = .2),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("central_illustration.png", height = 16, width = 20.5, dpi = 1200, units = "cm")

ggsave("central_illu.tiff", height = 16, width = 20.5, dpi = 1200, units = "cm", compression = "lzw")
