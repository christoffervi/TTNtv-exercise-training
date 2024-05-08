################
#HEART Figure 5
################

p1<-
  df %>%
  mutate(col_id = fct_reorder(ID, hgb_mass_2),
         col_id = as.numeric(col_id)
  ) %>% 
  filter(!is.na(hgb_mass_3)) %>% 
  
  pivot_longer(cols = c(hgb_mass_2, hgb_mass_3)) %>% 
  mutate(name = if_else(name=="hgb_mass_2", "Baseline", "Training")) %>% 
  ggplot(aes(y=value, x = name, fill = name)) +
  
  geom_violin(aes(), show.legend = F, size = 1, width =.5)+ 
  geom_line(aes(group =ID), show.legend = F, size = .4, width =.5, position = position_jitter(seed = 1, width = .2))+ 
  scale_y_continuous(breaks=seq(0,10000,100))+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5, 
             alpha = 1,
             position = position_jitter(width = .2, seed = 1)
  ) +
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F, 
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  annotate("text", x=1.5, y= 1250, angle = 0, family = "Roboto", label = "p=0.003")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(
    plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
    axis.text = element_text(color = "black", family = "Roboto", size = 14),
    axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Total hemoglobin mass, g",title = "")+
  coord_cartesian(ylim = c(600,1300), xlim = c(0.4,2.6),
                  expand = F)


p2<-
  df %>%
  mutate(col_id = fct_reorder(ID, hgb_mass_2),
         col_id = as.numeric(col_id)
  ) %>% 
  filter(!is.na(hgb_mass_3)) %>% 
  
  pivot_longer(cols = c(blood_vol_2, blood_vol_3)) %>% 
  mutate(name = if_else(name=="blood_vol_2", "Baseline", "Training")) %>% 
  ggplot(aes(y=value, x = name, fill = name)) +
  
  geom_violin(aes(), show.legend = F, size = 1, width =.5)+ 
  geom_line(aes(group =ID), show.legend = F, size = .4, width =.5, position = position_jitter(seed = 1, width = .2))+ 
  scale_y_continuous(breaks=seq(0,10000,500))+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5, 
             alpha = 1,
             position = position_jitter(width = .2, seed = 1)
  ) +
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F, 
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.001")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Blood volume, mL",title = "")+
  coord_cartesian(ylim = c(4200,9000), xlim = c(0.4,2.6), expand = F)
p1/p2+plot_annotation(tag_levels = "A")
pixel40(24,16,819)
ggsave(filename = 'Heart Figure 5.pdf', device = cairo_pdf , height = 24, width = 16, units = "cm", dpi =819)
