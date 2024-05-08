s1<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(co_rest_1, co_rest_2, co_rest_3)) %>%
  mutate(name = case_when(name=="co_rest_1"~ "Baseline",
                          name=='co_rest_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Cardiac output at rest, L/min",title = "")

s1

s2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(co_max_1, co_max_2, co_max_3)) %>%
  mutate(name = case_when(name=="co_max_1"~ "Baseline",
                          name=='co_max_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 5)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Cardiac output at max exertion, L/min",title = "")


s2
s1/s2+plot_annotation(tag_levels = 'A')
ggsave(filename = 'CO_three.pdf', device = cairo_pdf , height = 24, width = 18, units = "cm", dpi =3000)
s1+s2+plot_annotation(tag_levels = 'A')
ggsave(filename = 'CO_three.pdf', device = cairo_pdf , height = 12, width = 24, units = "cm", dpi =3000)

############ Functional tests

f1<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(mwt_1, mwt_2, mwt_3)) %>%
  mutate(name = case_when(name=="mwt_1"~ "Baseline",
                          name=='mwt_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "6-minute walk test distance, meters",title = "")
f1




ggsave(filename = '6MWT_three.pdf', device = cairo_pdf , height = 12, width = 18, units = "cm", dpi =3000)

f2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(sf36_overall1, sf36_overall2, sf36_overall3)) %>%
  mutate(name = case_when(name=="sf36_overall1"~ "Baseline",
                          name=='sf36_overall3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Short from 36",title = "")
f2

p3<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(sf36_pf1, sf36_pf2, sf36_pf3)) %>%
  mutate(name = case_when(name=="sf36_pf1"~ "Baseline",
                          name=='sf36_pf3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Short from 36, physical capacity",title = "")
p3

########### VO2
p2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(vo2max_1, vo2max_2, vo2max_3)) %>%
  mutate(name = case_when(name=="vo2max_1"~ "Baseline",
                          name=='vo2max_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "VO2 max, mL O2/min",title = "")

p1<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(vo2max_kg_1, vo2max_kg_2, vo2max_kg_3)) %>%
  mutate(name = case_when(name=="vo2max_kg_1"~ "Baseline",
                          name=='vo2max_kg_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "VO2 max, mL O2/kg/min",title = "")

p1/p2+plot_annotation(tag_levels = 'A')
ggsave(filename = 'vo2_three.pdf', device = cairo_pdf , height = 24, width = 18, units = "cm", dpi =3000)
p1+p2+plot_annotation(tag_levels = 'A')
ggsave(filename = 'vo2_three.pdf', device = cairo_pdf , height = 12, width = 24, units = "cm", dpi =3000)

df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(max_workload_1, max_workload_2, max_workload_3)) %>%
  mutate(name = case_when(name=="max_workload_1"~ "Baseline",
                          name=='max_workload_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Max workload , Watts",title = "")


###



df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(inno_sv_plateau_1, inno_sv_plateau_2, inno_sv_plateau_3)) %>%
  mutate(name = case_when(name=="inno_sv_plateau_1"~ "Baseline",
                          name=='inno_sv_plateau_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Stroke volume at submaximal exertion, mL",title = "")


###############################


e1<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(lvef_1, lvef_2)) %>%
  mutate(name = case_when(name=="lvef_1"~ "Baseline",
                          name=='lvef_2' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 4)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Left ventricular ejection fraction, %",title = "")
e1

e2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(gls_1, gls_2)) %>%
  mutate(name = case_when(name=="gls_1"~ "Baseline",
                          name=='gls_2' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_continuous(n.breaks= 5)+
  geom_point(aes(color = name), show.legend = F, size = 5, #maxwidth = .5,
             alpha = 1,position = position_jitter(width = .2, seed = 1)) +
  
  stat_summary(fun = "mean", shape = 95, size = 10, show.legend = F)+
  stat_summary(fun.data = "mean_cl_boot" , shape = 95, size = 1, show.legend = F,
               geom = "errorbar",
               width = .1)+
  # ggforce::geom_sina(aes(color = col_id), show.legend = F, size = 6, maxwidth = .5, alpha = .7) +
  # annotate("text", x=1.5, y= 8500, angle = 0, family = "Roboto", label = "p<0.01")+
  scico::scale_fill_scico_d(palette = "grayC", begin = .1, end = .5)+
  scico::scale_color_scico_d(palette = "corkO", begin = .3, end= .7, direction = -1)+
  theme_minimal()+
  theme(plot.title = element_text(color = "black", family = "Roboto", size = 20, face = "bold"),
        axis.text = element_text(color = "black", family = "Roboto", size = 14),
        axis.title = element_text(color = "black", family = "Roboto", size = 14, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "Global longitudinal strain, %",title = "")
e2

e1/e2+plot_annotation(tag_levels = 'A')

ggsave(filename = 'echo_three.pdf', device = cairo_pdf , height = 24, width = 18, units = "cm", dpi =3000)

layout_temp <-
  '
AB
CD
EF
GH'
p1+p2+s1+s2+e1+e2+f1+f2+plot_layout(ncol = 2)+plot_annotation(tag_levels = 'A')
ggsave(filename = 'many.pdf', device = cairo_pdf , height = 36, width = 28, units = "cm", dpi =3000)
