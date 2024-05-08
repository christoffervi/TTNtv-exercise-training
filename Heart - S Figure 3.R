s1<-
df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%

  pivot_longer(cols = c(kreatinin_1, kreatinin_2, kreatinin_3)) %>%
  mutate(name = case_when(name=="kreatinin_1"~ "Baseline",
                          name=='kreatinin_3' ~"Training",
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
  labs(x = "", y = "Creatinine, µmol/L",title = "")

s1

s2<-
df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%

  pivot_longer(cols = c(kreatinkinase_1, kreatinkinase_2, kreatinkinase_3)) %>%
  mutate(name = case_when(name=="kreatinkinase_1"~ "Baseline",
                          name=='kreatinkinase_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_log10(n.breaks= 8)+
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
  labs(x = "", y = "Creatine kinase, U/L",title = "")


s2
s1+s2+plot_annotation(tag_levels = 'A')

############ Functional tests

f1<-
df %>%
  group_by(ID) %>% mutate(map_1 = (bt1_dia_1+bt2_dia_1+bt1_dia_1+bt2_dia_1+bt1_sys_1+bt2_sys_1)/6,
                          map_2 = (bt1_dia_2+bt2_dia_2+bt1_dia_2+bt2_dia_2+bt1_sys_2+bt2_sys_2)/6,
                          map_3 = (bt1_dia_3+bt2_dia_3+bt1_dia_3+bt2_dia_3+bt1_sys_3+bt2_sys_3)/6,
                          ) %>% 

  pivot_longer(cols = c(map_1, map_2, map_3)) %>%
  mutate(name = case_when(name=="map_1"~ "Baseline",
                          name=='map_3' ~"Training",
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
  labs(x = "", y = "Mean arterial blood pressure, mmHg",title = "")
f1




ggsave(filename = '6map_three.pdf', device = cairo_pdf , height = 12, width = 18, units = "cm", dpi =3000)

f2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%

  pivot_longer(cols = c(tnt_1, tnt_2, tnt_3)) %>%
  mutate(name = case_when(name=="tnt_1"~ "Baseline",
                          name=='tnt_3' ~"Training",
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
  labs(x = "", y = "Troponin T, ng/L",title = "")
f2

p3<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%

  pivot_longer(cols = c(probnp_1, probnp_2, probnp_3)) %>%
  mutate(name = case_when(name=="probnp_1"~ "Baseline",
                          name=='probnp_3' ~"Training",
                          T~'Usual care'),
         name = fct_relevel(name, 'Baseline', 'Usual care')) %>% #select(name, value)
  ggplot(aes(y=value, x = name, fill = name)) +
  # geom_half_violin(aes(), show.legend = F, size = 1, width =.5, position = position_nudge(x=-.25))+
  geom_line(aes(group =ID), show.legend = F, linewidth = .2, , position = position_jitter(seed = 1, width = .2), color = 'gray79')+
  scale_y_log10(n.breaks= 5)+
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
  labs(x = "", y = "NTproBNP, pg/mL",title = "")
p3

s1+s2+f1+f2+p3
########### VO2
p2<-
df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%

  pivot_longer(cols = c(kol_ldl_1, kol_ldl_2, kol_ldl_3)) %>%
  mutate(name = case_when(name=="kol_ldl_1"~ "Baseline",
                          name=='kol_ldl_3' ~"Training",
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
  labs(x = "", y = "LDL cholesterol, mmol/L",title = "")
p2



p2<-
  df %>%
  mutate(col_id = fct_reorder(ID, height),
         col_id = as.numeric(col_id)
  ) %>%
  
  pivot_longer(cols = c(myoglobin_1, myoglobin_2, myoglobin_3)) %>%
  mutate(name = case_when(name=="myoglobin_1"~ "Baseline",
                          name=='myoglobin_3' ~"Training",
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
  labs(x = "", y = "Myoglobin, mcg/L",title = "")
p2
s1+f1+s2+p2+f2+p3+plot_layout(ncol = 2)#+plot_annotation(tag_levels = 'A')
ggsave(filename = 'many3.pdf', device = cairo_pdf , height = 30, width = 24, units = "cm", dpi =3000)
