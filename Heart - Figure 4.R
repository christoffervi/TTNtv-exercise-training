ttn<-
  ttn %>% 
  mutate(dvo2_1 = vo2max_kg_2-vo2max_kg_1,
         dvo2_2 = vo2max_kg_3-vo2max_kg_2,
         dabsvo2_1 = vo2max_2-vo2max_1,
         dabsvo2_2 = vo2max_3-vo2max_2,
         dwatt_1 = max_workload_2-max_workload_1,
         dwatt_2 = max_workload_3-max_workload_2,
         dcorest_1 = co_rest_2-co_rest_1,
         dcorest_2 = co_rest_3-co_rest_2,
         
         
         dcoplateau_1 = (co_inno_plateau_2)*1000/nex_hr_run_plateau_2-co_inno_plateau_1*1000/nex_hr_run_plateau_1,
         dcoplateau_2 = co_inno_plateau_3*1000/nex_hr_run_plateau_3-co_inno_plateau_2*1000/nex_hr_run_plateau_2,
         #dcoplateau_1 = (nex_co_run_plateau_2)*1000/nex_hr_run_plateau_2-nex_co_run_plateau_1*1000/nex_hr_run_plateau_1,
         #dcoplateau_2 = nex_co_run_plateau_3*1000/nex_hr_run_plateau_3-nex_co_run_plateau_2*1000/nex_hr_run_plateau_2,
         
         lvef_1 = lvef_bp_1,
         lvef_2 = lvef_bp_2,
         dcomax_1 = co_inno_max_2-co_inno_max_1,
         dcomax_2= co_inno_max_3-co_inno_max_2,
         d6mwt_1 = mwt_2-mwt_1,
         d6mwt_2 = mwt_3-mwt_2,
         sf36_1 = ((sf36_pf2-sf36_pf1)-70.61)/27.42,
         sf36_2 = ((sf36_pf3-sf36_pf2)-70.61)/27.42,
         r= row_number(),
         across(contains("nex_co_run"),~as.numeric(.x))) %>% 
  group_by(ID) %>% 
  mutate(co_max_1 = case_when(r==2~nex_co_run_max_1,
                              r%in%c(3,6,13)~co_inno_max_1,
                              T~mean(c(nex_co_run_max_1, co_inno_max_1), na.rm = T)),
         co_max_2 = case_when(r==2~nex_co_run_max_2,
                              r%in%c(3,6,13)~co_inno_max_2,
                              T~mean(c(nex_co_run_max_2, co_inno_max_2), na.rm = T)),
         co_max_3 = case_when(r==2~nex_co_run_max_3,
                              r%in%c(3,6,13)~co_inno_max_3,
                              T~mean(c(nex_co_run_max_3, co_inno_max_3), na.rm = T)),
         dcomax_1 = co_max_2-co_max_1,
         dcomax_2= co_max_3-co_max_2,
  ) %>% ungroup()


rbind(
  norm_slope2(ttn, "Peak oxygen uptake, <br> ml O2/kg/min", 2, dvo2_1,one =NA, dvo2_2) %>% 
    mutate(abs = mean(ttn$vo2max_kg_1, na.rm=T),
           abs_sd = sd(ttn$vo2max_kg_1, na.rm=T)),
  norm_slope2(ttn, "Max workload, <br>Watts", 2, dwatt_1,one =NA, dwatt_2) %>% 
    mutate(abs = mean(ttn$max_workload_1, na.rm=T),
           abs_sd = sd(ttn$max_workload_1, na.rm=T)),
  norm_slope2(ttn, "Maximal cardiac output <br> L/min", 2, dcomax_1,one =NA, dcomax_2) %>% 
    mutate(abs = mean(ttn$co_max_1, na.rm=T),
           abs_sd = sd(ttn$co_max_1, na.rm=T)),
  norm_slope2(ttn, "Submaximal stroke volume <br> mL", 2, dcoplateau_1,one =NA, dcoplateau_2) %>% 
    mutate(abs = mean(ttn$co_inno_plateau_1*1000/ttn$nex_hr_run_plateau_1, na.rm=T),
           abs_sd = sd(ttn$co_inno_plateau_1*1000/ttn$nex_hr_run_plateau_1, na.rm=T)),
  norm_slope2(ttn, "Resting cardiac output <br> L/min", 2, dcorest_1,one =NA, dcorest_2) %>% 
    mutate(abs = mean(ttn$co_inno_max_1, na.rm=T),
           abs_sd = sd(ttn$co_inno_max_1, na.rm=T)),
  norm_slope2(ttn, "Left ventricular <br> ejection fraction, %", 2, lvef_1,one =NA, lvef_2) %>% 
    mutate(abs = mean(ttn$lvef_bp_1, na.rm=T),
           abs_sd = sd(ttn$lvef_bp_1, na.rm=T)),
  norm_slope2(ttn, "Global longitudinal <br> strain, negative %", 2, gls_2,one =NA, gls_1) %>% 
    mutate(abs = mean(ttn$gls_1, na.rm=T),
           abs_sd = sd(ttn$gls_1, na.rm=T)),
  norm_slope2(ttn, "Blood volume <br> ml", 2, blood_vol_2,one =NA, blood_vol_3) %>% 
    mutate(abs = mean(ttn$blood_vol_2, na.rm=T),
           abs_sd = sd(ttn$blood_vol_2, na.rm=T)),
  norm_slope2(ttn, "Total hemoglobin mass <br> grams", 2, hgb_mass_2,one =NA, hgb_mass_3) %>% 
    mutate(abs = mean(ttn$hgb_mass_2, na.rm=T),
           abs_sd = sd(ttn$hgb_mass_2, na.rm=T)),
  norm_slope2(ttn, "6 minute walk distance <br> meters", 2, mwt_2,one = NA, mwt_3) %>% 
    mutate(abs = mean(ttn$mwt_2, na.rm=T),
           abs_sd = sd(ttn$mwt_2, na.rm=T)),
  norm_slope2(ttn, "Short Form 36 survey <br> physical function <br> z-score", 2, sf36_1,one = NA, sf36_2) %>% 
    mutate(abs = mean(ttn$sf36_overall1, na.rm=T),
           abs_sd = sd(ttn$sf36_overall1, na.rm=T))
) %>% 
  mutate(r = row_number(),
         z_change = if_else(term == "Short Form 36 survey <br> physical function <br> z-score", estimate, z_change), 
         z_low = if_else(term == "Short Form 36 survey <br> physical function <br> z-score", conf.low, z_low),
         z_hi = if_else(term == "Short Form 36 survey <br> physical function <br> z-score", conf.high, z_hi),
         term = fct_reorder(term, desc(r)),
         across(.cols =  c("estimate", "conf.low", "conf.high"),~case_when(abs(.x)>5~round(.x,0),
                                                                           abs(.x)>1~round(.x,1),
                                                                           T~round(.x,2))),
         label_text = paste(sep = "", estimate, " (", conf.high,"-", conf.low,")"),
         label_p = case_when(p.value<0.001~"<0.001",
                             T~as.character(round(p.value, 3))),
         abs_text = glue::glue("{round({abs},1)} 
                               ({round({abs_sd},1)})")
  ) %>%
  mutate(z_hi = case_when(r==2~0.454,T~z_hi),
         z_low = case_when(r==2~2.1,T~z_low),
         z_low) %>% 
  ggplot(aes(x= term, y = z_change, ymin = z_low, ymax = z_hi))+
  geom_segment(aes(x=term, xend=term, y=-1.5, yend = 2.8), linetype = 3, color = "gray79")+
  geom_segment(aes(x=12, xend=12, y= -.05, yend = -1.5), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_segment(aes(x=12, xend=12, y= .05, yend = 1.5), arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  #geom_segment(aes(x=0.6, xend=0.6, y= -1.5, yend = 1))+
  geom_segment(aes(x=10, xend=10, y= 2.1, yend = 3),size = .37, arrow = arrow(length = unit(.02, "npc"), type = "closed"))+
  geom_errorbar(width = .2)+ 
  geom_hline(aes(yintercept = 0), linetype =2)+
  geom_point(aes(fill = term), size=5, show.legend = F, shape=21)+
  #geom_point(aes(x=10, y=-1.38), size=6, show.legend = F, shape="*")+
  geom_text(aes(x= term, y=-2, label = abs_text), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=4, label = label_text), family = "Roboto", size = 3.5)+
  geom_text(aes(x= term, y=5, label = label_p), family = "Roboto", size = 3.5)+
  annotate("text",
           x= c(12.4,12.4,12,12,12),
           y = c(-.75,.75, 4,5,-2),
           label = c("Favours usual care", "Favours exercise", "Absolute change \n (CI)", "P-value",
                     "Mean value \n(SD)"
           ),
           size = c(3.5),
           fontface = "bold", family = "Roboto"
  )+
  scale_y_continuous(breaks = seq(-1.5,3,.5))+
  scico::scale_fill_scico_d(palette = "oleron")+
  labs(y = "Normalized change from baseline (*z*)")+  
  ggthemes::theme_clean()+ 
  coord_flip(ylim = c(-1.55,6),
             xlim = c(0.6,13), 
             expand = F)+
  
  theme(axis.text.y = element_markdown(family = "Roboto"),
        axis.title.y = element_blank(),
        axis.title.x = element_markdown( hjust = .2),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank())
pixel40(17,22,3000)

ggsave(filename = 'Heart - Figure 4.pdf', device = cairo_pdf , height = 17, width = 22, units = "cm", dpi =830)
ggsave(filename = 'Figure 4.tiff', device = 'tiff' , height = 17, width = 22, units = "cm", dpi =830,compression = "lzw")

