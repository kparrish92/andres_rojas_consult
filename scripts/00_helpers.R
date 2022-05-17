run_t_test = function(df, p_id, segment)
{
  t1 <- df %>% 
    filter(participant == p_id) %>% 
    filter(is_trill == segment) %>% 
    filter(time == "PRE")
  
  t2 <- df %>% 
    filter(participant == p_id) %>% 
    filter(is_trill == segment) %>% 
    filter(time == "POST")
  
  t1_na <- t1 %>% 
    filter(duration == "N/A" | duration == "N/A /d/")
  
  t2_na <- t2 %>% 
    filter(duration == "N/A" | duration == "N/A /d/")
  
  remove_list <- rbind(t1_na, t2_na)
  
  t2_filt = t2 %>% 
    filter(!trial %in% remove_list$trial)
  
  t1_filt = t1 %>% 
    filter(!trial %in% remove_list$trial)
  
  t2_filt = t2_filt %>% filter(trial %in% t1_filt$trial)
  
  t1_filt = t1_filt %>% filter(trial %in% t2_filt$trial)
  
  t1_filt$duration <- as.numeric(t1_filt$duration)
  t2_filt$duration <- as.numeric(t2_filt$duration)
  
  
  test <- t.test(t2_filt$duration, t1_filt$duration, paired = TRUE)
  output_df = data.frame("Participant" = p_id,
             "tap_or_trill" = segment,
                "p_val" = test$p.value,
                "df" = test$parameter,
                "t_val" = test$statistic, 
                "estimate" = test$estimate, 
                "ci_lo" = test$conf.int[1],
                "ci_hi" = test$conf.int[2])
  return(output_df)
}


#model = srt_trill_cat
#effect = "timePOST"
#task = "Picture Naming Task"
#taps = "Taps"
#continuous = "Continuous"

plot_re = function(model, effect, task, taps, continuous)
{
  fixef_df = fixef(model) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() 
  
  fixef = fixef_df$timePOST
  fixef_2 = fixef_df$timePOST + fixef_df$`timePOST:levelsp102`
  fixef_3 = fixef_df$timePOST + fixef_df$`timePOST:levelsp203`
  
  
  group_101 = c("sp1_P1E", "sp1_P3E", "sp1_P5E", "sp1_P6E", "sp1_P7E",
                "sp1_P8E")
  group_102 = c("sp102_P1E", "sp102_P2E", "sp102_P3E")
  group_203 = c("sp203_P1E", "sp203_P2E", "sp203_P3E", "sp204_P2E")
  
  ranef_df_1 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_101) %>% 
    mutate(re_adjusted = as.numeric(fixef) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df_2 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_102) %>% 
    mutate(re_adjusted = as.numeric(fixef_2) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df_3 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_203) %>% 
    mutate(re_adjusted = as.numeric(fixef_3) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df = rbind(ranef_df_1, ranef_df_2, ranef_df_3)
  
  
  ranef_df$grp <- factor(ranef_df$grp, 
                         levels = 
                           c("sp1_P1E",
                             "sp1_P3E", 
                             "sp1_P5E", 
                             "sp1_P6E",
                             "sp1_P7E",
                             "sp1_P8E",
                             "sp102_P1E",
                             "sp102_P2E",
                             "sp102_P3E",
                             "sp203_P1E",
                             "sp203_P2E",
                             "sp203_P3E",
                             "sp204_P2E"
                           ))
  
  plot = ranef_df %>% 
    ggplot(aes(y = grp, x = re_adjusted, fill = as.factor(color))) +
    scale_fill_manual(values=c("#fa6652", "#0dbaff")) +
    geom_pointrange(aes(xmin = re_hi, xmax = re_lo), size = .4, shape = 21) + geom_vline(xintercept = 0, linetype = "dashed") +
    ylab("Participant ID") + xlab("Effect per participant") + 
    xlim(-100, 100) +
    theme(text=element_text(size=10, family="Times")) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(
            size = 0.1, 
            linetype = 'solid',
            colour = "grey"),
          legend.position = "none") + geom_text(data = mutate_if(ranef_df, is.numeric, round, 2),
                                                aes(label = paste(re_adjusted, "[",
                                                                  re_lo, "-", re_hi,
                                                                  "]"), x = Inf), 
                                                hjust = "inward", family = "Times", size = 3) +
    labs(title = paste(task, paste(taps)),
         subtitle = "Difference between PRE and POST Test", 
         caption = paste(continuous))
  
  return(plot)
}

plot_re_2 = function(model, effect, task, taps, continuous)
{
  fixef_df = fixef(model) %>% 
    as.data.frame() %>% 
    t() %>% 
    as.data.frame() 
  
  fixef = fixef_df$timePOST
  fixef_2 = fixef_df$timePOST 
  fixef_3 = fixef_df$timePOST 
  
  
  group_101 = c("sp1_P1E", "sp1_P3E", "sp1_P5E", "sp1_P6E", "sp1_P7E",
                "sp1_P8E")
  group_102 = c("sp102_P1E", "sp102_P2E", "sp102_P3E")
  group_203 = c("sp203_P1E", "sp203_P2E", "sp203_P3E", "sp204_P2E")
  
  ranef_df_1 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_101) %>% 
    mutate(re_adjusted = as.numeric(fixef) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df_2 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_102) %>% 
    mutate(re_adjusted = as.numeric(fixef_2) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df_3 = brms::ranef(model) %>% 
    as.data.frame() %>% 
    filter(term == effect) %>% 
    filter(grp %in% group_203) %>% 
    mutate(re_adjusted = as.numeric(fixef_3) + as.numeric(condval)) %>% 
    mutate(re_hi = re_adjusted + condsd) %>% 
    mutate(re_lo = re_adjusted - condsd) %>% 
    mutate("color" = 
             case_when(re_adjusted > 0 ~ 1,
                       re_adjusted < 0 ~ 0))
  
  ranef_df = rbind(ranef_df_1, ranef_df_2, ranef_df_3)
  
  
  ranef_df$grp <- factor(ranef_df$grp, 
                         levels = 
                           c("sp1_P1E",
                             "sp1_P3E", 
                             "sp1_P5E", 
                             "sp1_P6E",
                             "sp1_P7E",
                             "sp1_P8E",
                             "sp102_P1E",
                             "sp102_P2E",
                             "sp102_P3E",
                             "sp203_P1E",
                             "sp203_P2E",
                             "sp203_P3E",
                             "sp204_P2E"
                           ))
  
  plot = ranef_df %>% 
    ggplot(aes(y = grp, x = re_adjusted, fill = as.factor(color))) +
    scale_fill_manual(values=c("#fa6652", "#0dbaff")) +
    geom_pointrange(aes(xmin = re_hi, xmax = re_lo), size = .4, shape = 21) + geom_vline(xintercept = 0, linetype = "dashed") +
    ylab("Participant ID") + xlab("Effect per participant") + 
    xlim(-5, 1) +
    theme(text=element_text(size=10, family="Times")) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(
            size = 0.1, 
            linetype = 'solid',
            colour = "grey"),
          legend.position = "none") + geom_text(data = mutate_if(ranef_df, is.numeric, round, 2),
                                                aes(label = paste(re_adjusted, "[",
                                                                  re_lo, "-", re_hi,
                                                                  "]"), x = Inf), 
                                                hjust = "inward", family = "Times", size = 3) +
    labs(title = paste(task, paste(taps)),
         subtitle = "Difference between PRE and POST Test", 
         caption = paste(continuous))
  
  return(plot)
}


