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
