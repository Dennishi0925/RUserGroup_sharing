library(tidyverse)
library(formattable)
options(stringsAsFactors = F)
# Sys.setlocale(locale = "UTF-8")
# Sys.setlocale(category = "LC_ALL", locale = "cht")
# rm(list=ls())

# NTUSC Attendance Data 台大學代出席資料 

### read data
df_sc_attnd <- read_csv("Sharing/data/NTUSC/df_sc_attnd_college.csv")
df_vote_college <- read_csv("Sharing/data/NTUSC/df_vote_college.csv")
df_candidate <- read_csv("Sharing/data/NTUSC/df_candidate_final.csv")
df_college_f <- read_csv("Sharing/data/NTUSC/df_college_join.csv")
df_sc_attnd_vis <- read_csv("Sharing/data/NTUSC/df_sc_attnd_vis.csv")

##### facet line chart

### Turnout Rate vs. Term, facetted by College

df_college_avg <-
  df_college_f %>%
  filter(term != "103-2") %>%
  group_by(term) %>%
  summarise(vote = sum(vote, na.rm = T), all = sum(all, na.rm = T)) %>%
  mutate(vote_rate = vote/all) %>%
  left_join(
    df_college_f %>%
      filter(term != "103-2") %>%
      group_by(college_english) %>%
      mutate(vote_rate_college_order = sum(vote, na.rm = T)/sum(all, na.rm = T)) %>%
      ungroup() %>%
      mutate(college_english = fct_reorder(as.factor(college_english), -vote_rate_college_order)) %>%
      distinct(term, college_english)
  )

df_college_f %>%
  filter(term != "103-2") %>%
  mutate(college_english_vote_rate = str_c(college_english,"_", as.character(vote_rate))) %>%
  ggplot(aes(term, vote_rate, group = college_english)) +
  geom_line(colour = "red2", size = .8) +
  facet_wrap(~college_english, ncol = 4, scale="free_x")

df_college_f %>%
  filter(term != "103-2") %>%
  group_by(college_english) %>%
  mutate(vote_rate_min = min(vote_rate, na.rm = TRUE), vote_rate_max = max(vote_rate, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(college_english_vote_rate = str_c(college_english,"_", as.character(vote_rate)),
         college_english_vote_rate_min = str_c(college_english,"_",  as.character(vote_rate_min)),
         college_english_vote_rate_max = str_c(college_english,"_",  as.character(vote_rate_max))) %>%
  group_by(college_english) %>%
  mutate(vote_rate_label = if_else(college_english_vote_rate == college_english_vote_rate_min | college_english_vote_rate == college_english_vote_rate_max, as.character(formattable::percent(vote_rate, digits = 0)), NA_character_)) %>%
  mutate(vote_rate_label_dot = if_else(college_english_vote_rate == college_english_vote_rate_min | college_english_vote_rate == college_english_vote_rate_max, vote_rate, as.double(NA))) %>%
  ungroup() %>%
  group_by(college_english) %>%
  mutate(vote_rate_college_order = sum(vote, na.rm = T)/sum(all, na.rm = T)) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -vote_rate_college_order)) %>%
  ggplot(aes(term, vote_rate, group = college_english)) +
  geom_line(colour = "red2", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0, 0.2),
                     breaks = c(0, 0.1, 0.2)) +
  facet_wrap(~college_english, ncol = 4, scale="free_x") +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=vote_rate_label),hjust=+0.5, vjust=-1, size=3) +
  geom_point(aes(term, vote_rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "Arial", face = "plain", size=12)) +
  labs(title = "Turnout Rate Diverged across Colleges in NTU Student Congress Election") +
  labs(subtitle = "College of Law and Social Sciences students were more willing to vote") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("vote rate") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain")) +
  geom_line(data=df_college_avg, aes(term, vote_rate), colour="grey50")

### Turnout Rate: College vs. Term
#投票率散點圖_五學期 vs.所有學院
#這邊不太適合用類組因為管院就有兩個類組

### Turnout Rate vs. Population, facetted by Term, filled by Uncontested Election
df_college_f %>% 
  rename(Voting_eligible_population = all) %>%
  filter(term != "103-2") %>%
  ggplot(aes(Voting_eligible_population, vote_rate, group = term)) +
  geom_point() +
  facet_wrap(~term, ncol = 5, scale="free_x")

df_college_f %>% 
  rename(Voting_eligible_population = all) %>%
  mutate(uncontested_election = if_else(competitive == "no", "uncontested", "contested")) %>%
  filter(term != "103-2") %>%
  ggplot(aes(Voting_eligible_population, vote_rate, group = term)) +
  geom_point() +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  scale_x_continuous(limits = c(0, 6000),
                     breaks = c(1000, 3000, 5000)) +
  facet_wrap(~term, ncol = 5, scale="free_x") +
  # facet_grid(~term, scale="free_x") +
  theme(panel.spacing.x = unit(.5, "lines")) + 
  theme(strip.background = element_rect(fill="white"),
        strip.placement = "inside") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", linetype = "dashed"),
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        panel.background = element_rect(fill = "#fafafa"),
        axis.line = element_line(colour = "black")) +
  geom_label(aes(label=college_english_ab, color = uncontested_election),hjust=+0.3, vjust=1.2, size=3, family = "Arial") +
  scale_color_manual( values=c("blue","red","#8abbd0","#e3120b"))+
  theme(strip.text.x = element_text(family = "Arial", face = "plain")) + 
  xlab("\nvoting-eligible population") +
  ylab("vote rate") +
  labs(title = "NTU Students Tended Not to Vote Nor Run for Student Congress") +
  labs(subtitle = "Uncontested Elections Occured more than Contested Elections ") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") +
  labs(color = "Uncontested Election") +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain"))


##### facet line chart
df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english, term) %>%
  count(Attnd) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  ungroup() %>%  
  filter(Attnd == "出席") %>%
  ggplot(aes(term, Attnd_Rate, group = college_english)) +
  geom_line(colour = "#bd3037", size = .8) +
  facet_wrap(~college_english, ncol = 3) 

### attendance rate average
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(
    df_sc_attnd_vis %>%
      select(college_english, term, Attnd) %>%
      filter(term != "103-2") %>%
      group_by(college_english, term, Attnd) %>%
      summarise(n = n()) %>% 
      mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
      ungroup() %>%
      group_by(college_english) %>%
      mutate(Attnd_Rate_college_order = sum(Attnd_yes, na.rm = T)/sum(n, na.rm = T))  %>%
      ungroup %>% 
      filter(Attnd == "出席") %>%
      mutate(college_english = fct_reorder(as.factor(college_english), -Attnd_Rate_college_order)) %>%
      distinct(term, college_english)
  )

df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(college_english, term) %>%
  count(Attnd) %>%
  mutate(Attnd_yes = if_else(Attnd == "出席", n, as.integer(0))) %>%
  mutate(Attnd_Rate = n/sum(n)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1"), as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1"), formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  group_by(college_english) %>%
  mutate(Attnd_Rate_college_order = sum(Attnd_yes, na.rm = T)/sum(n, na.rm = T))  %>%
  ungroup() %>% 
  filter(Attnd == "出席") %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -Attnd_Rate_college_order)) %>%
  add_row(college_english = "Life Science", term = "104-1", Attnd = "出席",
          n = 0, Attnd_yes = 0, Attnd_Rate =  formattable::percent(0), 
          Attnd_Rate_label = as.character(formattable::percent(0, digits = 0)),
          Attnd_Rate_label_dot = as.double(formattable::percent(0)),
          Attnd_Rate_college_order = as.double(0)) %>%
  add_row(college_english = "Life Science", term = "104-2", Attnd = "出席",
          n = 0, Attnd_yes = 0, Attnd_Rate =  NA, 
          Attnd_Rate_label = NA_character_,
          Attnd_Rate_label_dot = as.double(NA),
          Attnd_Rate_college_order = as.double(0)) %>%
  ggplot(aes(term, Attnd_Rate, group = college_english)) +
  geom_line(colour = "#bd3037", size = .8) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0, 1),
                     breaks = c(0, 0.5, 1)) + 
  facet_wrap(~college_english, ncol = 3) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=+2.5, size=3) +
  geom_point(aes(term, Attnd_Rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "Arial", face = "plain", size=12)) +
  labs(title = "Student Congress Members Assembly Attendance Rate Reached 50% at NTU") +
  labs(subtitle = "College of Management Congress Members Preserve Their Time for Internship instead of Assembly") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain")) +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

#出席率折線圖_五學期 vs.院 ~ 依照院分
df_sc_attnd_vis_avg <-
  df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(term) %>%
  summarise(Attnd_Rate_avg = sum(if_else(Attnd == '出席', n, as.integer(0)))/sum(n)) %>%
  left_join(distinct(df_sc_attnd_vis, term, grade)) %>%
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order))


df_sc_attnd_vis %>%
  filter(term != "103-2") %>%
  group_by(grade, term, Attnd) %>%
  count() %>%
  ungroup %>%
  group_by(grade, term) %>%
  mutate(Attnd_Rate = round(n/sum(n), 2)) %>%
  mutate(Attnd_Rate_label = if_else(term %in% c("104-1", "106-1") | grade == 1, as.character(formattable::percent(Attnd_Rate, digits = 0)), NA_character_)) %>%
  mutate(Attnd_Rate_label_dot = if_else(term %in% c("104-1", "106-1") | grade == 1, formattable::percent(Attnd_Rate), as.double(NA))) %>%
  ungroup() %>%  
  rename(grade_order = grade) %>%
  mutate(grade = case_when(grade_order == 1 ~ "freshman",
                           grade_order == 2 ~ "sophomore",
                           grade_order == 3 ~ "junior",
                           grade_order == 4 ~ "senior",
                           grade_order == 5 ~ "super senior")) %>%
  mutate(grade = reorder(as.factor(grade), grade_order)) %>%
  filter(Attnd == "出席") %>%
  # select(grade, term, Attnd, Attnd_Rate) %>%
  # filter(grade == 'j')
  ggplot(aes(term, Attnd_Rate, group = grade)) +
  geom_line(colour = "#bd3037", size = .8, group=1) +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L),
                     limits = c(0.35, 0.90),
                     breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + 
  facet_wrap(~grade, ncol = 5) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  # theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  geom_text(aes(label=Attnd_Rate_label),hjust=0.5, vjust=2, size=3) +
  geom_point(aes(term, Attnd_Rate_label_dot)) +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "Arial", face = "plain", size=12)) +
  labs(title = "Student Congress Members Attendance Rate Changed a Lot as Time Differed in Terms of Grade") + #有可能根本是缺席或出席的人年級上升了
  labs(subtitle = "Attendance Rate of Sophomores and Juniors were Relatively Stable") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain")) +
  geom_line(data=df_sc_attnd_vis_avg, aes(term, Attnd_Rate_avg), colour="grey50")

##### Stacked Bar chart

df_attnd_vote <-
  df_sc_attnd %>%
  count(college_english, term, start, end, name, Attnd) %>%
  unite(ID, college_english, term, start, end, name, sep = "_", remove = T) %>%
  spread(key = Attnd, value = n) %>%
  mutate(出席 = if_else(is.na(出席), as.integer(0), 出席),
           未請假缺席 = if_else(is.na(未請假缺席), as.integer(0), 未請假缺席),
           其他原因缺席 = if_else(is.na(其他原因缺席), as.integer(0), 其他原因缺席),
           請假缺席 = if_else(is.na(請假缺席), as.integer(0), 請假缺席)) %>%
  separate(col = ID, into = c("college_english", "term", "start", "end", "name"), sep = "_") %>%
  mutate(Attnd_all = 出席 + 未請假缺席 + 其他原因缺席 + 請假缺席) %>%
  mutate(Attnd_Rate = 出席/Attnd_all) %>%
  left_join(select(df_vote_college, -college_english), by = c("start", "end", "name"))


### Number of Candidates vs. Term, facetted by College, filled by Uncontested Election, colored by Elected
df_vote_college %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  ungroup() %>%
  count(start, college_english) %>%
  ggplot(aes(start, n)) +
  geom_col() +
  facet_wrap(~college_english, ncol = 4, scale="free_x")

df_vote_college %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  ungroup() %>%
  mutate(elected = if_else(is.na(elected), "lost", "won")) %>%
  mutate(uncontested_election = if_else(competitive == 0, "uncontested", "contested")) %>%
  count(start, college_english, uncontested_election, elected) %>%
  ggplot(aes(start, n, fill = elected, color = uncontested_election)) +
  geom_col() +
  scale_color_manual(values=c("black","white")) +
  facet_wrap(~college_english, ncol = 4, scale="free_x")

df_vote_college %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  group_by(college_english) %>%
  mutate(candidate_college_order = n()) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -candidate_college_order)) %>%
  mutate(elected = if_else(is.na(elected), "lost", "won")) %>%
  mutate(uncontested_election = if_else(competitive == 0, "uncontested", "contested")) %>%
  mutate(uncontested_election = as.factor(uncontested_election),
         elected = as.factor(elected)) %>%
  count(start, college_english, uncontested_election, elected) %>%
  add_row(start = "104-1", college_english = "Life Science", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  add_row(start = "105-1", college_english = "Life Science", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  add_row(start = "105-5", college_english = "Public Health", uncontested_election = "uncontested", elected = "lost", n = 0) %>%
  ggplot(aes(start, n, fill = elected, color = uncontested_election)) +
  geom_col() +
  scale_color_manual(values=c("black","white")) +
  facet_wrap(~college_english, ncol = 4, scale="free_x") +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  theme(legend.position = "bottom") +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "Arial", face = "plain", size=12)) +
  labs(title = "To Lose a Student Congress Election Was Not Easy at NTU") +
  labs(subtitle = "College of Law and Social Sciences Students Fought Relatively Hard for Being Elected") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\nterm") +
  ylab("candidate number") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain")) 


##### Scatter Plot

df_attnd_vote %>%
  mutate(college_english = if_else(str_detect(college_english, "Computer"), "EE & CS", if_else(str_detect(college_english, "Bio"), "Bio & Agr", college_english ))) %>%
  filter(start != "103-2") %>%
  group_by(college_english) %>%
  mutate(candidate_college_order = n()) %>%
  ungroup() %>%
  mutate(college_english = fct_reorder(as.factor(college_english), -candidate_college_order)) %>%
  mutate(elected = if_else(is.na(elected), "lost", "won")) %>%
  mutate(uncontested_election = if_else(competitive == 0, "uncontested", "contested")) %>%
  mutate(uncontested_election = as.factor(uncontested_election),
         elected = as.factor(elected)) %>% 
  mutate(competitive = as.factor(competitive)) %>% 
  ggplot(aes(college_support_rate , Attnd_Rate, color = uncontested_election)) +
  geom_point() +
  scale_y_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  scale_x_continuous(labels =  scales::percent_format(accuracy = 1L)) + 
  facet_wrap(.~uncontested_election, ncol = 4, scale="free_x") + 
  geom_smooth(method='lm',formula=y~x) +
  theme(panel.spacing.y = unit(1, "lines")) + 
  theme(strip.background = element_rect(fill="white")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 270)) + 
  theme(legend.position = "bottom") +
  theme(strip.placement = "inside",
        strip.text.x = element_text(family = "Arial", face = "plain", size=12)) +
  labs(title = "Failed to Find Relationships Between Election Support and Congress Attendance") +
  labs(subtitle = "On Average Congress Members Elected from Contested Election Attended More") +
  labs(caption = "Source: NTUSC Communique; Year: 2014 ~ 2016") + 
  xlab("\n(support vote / college total vote)") +
  ylab("attendance rate") +
  theme(axis.title = element_text(family = "Arial", face = "plain"), 
        legend.title = element_text(family = "Arial", face = "plain"), 
        legend.text = element_text(family = "Arial", face = "plain"), 
        plot.title = element_text(family = "Arial", face = "plain"), 
        plot.subtitle = element_text(family = "Arial", face = "italic"), 
        plot.caption = element_text(family = "Arial", face = "plain")) 


df_candidate %>% 
  mutate(elected = if_else(!is.na(elected), "yes", "not")) %>%
  mutate(elected = as.factor(elected)) %>%
  ggplot(aes(support, object, color = elected)) +
  geom_jitter() +
  scale_y_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300)) + 
  scale_x_continuous(limits = c(0, 300),
                     breaks = c(0, 100, 200, 300))
