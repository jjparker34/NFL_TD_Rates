library(nflfastR)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggimage)


pbp  <- load_pbp(2023)
conversion <- calculate_series_conversion_rates(pbp, weekly = FALSE)

pbp_rp <- pbp %>%
  filter(pass == 1 | rush==1)%>%
  filter(!is.na(epa))

best_offenses <- conversion %>%
  group_by(team) %>%
  summarise(off_td, off_fg)

best_defenses <- conversion %>%
  group_by(team) %>%
  summarise(def_td, def_fg)

total_td_success <- best_offenses %>%
  left_join(best_defenses, by = 'team')

total_td_success <- total_td_success %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

total_td_success %>%
  ggplot(aes(x=off_td,y=def_td))+
  geom_hline(yintercept = mean(total_td_success$def_td))+
  geom_vline(xintercept = mean(total_td_success$off_td))+
  geom_smooth(method = 'lm')+
  geom_image(aes(image=team_logo_espn), size = .05, asp = 16/9)+
  xlab("Offensive TD Rate")+
  ylab("Defensive TD Rate")+
  scale_y_reverse()
