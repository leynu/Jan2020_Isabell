library(tidyverse)
library(readr)
library(here)


tbl1 <-
  paste0(here("namn-1998-2019"), "/", list.files(here("namn-1998-2019"), pattern = "*.csv"))[1:3] %>% 
  map_df(~cbind(read_delim(., 
                           delim = ";", 
                           col_types = 
                             cols_only(
                               X1 = col_double(),
                               X2 = col_character(),
                               X3 = col_character(),
                               X4 = col_character()),
                           escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE, skip = 8),
                year = str_extract_all(basename(.), "\\d{4}", simplify = T)
  )) %>% 
  select(rank_nr = X1, name = X2, count = X3, count_1000 = X4, year)

tbl2 <-
  paste0(here("namn-1998-2019"), "/", list.files(here("namn-1998-2019"), pattern = "*.csv"))[4:22] %>% 
  map_df(~cbind(read_delim(., 
                           delim = ";", 
                           col_types = 
                             cols_only(
                               X1 = col_double(),
                               X2 = col_character(),
                               X3 = col_character(),
                               X4 = col_character(),
                               X5 = col_character()),
                           escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE, skip = 8),
                year = str_extract_all(basename(.), "\\d{4}", simplify = T)
  )) %>% 
  select(rank_nr = X1, name = X3, count = X4, count_1000 = X5, year)

tbl3 <- read_delim("namn-1998-2019/Flickor 2020-Table 1.csv", 
                   delim = ";", 
                   col_types = 
                     cols_only(
                       X1 = col_double(),
                       X2 = col_character(),
                       X3 = col_character(),
                       X4 = col_character(),
                       X5 = col_character()),
                   escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE, skip = 13) %>% 
  mutate(year = 2020) %>% 
  select(rank_nr = X1, name = X3, count = X4, count_1000 = X5, year)


tbl <- rbind(tbl1, tbl2, tbl3) %>%
  filter(name %in% c("Isabella", "Isabelle")) %>% 
  arrange(desc(name)) %>% 
  mutate(count = as.numeric(count),
         year_num = as.numeric(year)) %>%
  group_by(year) %>%
  mutate(label_y = cumsum(count)) %>%
  ungroup()
  
my_background <- "#e2e8eb"

cbbPalette <- c("#ee5935", "#f6b042")
a <- c(rep("#4D4D4D", length(1998:2009)), "#ee5935", rep("#4D4D4D", length(2011:2020)))

table(tbl$name)





ggplot(tbl,
       aes(x = year, y = count, fill=name)) +
  geom_bar(stat="identity") +
  labs(title = "Isabell",
       subtitle = "You always will be my one and only",
       caption = c("Data source: Statistics Sweden. The number of girls named Isabella/Isabelle between 1998 and 2020.\nTheir ranking within each year is also provided.", "\n\n@leynu | Jan 2022")) +
  theme_bw() + 
  theme(plot.title = element_text(size = rel(2), lineheight = .9, 
                                  face = "bold",
                                  color = "#4D4D4D"),
        plot.subtitle = element_text(size = rel(1.15), 
                                     face = "italic",
                                     color = "#4D4D4D"),
        plot.caption = element_text(size = rel(0.75), 
                                    color = "#4D4D4D",
                                    hjust=c(0, 1)),
        plot.caption.position =  "plot",
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.border = element_blank(),
        plot.background = element_rect(fill = my_background),
        panel.background = element_rect(fill = my_background),
        legend.justification='right',
        legend.direction='vertical',
        legend.position = c(1, 1.08),
        legend.title = element_blank(),
        legend.text = element_text(color = "#4D4D4D"),
        legend.background = element_rect(fill = my_background),
        legend.key = element_rect(fill = my_background),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text=element_text(size=10, color = "#4D4D4D"),
        axis.text.x = element_text( angle = 45, vjust = 1.3, 
                                    hjust = 1, 
                                    colour = a
                                    )
        ) + 
  scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0, 1200)) +
  expand_limits(x = 0, y = 0) +
  scale_fill_manual(values=cbbPalette) +
  geom_text(aes(y = label_y, label = rank_nr), 
            vjust = 1.5, 
            hjust = 0.5, 
            colour = "#4D4D4D",
            size = 3.25) 



