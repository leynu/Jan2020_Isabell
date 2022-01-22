library(tidyverse)
library(readr)
library(here)
library(directlabels)
library(cowplot)
library(magick)

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

tbl_sum <- tbl %>% 
  group_by(year) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(name = "Isabella &\nIsabelle")

tbl_all <- bind_rows(tbl, tbl_sum) %>% 
  mutate(year_num = as.numeric(as.character(year)))


my_background <- "#e2e8eb"

cbbPalette <- c("#ee5935", "#4D4D4D", "#f6b042")
a <- c(rep("#4D4D4D", length(1998:2009)), "#ee5935", rep("#4D4D4D", length(2011:2020)))

table(tbl$name)

theme_set(theme_cowplot())

p <- ggplot(tbl_all,
       aes(x = as.numeric(year), y = count, group=name, color=name)) +
  geom_line(position="identity", size = 1.8) +
  geom_dl(aes(label = name), method = list(dl.trans(x = x + 0.3), "last.points", cex = 0.9)) +
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off') +
  # The vertical line in the plot
  geom_segment(aes(x = 2010, y = 0, xend = 2010, yend = 1114), 
               col = "#ee5935", 
               size = 0.75) +
  annotate(geom="text", x=2010, y=1196, 
           label=expression(paste("The year", ~ bold("Isabell"))),
                            color="#4D4D4D", hjust=0.04, 
           size = 3.5) +
  annotate(geom="text", x=2010, y=1152, 
         label="was born",
         color="#4D4D4D", hjust=0.04, 
         size = 3.5) +
  geom_point(position="identity", 
             aes(colour = factor(name)),
             shape = 21, fill = "white", size = 2, stroke = 2) +
  labs(title = "Isabell",
       subtitle = "You always will be my one and only",
       caption = "Data source: Statistics Sweden. The number of girls named Isabella/Isabelle between 1998 and 2020.\n@leynu | Jan 2022") +
  theme_bw() + 
  # Remove legend & adjust margins to give more space for labels
  # Remember, the margins are t-r-b-l
  theme(legend.position = "none",
        plot.margin = margin(0.3, 1.7, 0.2, 0.2, "cm"),
        plot.title = element_text(hjust = -0.11,
                                  size = rel(2), 
                                  face = "bold",
                                  color = "#4D4D4D"),
        plot.subtitle = element_text(hjust = -0.26, 
                                     size = rel(1.25), 
                                     face = "italic",
                                     color = "#4D4D4D"),
        plot.caption = element_text(size = rel(0.75),
                                    color = "#4D4D4D",
                                    hjust=0),
        plot.caption.position =  "plot",
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.x=element_blank(), 
        panel.border = element_blank(),
        plot.background = element_rect(fill = my_background),
        panel.background = element_rect(fill = my_background),
        legend.justification='right',
        legend.direction='vertical',
        legend.title = element_blank(),
        legend.text = element_text(color = "#4D4D4D"),
        legend.background = element_rect(fill = my_background),
        legend.key = element_rect(fill = my_background),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(colour = a,
                                   angle = 45, 
                                   vjust = 1.5, 
                                   hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text=element_text(size=10, color = "#4D4D4D")
        ) + 
  scale_x_continuous(breaks = seq(1998, 2020, 1)) + 
  scale_y_continuous(breaks = seq(0, 1200, 200), limits = c(0, 1200)) +
  scale_color_manual(values=cbbPalette) 
p

ggdraw() +
  draw_plot(p) +
  draw_image(file.path(here("pngaaa.com-4475818.png")),
             x = 0.425, y = 0.4, scale = .16) 





