library(dplyr)
library(ggplot2)
options(scipen = 999)

# natinoal debt df 
base_url = 'https://www.transparency.treasury.gov/services/api/fiscal_service/v1/accounting/od/debt_outstanding?'
full_url = paste0(base_url,
                  'format=csv',
                  '&page[size]=500')

debt <- read.csv(full_url) %>%
        group_by(reporting_calendar_year) %>%
        summarise(debt = sum(debt_outstanding_amt)) %>%
        mutate(year = reporting_calendar_year,
               debtb = debt/1000000000) %>%
        select(year, debt, debtb) %>%
        data.frame()

# presidential df
prez <- read.csv('data/presidents.csv')

# joining debt and party info
df <- debt %>%
      left_join(prez, by = 'year') %>%
      filter(year>1960)

# percent growth
growth <- df %>%
  group_by(president, party) %>%
  summarise(start_year = min(year),
            pct_increase = round(((max(debtb) - min(debtb))/min(debtb))*100,2)) %>%
  arrange(start_year)

# visual
ggplot(data = df, aes(x=year, y=debtb)) +
  geom_rect(aes(xmin = year, xmax = dplyr::lead(year), ymin = -0.5, ymax = Inf, fill = party)) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_line(color = "white", size = 1.75) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x="Year", y="National Deficit (billions)",
       fill = "White House")

ggsave("images/deficit.png")




