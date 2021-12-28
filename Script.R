# Carregando pacotes --------------------------------------------------------------------------
library(readr)
library(tidyverse)
library(tidyr)
library(ggthemes)
library(patchwork)

# Lendo a base --------------------------------------------------------------------------------

brasil <- read_csv("C:/Users/saulo/Downloads/multiTimeline.csv",
                   skip = 1) |>
  janitor::clean_names()

glimpse(brasil)

mundo  <- read_csv("C:/Users/saulo/Downloads/multiTimeline (1).csv",
                   skip = 1) |>
  janitor::clean_names()

glimpse(mundo)

# Juntando as bases ---------------------------------------------------------------------------

base <- bind_cols(brasil, mundo)

glimpse(base)

# Arrumando o nome das colunas ----------------------------------------------------------------

base_ajustada <-
  base |>
  select(!semana...7) |> # tirando a coluna duplicada da data
  rename(
    Semana = semana...1,
    "R no Brasil" = r_brasil,
    "Python no Brasil" = python_brasil,
    "SPSS no Brasil" = spss_brasil,
    "GraphPad no Brasil" = graph_pad_prism_brasil,
    "SAS no Brasil" = sas_brasil,
    "R no mundo" =r_todo_o_mundo,
    "Python no mundo" = python_todo_o_mundo,
    "SPSS no mundo" = spss_todo_o_mundo,
    "GraphPad no mundo" = graph_pad_prism_todo_o_mundo,
    "SAS no mundo" = sas_todo_o_mundo
  ) |> # ajustando o tipo das colunas - character -> numeric
  mutate(
    `SPSS no Brasil` = as.numeric(`SPSS no Brasil`),
    `GraphPad no Brasil` = as.numeric(`GraphPad no Brasil`),
    `SAS no Brasil` = as.numeric(`SAS no Brasil`),
    `GraphPad no mundo` = as.numeric(`GraphPad no mundo`)
  )

glimpse(base_ajustada)

# gravando a base ajustada no projeto ---------------------------------------------------------

write.csv(x = base_ajustada,file = "base_ajustada")

# plot ----------------------------------------------------------------------------------------

# Explicação dos valores

# Interesse ao longo do tempo os números representam o interesse de pesquisa relativo ao ponto
# mais alto no gráfico de uma determinada região em um dado período. Um valor 0 de 100 representa
# o pico de popularidade de um termo. Um valor de 50 significa que o termo teve metade da
# popularidade. Uma pontuação de 0 significa que não havia dados suficientes sobre o termo.

# Brasil

g1 <- base_ajustada |>
  ggplot(mapping = aes(x = Semana)) +
  geom_point(mapping = aes(y = `R no Brasil`, col = "`R no Brasil`")) +
  geom_line(mapping = aes(y = `R no Brasil`, col = "`R no Brasil`")) +
  geom_point(mapping = aes(y = `Python no Brasil`, col = "`Python no Brasil`")) +
  geom_line(mapping = aes(y = `Python no Brasil`, col = "`Python no Brasil`")) +
  geom_point(mapping = aes(y = `Python no Brasil`, col = "`Python no Brasil`")) +
  geom_point(mapping = aes(y = `SPSS no Brasil`, col = "`SPSS no Brasil`")) +
  geom_line(mapping = aes(y = `SPSS no Brasil`, col = "`SPSS no Brasil`")) +
  geom_point(mapping = aes(y = `GraphPad no Brasil`, col = "`GraphPad no Brasil`")) +
  geom_line(mapping = aes(y = `GraphPad no Brasil`, col = "`GraphPad no Brasil`")) +
  geom_point(mapping = aes(y = `SAS no Brasil`, col = "`SAS no Brasil`")) +
  geom_line(mapping = aes(y = `SAS no Brasil`, col = "`SAS no Brasil`")) +
  labs(
    title = "Popularidade entre os softwares utilizados para análise de dados",
    subtitle = "Interesse da população brasileira",
    y = "Escore de popularidade (0 - 100)"
  ) +
  guides(color=guide_legend(title = "")) +
  theme_wsj(color = "brown",
            title_family = "mono",
            base_family = "sans")

# Mundo

g2 <- base_ajustada |>
  ggplot(mapping = aes(x = Semana)) +
  geom_point(mapping = aes(y = `R no mundo`, col = "`R no mundo`")) +
  geom_line(mapping = aes(y = `R no mundo`, col = "`R no mundo`")) +
  geom_point(mapping = aes(y = `Python no mundo`, col = "`Python no mundo`")) +
  geom_line(mapping = aes(y = `Python no mundo`, col = "`Python no mundo`")) +
  geom_point(mapping = aes(y = `Python no mundo`, col = "`Python no mundo`")) +
  geom_point(mapping = aes(y = `SPSS no mundo`, col = "`SPSS no mundo`")) +
  geom_line(mapping = aes(y = `SPSS no mundo`, col = "`SPSS no mundo`")) +
  geom_point(mapping = aes(y = `GraphPad no mundo`, col = "`GraphPad no mundo`")) +
  geom_line(mapping = aes(y = `GraphPad no mundo`, col = "`GraphPad no mundo`")) +
  geom_point(mapping = aes(y = `SAS no mundo`, col = "`SAS no mundo`")) +
  geom_line(mapping = aes(y = `SAS no mundo`, col = "`SAS no mundo`")) +
  labs(
    subtitle = "Interesse da população mundial",
    caption = "Fonte - Google Trends",
    y = "Escore de popularidade (0 - 100)"
  ) +
  guides(color=guide_legend(title = "")) +
  theme_wsj(color = "brown",
            title_family = "mono",
            base_family = "sans")

# Rearrange -----------------------------------------------------------------------------------

fig <- g1/g2

ggsave(filename = "figura",
       plot = fig,device = "png")


