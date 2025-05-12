# пакеты и дф
library(dplyr)
library(psych)
library(ggplot2)
library(nortest)
library(patchwork)
library(ggcorrplot)
library(ggpubr)
library(Hmisc)
library(reshape2)
library(rlang)
library(corrplot)
library(purrr)
library(FSA)
library(coin)
library(stringr)

df <- read.csv("/home/phiia_nec/working/Diploma/anketa_detailed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df <- df %>%
  mutate(faculty = str_trim(faculty)) 
df$format <- as.factor(df$format)
# View(df)

df_filtered <- df %>%
  select(starts_with("MC"), starts_with("AR"))
write.csv(df_filtered, "filtered_data.csv", row.names = FALSE)

# альфа Кронбаха
# для MC (все пункты)
alpha_MC_total <- alpha(df[, 1:14])
print(alpha_MC_total$total$raw_alpha)  # Общий α

# Для опросника AR (все пункты)
alpha_AR_total <- alpha(df[, c("AR1", "AR2", "AR3", ...)])
print(alpha_AR_total$total$raw_alpha)




# Описательные статистики 
num_vars <- c("PN", "MN", "RFL", "MC", "RA", "IC", "A", "AR", 
                   "SAM", "MMW", "DMP", "ST1", "ST2", "ST3", 
                   "ST4", "ST5", "ST6", "age")
desc_stats <- function(data, vars) {
  stats <- psych::describe(data[vars])[c("mean", "sd", "min", "max")]
  colnames(stats) <- c("M", "SD", "Min", "Max")
  round(stats, 2)
}
desc_stats(df, num_vars)


# Проверка нормальности по критериям
normality_tests <- function(data, vars) {
  res <- lapply(vars, function(var) {
    x <- na.omit(data[[var]])
    sw <- shapiro.test(x)
    ks <- ks.test(scale(x), "pnorm")
    
    data.frame(
      Variable = var,
      Test = c("Shapiro-Wilk", "Kolmogorov-Smirnov"),
      Statistic = c(round(sw$statistic, 2), round(ks$statistic, 2)),
      p.value = ifelse(c(sw$p.value, ks$p.value) < 0.001, 
                       "<.001", 
                       format(c(sw$p.value, ks$p.value), digits = 3, nsmall = 3))
    )
  })
  do.call(rbind, res)
}
normality_tests(df, c("MC", "AR", "DMP"))  # основные переменные
normality_tests(df, c("PN", "MN", "RFL", "RA", "IC", "A", "age")) # подшкалы




# Графики плотности и гистограммы распределений
create_dist_plot <- function(data, var, var_names, discrete_vars = c()) {
  x <- na.omit(data[[var]])
  is_discrete <- var %in% discrete_vars
  
  if (is_discrete) {
    ggplot(data.frame(x = factor(x)), aes(x)) +
      geom_bar(fill = "#00CED1", color = "black") +
      geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 3) +
      labs(title = var_names[var], x = "", y = "Частота") +
      theme_minimal()
  } else {
    ggplot(data.frame(x), aes(x)) +
      geom_density(fill = "#00CED1", alpha = 0.5) +
      stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)),
                    color = "#9400D3", size = 1) +
      labs(title = var_names[var], x = "", y = "Плотность") +
      theme_minimal()
  }
}

main_var_names <- c("MC" = "Метакогниция", "AR" = "Произвольная регуляция", 
  "DMP" = "Процент многозадачности", "SAM" = "Самооценённая многозадачность",
  "MMW" = "Желание усилить многозадачность") # основные переменные

sub_var_names <- c("PN" = "Планирование", "MN" = "Мониторинг", 
  "RFL" = "Рефлексивность", "RA" = "Регуляция активности",# подшкалы
  "IC" = "Тормозный контроль", "A" = "Внимание", "age" = "Возраст")

# построение 
main_dplots <- lapply(names(main_var_names), \(v)  create_dist_plot(df, v, main_var_names, discrete_vars = c("SAM", "MMW")))
sub_dplots <- lapply(names(sub_var_names), \(v) create_dist_plot(df, v, sub_var_names))

wrap_plots(main_dplots, ncol = 3) # отображение
wrap_plots(sub_dplots, ncol = 3)



# Общая корреляционная матрица 
# переменные для корреляции
corr_vars <- c('PN', 'MN', 'RFL', 'RA', 'IC', 'A', 'SAM', 'MMW', 'DMP', 'ST1', 'ST2', 'ST3', 'ST4', 'ST5', 'ST6')

cor_stats <- Hmisc::rcorr(as.matrix(df[corr_vars]), type = "spearman") # расчёт
cor_mat <- t(cor_stats$r) # корреляции
p_mat <- t(cor_stats$P) # p-значения

ggcorrplot( # визуализация
  cor_mat, method = "square", type = "lower",
  colors = c("#E757EE", "white", "#5eee57"),
  ggtheme = theme_dark(), outline.color = "black",
  lab = TRUE, lab_size = 3,
  p.mat = p_mat, sig.level = 0.05, insig = "blank",
  show.diag = FALSE
) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank()
  )




# Корреляции по группам
create_cor_plot <- function(data, format_type) {
  df_filt <- filter(data, format == format_type)
  cor_stats <- rcorr(as.matrix(df_filt[corr_vars]), type = "spearman")
  cor_pmat <- cor_stats$P
  
  ggcorrplot(
    t(cor_stats$r), method = "square", type = "lower",
    colors = c("#E757EE", "white", "#5eee57"),
    ggtheme = theme_dark(), outline.color = "black",
    lab = TRUE, lab_size = 3,
    p.mat = t(cor_stats$P), sig.level = 0.05, insig = "blank",
    show.diag = FALSE
  ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank()
    )
 print(cor_pmat)
}

online_plot  <- create_cor_plot(df, "online") # построение
offline_plot <- create_cor_plot(df, "offline")
hybrid_plot  <- create_cor_plot(df, "hybrid")

online_plot # вывод
offline_plot
hybrid_plot




# Межгрупповые сравнения Kruskal-Wallis
get_kruskal <- function(var, data) {
  test <- kruskal.test(reformulate("format", var), data)
  p <- test$p.value
  data.frame(
    Variable = var,
    H = round(test$statistic, 2),
    p.value = ifelse(p < 0.001, "<.001", round(p, 3)),
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      TRUE ~ "")
    )
}

kruskal_results <- do.call(rbind, lapply(num_vars, get_kruskal, df))
print(kruskal_results, row.names = FALSE)




# Post-hoc
post_hoc <- lapply(combn(levels(df$format), 2, simplify = FALSE), function(p) {
  t <- independence_test(A ~ format, 
                         data = df[df$format %in% p,],
                         distribution = approximate(10000)) # пермутации!
  p_val <- pvalue(t)
  data.frame(
    Comparison = paste(p, collapse = " vs "),
    Z = round(statistic(t), 3),
    p = round(p_val, 3),
    p.adj = round(p.adjust(p_val, "BH"), 3), # FDR-поправка
    Sig = ifelse(p.adjust(p_val, "BH") < 0.001, "***",
                 ifelse(p.adjust(p_val, "BH") < 0.01, "**",
                        ifelse(p.adjust(p_val, "BH") < 0.05, "*", "")))
  )
})
do.call(rbind, post_hoc)

# без выброса A = 8 в офлайн группе
df_filtered <- subset(df, !(format == "offline" & A == 8))
post_hoc_filtered <- lapply(combn(levels(df_filtered$format), 2, simplify = FALSE), function(p) {
  t <- independence_test(A ~ format, 
                         data = df_filtered[df_filtered$format %in% p,],
                         distribution = approximate(10000))
  p_val <- pvalue(t)
  data.frame(
    Comparison = paste(p, collapse = " vs "),
    Z = round(statistic(t), 3),
    p = round(p_val, 3),
    p.adj = round(p.adjust(p_val, "BH"), 3),
    Sig = ifelse(p.adjust(p_val, "BH") < 0.001, "***",
                 ifelse(p.adjust(p_val, "BH") < 0.01, "**",
                        ifelse(p.adjust(p_val, "BH") < 0.05, "*", "")))
  )
})
do.call(rbind, post_hoc_filtered)








# вайолин-плоты
# если с выбросом - то просто df; без выброса - subset(df, !(format == "offline" & A == 8))
ggplot(subset(df, !(format == "offline" & A == 8)), aes(format, A, fill = format)) +
  geom_violin(alpha = 0.7, width = 0.8, trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) + # мини-боксплоты
  geom_signif(
    comparisons = list(c("offline", "online")), 
    annotations = "*",                     
    y_position = max(df$A) * 1.2, # позиция по вертикали
    tip_length = 0.04, # усы скобки
    textsize = 5) + # размер звёздочки                            
  labs(x = "Группа", 
       y = "Внимание") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11), # увеличиваем шрифт
        axis.text.y = element_text(size = 11)) +
  scale_fill_manual(values = c("#556270", "#00CED1", "#9400D3")) +
  scale_x_discrete(labels = c("hybrid" = "Гибрид", 
                              "offline" = "Офлайн", 
                              "online" = "Онлайн"))



for (f in unique(df$format)) {
  min_val <- min(df$A[df$format == f])
  max_val <- max(df$A[df$format == f])
  cat(sprintf("min of %s is %f\n", f, min_val))
  cat(sprintf("max of %s is %f\n\n", f, max_val))
}
boxplot(A ~ format, data = df)
