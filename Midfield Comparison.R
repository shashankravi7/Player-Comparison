library(tidyverse)
library(scales)
library(ggpubr)


havertz <- read_csv("havertz.csv")
wirtz <- read_csv("Wirtz.csv")
sancho <- read_csv("sancho.csv")

calculate_per90_metrics <- function(data, player_name) {
  data_clean <- data %>%
    mutate(
      Min = as.numeric(Min),
      Gls = as.numeric(Gls),
      PK = as.numeric(PK),
      npxG = as.numeric(npxG),
      xAG = as.numeric(xAG)
    ) %>%
    filter(Min > 0 & !is.na(Min) & !is.na(Gls) & !is.na(npxG) & !is.na(xAG))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  
  metrics <- data.frame(
    player = player_name,
    NPG = (sum(data_clean$Gls, na.rm = TRUE) - sum(data_clean$PK, na.rm = TRUE)) / total_minutes * 90,
    npXG = sum(data_clean$npxG, na.rm = TRUE) / total_minutes * 90,
    xAG = sum(data_clean$xAG, na.rm = TRUE) / total_minutes * 90
  )
  
  return(metrics)
}

#shooting metrics
calculate_shots_metrics <- function(data, player_name) {
  data_clean <- data %>%
    mutate(
      Min = as.numeric(Min),
      Sh = as.numeric(Sh),
      SoT = as.numeric(SoT)
    ) %>%
    filter(Min > 0 & !is.na(Min) & !is.na(Sh) & !is.na(SoT))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  
  metrics <- data.frame(
    player = player_name,
    Shots = sum(data_clean$Sh, na.rm = TRUE) / total_minutes * 90,
    SoT = sum(data_clean$SoT, na.rm = TRUE) / total_minutes * 90
  )
  
  return(metrics)
}

# progressive actions metrics
calculate_progressive_metrics <- function(data, player_name) {
  data_clean <- data %>%
    mutate(
      Min = as.numeric(Min),
      PrgC = as.numeric(PrgC),
      SCA = as.numeric(SCA),
      GCA = as.numeric(GCA)
    ) %>%
    filter(Min > 0 & !is.na(Min) & !is.na(PrgC) & !is.na(SCA) & !is.na(GCA))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  
  metrics <- data.frame(
    player = player_name,
    PrgC = sum(data_clean$PrgC, na.rm = TRUE) / total_minutes * 90,
    SCA = sum(data_clean$SCA, na.rm = TRUE) / total_minutes * 90,
    GCA = sum(data_clean$GCA, na.rm = TRUE) / total_minutes * 90
  )
  
  return(metrics)
}

#Passing Metrics
calculate_passing_metrics <- function(data, player_name) {
  data_clean <- data %>%
    mutate(
      Min = as.numeric(Min),
      Ast = as.numeric(Ast),
      PrgP = as.numeric(PrgP),
      Cmp = as.numeric(Cmp),
      Att = as.numeric(Att),
      Cmp_pct = as.numeric(`Cmp%`)
    ) %>%
    filter(Min > 0 & !is.na(Min))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  

  assists_per90 <- sum(data_clean$Ast, na.rm = TRUE) / total_minutes * 90
  prog_passes_per90 <- sum(data_clean$PrgP, na.rm = TRUE) / total_minutes * 90
  passes_completed_per90 <- sum(data_clean$Cmp, na.rm = TRUE) / total_minutes * 90
  passes_attempted_per90 <- sum(data_clean$Att, na.rm = TRUE) / total_minutes * 90
  pass_completion_pct <- mean(data_clean$Cmp_pct, na.rm = TRUE)
  
  metrics <- data.frame(
    player = player_name,
    Ast_per90 = assists_per90,
    PrgP_per90 = prog_passes_per90,
    Passes_Completed_per90 = passes_completed_per90,
    Passes_Attempted_per90 = passes_attempted_per90,
    Pass_Completion_Pct = pass_completion_pct
  )
  
  return(metrics)
}


calculate_dribbling_metrics <- function(data, player_name) {
 
  col_names <- names(data)
  
  data_clean <- data %>%
    mutate(Min = as.numeric(Min)) %>%
    filter(Min > 0 & !is.na(Min))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  

  total_attempts <- 0
  if("attempted take ons" %in% col_names) {
    data_clean <- data_clean %>% mutate(attempts_clean = as.numeric(`attempted take ons`))
    total_attempts <- sum(data_clean$attempts_clean, na.rm = TRUE)
  } else if("Att" %in% col_names) {

    data_clean <- data_clean %>% mutate(attempts_clean = as.numeric(Att))
    total_attempts <- sum(data_clean$attempts_clean, na.rm = TRUE)
  }
  
  total_successes <- 0
  if("Succ" %in% col_names) {
    data_clean <- data_clean %>% mutate(Succ_clean = as.numeric(Succ))
    total_successes <- sum(data_clean$Succ_clean, na.rm = TRUE)
  }
  
  success_rate <- ifelse(total_attempts > 0, (total_successes / total_attempts) * 100, 0)
  
  metrics <- data.frame(
    player = player_name,
    Att_per90 = total_attempts / total_minutes * 90,
    Succ_per90 = total_successes / total_minutes * 90,
    Success_Rate = success_rate
  )
  
  return(metrics)
}

# conversion efficiency metrics
calculate_conversion_ratio <- function(data, player_name) {
  data_clean <- data %>%
    mutate(
      Min = as.numeric(Min),
      Gls = as.numeric(Gls),
      PK = as.numeric(PK),
      Sh = as.numeric(Sh),
      SoT = as.numeric(SoT)
    ) %>%
    filter(Min > 0 & !is.na(Min) & !is.na(Gls) & !is.na(Sh) & !is.na(SoT))
  
  total_minutes <- sum(data_clean$Min, na.rm = TRUE)
  total_goals <- sum(data_clean$Gls, na.rm = TRUE)
  total_penalties <- sum(data_clean$PK, na.rm = TRUE)
  total_npg <- total_goals - total_penalties
  total_shots <- sum(data_clean$Sh, na.rm = TRUE)
  total_sot <- sum(data_clean$SoT, na.rm = TRUE)
  
  shots_per_npg <- ifelse(total_npg > 0, total_shots / total_npg, 0)
  npg_conversion_rate <- ifelse(total_shots > 0, (total_npg / total_shots) * 100, 0)
  sot_conversion_rate <- ifelse(total_sot > 0, (total_npg / total_sot) * 100, 0)
  
  metrics <- data.frame(
    player = player_name,
    NPG_total = total_npg,
    Shots_total = total_shots,
    SoT_total = total_sot,
    Shots_per_NPG = shots_per_npg,
    NPG_Conversion_Rate = npg_conversion_rate,
    SoT_Conversion_Rate = sot_conversion_rate,
    NPG_per90 = total_npg / total_minutes * 90,
    Shots_per90 = total_shots / total_minutes * 90
  )
  
  return(metrics)
}

# Complete Player Summary
create_complete_midfield_summary <- function(data, player_name) {
  summary_data <- cbind(
    calculate_per90_metrics(data, player_name), 
    calculate_shots_metrics(data, player_name)[,-1],
    calculate_progressive_metrics(data, player_name)[,-1],
    calculate_passing_metrics(data, player_name)[,-1],
    calculate_dribbling_metrics(data, player_name)[,-1],
    calculate_conversion_ratio(data, player_name)[,-1]
  )
  return(summary_data)
}



# Havertz
havertz_summary <- create_complete_midfield_summary(havertz, "Havertz")

# Wirtz
wirtz_summary <- create_complete_midfield_summary(wirtz, "Wirtz")

# Sancho
sancho_summary <- create_complete_midfield_summary(sancho, "Sancho")


all_midfield_data <- rbind(
  havertz_summary,
  wirtz_summary,
  sancho_summary
)

print("Midfield Player Performance Summary:")
print(all_midfield_data)


write_csv(all_midfield_data, "Player_Performance_Midfield.csv")


