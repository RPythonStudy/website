
################################################################################
## 함수명: my_histgram_with_outliers 
## 제작일: 2024-07-17
## 인  자: 데이터프레임과 컬럼명
## 반환값: outlier를 표시한 히스토그램
################################################################################
my_histgram_with_outliers <- function(df, column_name) {
  
  library(ggplot2)
  # 데이터 준비
  
  data <- df[[column_name]]
  
  # 평균과 표준편차 계산
  mean_value <- mean(data, na.rm = TRUE)
  sd_value <- sd(data, na.rm = TRUE)
  
  # 이상치 범위 계산
  lower_bound <- mean_value - 3 * sd_value
  upper_bound <- mean_value + 3 * sd_value
  
  # 데이터에 이상치 여부 표시
  df$outlier <- ifelse(data < lower_bound | data > upper_bound, "outlier", "normal")
  
  # 히스토그램 그리기
  p <- ggplot(df, aes_string(x = column_name, fill = "outlier")) +
    geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "identity") +
    scale_fill_manual(name = "Legend", values = c("normal" = "blue", "outlier" = "red")) +
    geom_point(data = subset(df, outlier == "outlier"), aes_string(x = column_name, y = 0), 
               color = "red", size = 4, shape = 21) +
    geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = lower_bound, color = "3 Std Dev Below"), linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = upper_bound, color = "3 Std Dev Above"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "Statistics", values = c("Mean" = "green", "3 Std Dev Below" = "red", "3 Std Dev Above" = "red")) +
    labs(title = paste("Histogram of", column_name),
         x = column_name,
         y = "Frequency") +
    theme_minimal()
  
  # 이상치 값 출력
  # outliers <- df[data < lower_bound | data > upper_bound, column_name]
  # cat("Outliers (values that are more than 3 standard deviations from the mean):\n")
  # print(outliers)
  
  return (p)
}


my_histogram_log_transformation_with_outliers <- function(df, column_name) {
  
  library(ggplot2)
  # 원본 데이터 저장
  original_data <- df[[column_name]]
  
  # NA 값을 포함한 행을 삭제
  df <- df[complete.cases(df), ]
  
  # 데이터의 해당 컬럼을 로그 변환
  df[[column_name]] <- log(df[[column_name]])
  
  # 로그 변환된 데이터 준비
  data <- df[[column_name]]
  
  # 로그 변환된 값의 평균과 표준편차 계산
  mean_value <- mean(data, na.rm = TRUE)
  sd_value <- sd(data, na.rm = TRUE)
  
  # 이상치 범위 계산
  lower_bound <- mean_value - 3 * sd_value
  upper_bound <- mean_value + 3 * sd_value
  
  # 데이터에 이상치 여부 표시
  df$outlier <- ifelse(data < lower_bound | data > upper_bound, "outlier", "normal")
  
  # 히스토그램 그리기
  p <- ggplot(df, aes_string(x = column_name, fill = "outlier")) +
    geom_histogram(binwidth = 0.1, color = "black", alpha = 0.7, position = "identity") +
    scale_fill_manual(name = "Legend", values = c("normal" = "blue", "outlier" = "red")) +
    geom_point(data = subset(df, outlier == "outlier"), aes_string(x = column_name, y = 0), 
               color = "red", size = 4, shape = 21) +
    geom_vline(aes(xintercept = mean_value, color = "Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = lower_bound, color = "3 Std Dev Below"), linetype = "dotted", size = 1) +
    geom_vline(aes(xintercept = upper_bound, color = "3 Std Dev Above"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "Statistics", values = c("Mean" = "green", "3 Std Dev Below" = "red", "3 Std Dev Above" = "red")) +
    labs(title = paste("Histogram of Log-transformed", column_name),
         x = paste("Log of", column_name),
         y = "Frequency") +
    theme_minimal()
  
  # 이상치 값 출력 (로그 변환 이전 값으로 변환)
  outlier_indices <- which(data < lower_bound | data > upper_bound)
  outliers <- original_data[outlier_indices]
  # cat("Outliers (values that are more than 3 standard deviations from the mean):\n")
  # print(outliers)
  
  print(p)
}

