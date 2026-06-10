
# 필요한 패키지 설치
# install.packages("sf")
library(sf)

# 데이터 파일 읽기 (인코딩 문제 해결을 위해 encoding="UTF-8" 또는 "CP949"를 시도)
apartment_data <- read.csv("아파트 좌표.csv", fileEncoding = "CP949")
subway_data <- read.csv("강남구 지하철역 좌표.csv")

# 아파트와 지하철역 데이터를 sf 객체로 변환
apartments <- st_as_sf(apartment_data, coords = c("경도", "위도"), crs = 4326) # 여기에 실제 위경도 열 이름 입력
stations <- st_as_sf(subway_data, coords = c("경도", "위도"), crs = 4326)

# 아파트에서 가장 가까운 지하철역 간 거리 계산
distances <- st_distance(apartments, stations)
nearest_station_distances <- apply(distances, 1, min) # 각 아파트별 최단 거리 계산

# 아파트 데이터에 역세권 여부 추가 (500m 기준 예시)
apartments$near_station <- nearest_station_distances <= 500

# 결과 확인
print(apartments)

# write.csv(apartments, "아파트_데이터.csv", row.names = FALSE, fileEncoding = "CP949")


# 가장 가까운 지하철역의 인덱스를 찾고 해당 이름과 거리 추가
nearest_station_index <- apply(distances, 1, which.min)
apartments$nearest_station <- subway_data$지하철역명[nearest_station_index]
apartments$nearest_distance <- nearest_station_distances

View(apartments)

###############################################################################
# 데이터 불러오기
apartment_trend <- read.csv("아파트 동향.csv")
near_station <- read.csv("역세권 여부.csv", fileEncoding = "CP949", row.names = NULL)
str(apartment_trend)
# 상승세 데이터만 필터링
apartment_trend_rise <- subset(apartment_trend, 동향 == "상승세")

# 상승세 데이터와 역세권 여부 데이터를 '아파트명' 기준으로 결합
merged_data <- merge(apartment_trend_rise, near_station, by = "아파트명")

# 로지스틱 회귀분석: 역세권 여부가 아파트 상승에 미치는 영향 평가
# 역세권 여부를 factor 형식으로 변환하여 로지스틱 회귀 모델에 사용
merged_data$역세권.여부 <- as.factor(merged_data$역세권.여부)

str(merged_data)


# 역세권 여부 변수를 숫자로 변환하여 상관관계 분석
merged_data$역세권_numeric <- as.numeric(merged_data$역세권.여부) - 1  # TRUE는 1, FALSE는 0으로 변환
correlation_matrix <- cor(merged_data$비율, merged_data$역세권_numeric)
print(correlation_matrix)

# 로지스틱 회귀분석 실행
model <- glm(역세권.여부 ~ 비율, family = binomial, data = merged_data)
summary(model)

###############################################################################
# t-검정
# 비율 데이터가 정규분포를 따르는지 확인
shapiro.test(merged_data$비율)

# 역세권 여부에 따라 비율 평균 차이를 검정
t_test_result <- t.test(비율 ~ 역세권.여부, data = merged_data)
print(t_test_result)
###############################################################################
# 역세권 여부를 숫자로 변환하여 회귀 분석
merged_data$역세권_numeric <- as.numeric(merged_data$역세권.여부) - 1  # TRUE는 1, FALSE는 0

# 공시가격 상승률에 대한 회귀분석 수행
model <- lm(비율 ~ 역세권_numeric, data = merged_data)
summary(model)
###############################################################################
# 비모수 검정: Mann-Whitney U Test (Wilcoxon Rank Sum Test)
wilcox_test_result <- wilcox.test(비율 ~ 역세권.여부, data = merged_data)
print(wilcox_test_result)

################################################################################
#<생산인구에 따른 상위 3개 지역의 지하철역>
apartment_data <- read.csv("아파트 좌표.csv", fileEncoding = "CP949")
subway_data <- read.csv("강남구 지하철역 좌표.csv")
str(apartment_data)
str(subway_data)

# 필요한 패키지 설치 및 로드
if (!require(geosphere)) install.packages("geosphere")
library(geosphere)
# 필요한 패키지 로드
if (!require(sf)) install.packages("sf")
library(sf)

# 상위 3개 동의 지하철역 리스트 (필터링용)
selected_stations <- c("신논현", "언주", "선정릉", "강남", "역삼", "선릉", "한티",
                       "삼성", "대치", "학여울", "도곡", "양재", "매봉")

# 지하철역 데이터에서 선택된 역들만 필터링
filtered_stations <- subway_data[subway_data$역명 %in% selected_stations, ]

# 아파트와 필터링된 지하철역 데이터를 sf 객체로 변환
apartments <- st_as_sf(apartment_data, coords = c("경도", "위도"), crs = 4326)
stations <- st_as_sf(filtered_stations, coords = c("경도", "위도"), crs = 4326)

# 아파트에서 가장 가까운 지하철역 간 거리 계산
distances <- st_distance(apartments, stations)
nearest_station_distances <- apply(distances, 1, min) # 각 아파트별 최단 거리 계산

# 아파트 데이터에 역세권 여부 추가 (500m 기준)
apartments$near_station <- nearest_station_distances <= 500

# 가장 가까운 지하철역의 인덱스를 찾고 해당 이름과 거리 추가
nearest_station_index <- apply(distances, 1, which.min)
apartments$nearest_station <- filtered_stations$역명[nearest_station_index]
apartments$nearest_distance <- nearest_station_distances

# 결과 확인
View(apartments)
# near_station이 TRUE인 아파트 개수 계산
true_count <- sum(apartments$near_station)
true_count
# 146개

summary(apartments)
# write.csv(apartments, "유동인구_역세권.csv", fileEncoding = "CP949")

apartment_trend <- read.csv("아파트 동향.csv")
str(apartment_trend)

# 필요한 패키지 로드
library(dplyr)

# 아파트 동향 데이터와 역세권 여부 데이터를 결합
apartments <- read.csv("유동인구_역세권.csv", fileEncoding = "CP949")
apartment_trend <- read.csv("아파트 동향.csv")

# 아파트명 전처리: 공백 제거, 소문자 변환
apartment_trend$아파트명 <- tolower(gsub(" ", "", apartment_trend$아파트명))
near_station$아파트명 <- tolower(gsub(" ", "", near_station$아파트명))

# 상승세 데이터만 필터링
apartment_trend_rise <- subset(apartment_trend, 동향 == "상승세")

# 상승세 데이터와 역세권 여부 데이터를 '아파트명' 기준으로 결합
merged_data <- merge(apartment_trend_rise, near_station, by = "아파트명")

# 결합된 데이터 확인
if (nrow(merged_data) > 0) {
  print("결합 성공")
  str(merged_data)
} else {
  print("결합 실패: 아파트명 일치 문제 가능성 있음")
}
# 역세권 여부를 factor 형식으로 변환
merged_data$역세권.여부 <- as.factor(merged_data$역세권.여부)

# '동향' 변수를 이진형 변수로 변환 (상승세: 1, 그 외: 0)
merged_data$상승세여부 <- ifelse(merged_data$동향 == "상승세", 1, 0)

# 상승세 여부를 종속 변수로 사용한 로지스틱 회귀분석 수행
logistic_model <- glm(상승세여부 ~ 역세권.여부, data = merged_data, family = binomial)

# 회귀분석 결과 출력
summary(logistic_model)

######################################################################

# 250m로 다시
#<생산인구에 따른 상위 3개 지역의 지하철역>
apartment_data <- read.csv("아파트 좌표.csv", fileEncoding = "CP949")
subway_data <- read.csv("강남구 지하철역 좌표.csv")
str(apartment_data)
str(subway_data)

# 상위 3개 동의 지하철역 리스트 (필터링용)
selected_stations <- c("신논현", "언주", "선정릉", "강남", "역삼", "선릉", "한티",
                       "삼성", "대치", "학여울", "도곡", "양재", "매봉")

# 지하철역 데이터에서 선택된 역들만 필터링
filtered_stations <- subway_data[subway_data$역명 %in% selected_stations, ]

# 아파트와 필터링된 지하철역 데이터를 sf 객체로 변환
apartments <- st_as_sf(apartment_data, coords = c("경도", "위도"), crs = 4326)
stations <- st_as_sf(filtered_stations, coords = c("경도", "위도"), crs = 4326)

# 아파트에서 가장 가까운 지하철역 간 거리 계산
distances <- st_distance(apartments, stations)
nearest_station_distances <- apply(distances, 1, min) # 각 아파트별 최단 거리 계산

# 아파트 데이터에 역세권 여부 추가 (250m 기준)
apartments$near_station <- nearest_station_distances <= 250

# 가장 가까운 지하철역의 인덱스를 찾고 해당 이름과 거리 추가
nearest_station_index <- apply(distances, 1, which.min)
apartments$nearest_station <- filtered_stations$역명[nearest_station_index]
apartments$nearest_distance <- nearest_station_distances
# near_station이 TRUE인 아파트 개수 계산
  true_count <- sum(apartments$near_station)
true_count
# 37개

# 결과 확인
View(apartments)

summary(apartments)
write.csv(apartments, "유동인구_역세권_250m.csv", fileEncoding = "CP949")

apartment_trend <- read.csv("아파트 동향.csv")
str(apartment_trend)

# 필요한 패키지 로드
library(dplyr)

# 아파트 동향 데이터와 역세권 여부 데이터를 결합
apartments <- read.csv("유동인구_역세권_250m.csv", fileEncoding = "CP949")
apartment_trend <- read.csv("아파트 동향.csv")

# 아파트명 전처리: 공백 제거, 소문자 변환
apartment_trend$아파트명 <- tolower(gsub(" ", "", apartment_trend$아파트명))
near_station$아파트명 <- tolower(gsub(" ", "", near_station$아파트명))

# 상승세 데이터만 필터링
apartment_trend_rise <- subset(apartment_trend, 동향 == "상승세")

# 상승세 데이터와 역세권 여부 데이터를 '아파트명' 기준으로 결합
merged_data <- merge(apartment_trend_rise, near_station, by = "아파트명")

# 결합된 데이터 확인
if (nrow(merged_data) > 0) {
  print("결합 성공")
  str(merged_data)
} else {
  print("결합 실패: 아파트명 일치 문제 가능성 있음")
}
# 역세권 여부를 factor 형식으로 변환
merged_data$역세권.여부 <- as.factor(merged_data$역세권.여부)

# '동향' 변수를 이진형 변수로 변환 (상승세: 1, 그 외: 0)
merged_data$상승세여부 <- ifelse(merged_data$동향 == "상승세", 1, 0)

# 상승세 여부를 종속 변수로 사용한 로지스틱 회귀분석 수행
logistic_model <- glm(상승세여부 ~ 역세권.여부, data = merged_data, family = binomial)

# 회귀분석 결과 출력
summary(logistic_model)

#############################################################
# 실거래가로 분석
# 필요한 패키지 로드
library(dplyr)
library(sf)
apartments <- read.csv("역세권 여부.csv", fileEncoding = "CP949")
# 1. 실거래가 데이터 로드 및 전처리
transaction_data <- read.csv("실거래가 데이터.csv")

str(apartments)
str(transaction_data)
# 계약일 변수를 날짜 형식으로 변환
transaction_data$계약일 <- as.Date(transaction_data$계약일, format = "%y.%m.%d")
# 2022년 내에서 각 단지의 첫 거래와 마지막 거래 가격을 사용
transaction_data_2022 <- transaction_data %>%
  filter(format(계약일, "%Y") == "2022") %>%
  group_by(apt_nm) %>%
  summarise(
    초기_가격 = first(거래금액, order_by = 계약일),
    말기_가격 = last(거래금액, order_by = 계약일)
  )

# 상승세 여부 추가
transaction_data_2022 <- transaction_data_2022 %>%
  mutate(상승세여부 = ifelse(말기_가격 > 초기_가격, 1, 0))
# 결과 확인
print(transaction_data_2022)

intersect(unique(apartments$아파트명), unique(transaction_data_2022$apt_nm))

# 상승세 여부와 역세권 여부 데이터를 아파트명 기준으로 결합
merged_data <- merge(transaction_data_2022, apartments, by.x = "apt_nm", by.y = "아파트명")

# 로지스틱 회귀 분석: 역세권 여부가 실거래가 상승에 미치는 영향 평가
# 역세권 여부를 factor 형식으로 변환하여 로지스틱 회귀 모델에 사용
merged_data$역세권.여부 <- as.factor(merged_data$역세권.여부)
# near_station 변수의 값 확인
table(merged_data$역세권.여부)

# 상승세여부를 종속 변수로 사용한 로지스틱 회귀 분석 수행
logistic_model <- glm(상승세여부 ~ 역세권.여부, data = merged_data, family = binomial)

# 회귀 분석 결과 출력
summary(logistic_model)
# 250m 유동인구 -> 역세권 여부가 2022년 아파트 실거래가 상승 여부에 통계적으로 유의한 영향을 미치지 않는다
# 500m 역세권 여부(not 유동인구) -> 역세권 여부가 아파트 실거래가 상승에 유의미한 영향을 미치지 않는 것
###########################################################################
# 다항회귀분석
# 종속: 2023년 공시지가
# 필요한 패키지 로드
library(nnet)
library(dplyr)
library(caret)

# 데이터 파일 읽기 (인코딩 문제 해결을 위해 encoding="UTF-8" 또는 "CP949"를 시도)
apartment_data <- read.csv("아파트 좌표.csv", fileEncoding = "CP949")
subway_data <- read.csv("강남구 지하철역 좌표.csv")

# 아파트와 지하철역 데이터를 sf 객체로 변환
apartments <- st_as_sf(apartment_data, coords = c("경도", "위도"), crs = 4326) # 여기에 실제 위경도 열 이름 입력
stations <- st_as_sf(subway_data, coords = c("경도", "위도"), crs = 4326)

# 아파트에서 가장 가까운 지하철역 간 거리 계산
distances <- st_distance(apartments, stations)
nearest_station_distances <- apply(distances, 1, min) # 각 아파트별 최단 거리 계산

# 아파트 데이터에 역세권 여부 추가 (500m 기준 예시)
apartments$near_station <- nearest_station_distances <= 500

# 역세권 여부 데이터와 공시지가 데이터 결합
apartment_trend <- read.csv("아파트 동향.csv")
near_station <- apartments %>% select(아파트명, near_station) # 역세권 여부 데이터 생성
merged_data <- merge(apartment_trend, near_station, by = "아파트명")

# 2023년 개별 공시지가 구간화 (예: 낮음, 중간, 높음으로 분류)
merged_data$공시지가_구간 <- cut(merged_data$X2023년.개별공시지가,
                           breaks = c(-Inf, quantile(merged_data$X2023년.개별공시지가, probs = c(0.33, 0.66)), Inf),
                           labels = c("낮음", "중간", "높음"))

# 다항 로지스틱 회귀를 위해 공시지가 구간을 factor로 변환
merged_data$공시지가_구간 <- as.factor(merged_data$공시지가_구간)

# 다항 로지스틱 회귀 분석 실행
multi_logistic_model <- multinom(공시지가_구간 ~ near_station, data = merged_data)

# 결과 확인
summary(multi_logistic_model)

# 1. p-값 계산
summary_model <- summary(multi_logistic_model)
z_values <- summary_model$coefficients / summary_model$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
print("p-값:")
print(p_values)

# 2. 교차 검증
set.seed(123)
trainIndex <- createDataPartition(merged_data$공시지가_구간, p = .8, list = FALSE, times = 1)
train_data <- merged_data[trainIndex, ]
test_data <- merged_data[-trainIndex, ]

# 훈련 데이터로 모델 학습
multi_logistic_model_cv <- multinom(공시지가_구간 ~ near_station, data = train_data)

# 테스트 데이터로 예측
predictions <- predict(multi_logistic_model_cv, test_data)

# 정확도 평가
confusion_matrix <- table(predictions, test_data$공시지가_구간)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print("모델 정확도:")
print(accuracy)

# 혼동 행렬 출력
print("혼동 행렬:")
print(confusion_matrix)

# 역세권 여부가 공시지가 구간에 유의미한 영향을 미친다는 명확한 결론을 내리기 어렵습니다. 모델의 정확도가 낮아 예측 성능도 미흡하며, p-값도 유의하지 않은 것
