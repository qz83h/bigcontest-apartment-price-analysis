anc<-read.csv("엘에이치_공시.csv",header = T)
trd<-read.csv("엘에이치_실거래가.csv",header = T)
info<-read.csv("엘에이치_단지.csv",header = T)
hkd<-read.csv("엘에이치_학군.csv",header = T)
soc<-read.csv("엘에이치_사회.csv",header = T)

str(anc)
str(trd)
str(info)
str(hkd)
str(soc)
library(leaflet)


# LH힐스테이트와 같은 법정동코드 아파트
# 아파트별로 전용면적세대수가 가장 많은 면적을 기준으로 함-> 수요가 가장 많은
apartment_data <- data.frame(
  name = c("엘에이치강남힐스테이트", "래미안강남힐즈", "엘에이치강남아이파크", "강남한양수자인(4단지)",
           "래미안포레(3단지)", "엘에이치강남3단지", "엘에이치강남브리즈힐"),
  area = c(59.93, 91.96, 59.96, 49.94, 59.97,36.08,74.04),  # 전용면적 (㎡)
  count = c(440,60,114,106,194,200,59), # 전용면적별 세대수
  median_price = c(635000000, 1376000000, 916000000, 1000000000, 957000000,348000000,825000000),  # 중위 공시가격 (원)
  lng = c(127.0882715, 127.0938574, 127.1015903, 127.1124898,127.1101236,127.0908817,127.0909955),  # 경도
  lat = c(37.47141205, 37.47412202, 37.47484492, 37.47536674,37.47479652,37.47072938,37.47283402),  # 위도
  rank=c(215,376,164,167,180,88,485), #시구군내공시가격순위
  hkd_price = c(1030000000,"모름","모름","모름","모름","모름","모름"),  # 면적코드별중위실거래가
  t=c("상승세","하락세","상승세","하락세","하락세","모름","모름") # 거래동향
)
# 래미안 면적코드 4는 상승, 3은 하락 -> 위에 나와있는 면적은 코드 2, 하락세로 기록함

# 데이터 확인
print(apartment_data)

anc<-anc[4,]

# 지도 생성
map <- leaflet() %>%
  addTiles() %>%  # OpenStreetMap 기본 타일 추가
  setView(lng = 126.9784, lat = 37.5665, zoom = 12)  # 서울 좌표

map <- map %>% 
  addMarkers(
    lng = apartment_data$lng,
    lat = apartment_data$lat,
    popup = paste(
      "<b>아파트명:</b>", apartment_data$name, "<br>",
      "<b>전용면적:</b>", apartment_data$area, "㎡<br>",
      "<b>중위 공시가격:</b>", format(apartment_data$median_price, big.mark = ",", scientific = FALSE), "원<br>",
      "<b>시구군내공시가격순위:</b>", apartment_data$rank, "위<br>",  # 'district_rank'를 사용하도록 수정
      "<b>거래동향:</b>", apartment_data$t, "<br>",  # 'transaction_trend' 변수를 사용해야 할 수 있음
      "<b>면적코드별중위실거래가:</b>", apartment_data$hkd_price, "<br>"
    )
  )
# 지도 출력
map

#####################################################

a<-read.csv("실거래가.csv",header=T)
str(a)
options(scipen = 999)  # 지수 표기법 방지
print(a)               # a 출력
# 변수명 변경
colnames(a) <- c("uid","아파트코드", "아파트명", "도로명주소", "법정동코드", "기준년월", "면적코드", "거래량", "누적거래량",
                 "회전율", "면적코드별최소실거래가", "면적코드별최대실거래가", "면적코드별평균실거래가", 
                 "면적코드별중위실거래가", "최근거래일자", "거래동향", "인근아파트거래동향", "해당읍면동거래동향", 
                 "경도", "위도")

# 변경된 데이터셋 확인
head(a)

# 필요한 패키지 로드
library(ggplot2)
library(dplyr)

# 결측치가 있는 모든 행 제거
a_clean <- na.omit(a)
# 데이터프레임 구조 확인
str(a_clean)
str(a_clean$법정동코드)

#면적코드가 2, 3, 4인 아파트 데이터 추출
apartments_code2_3_4 <- a_clean %>%
  filter(면적코드 %in% c(2, 3, 4))

# 산점도 생성 (아파트명 레이블 추가)
ggplot(apartments_code2_3_4, aes(x = 면적코드, y = 면적코드별중위실거래가, color = as.factor(면적코드))) +
  geom_point() +
  geom_text(aes(label = 아파트명), vjust = -1, hjust = 0.5, size = 3) + # 레이블 추가
  labs(
    title = "면적코드별 중위 실거래가",
    x = "면적코드",
    y = "중위 실거래가",
    color = "면적코드"
  ) +
  theme_minimal()

library(ggplot2)
library(ggplot2)

# 면적코드 2, 3, 4인 아파트 데이터 추출
apartments_code2_3_4 <- a_clean %>% 
  filter(면적코드 %in% c(2, 3, 4))
library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)  # ggrepel 패키지 추가

# 면적코드 2, 3, 4인 아파트 데이터 추출
apartments_code2_3_4 <- a_clean %>% 
  filter(면적코드 %in% c(2, 3, 4))

# 거래동향을 파악하는 산점도 그리기
ggplot(apartments_code2_3_4, aes(x = factor(면적코드), y = 거래동향, color = 거래동향)) +
  geom_point(size = 3, position = position_jitter(width = 0.1, height = 0)) +  # 점 크기 설정 및 위치 조정
  geom_text_repel(aes(label = 아파트명), size = 3, max.overlaps = Inf) +  # 아파트명 추가, 겹치지 않게
  labs(title = "면적코드별 거래동향", 
       x = "면적코드", 
       y = "거래동향") +
  scale_x_discrete(breaks = c("2", "3", "4")) +  # x축에 면적코드 2, 3, 4만 표시
  theme_minimal() +  # 테마 설정
  scale_color_manual(values = c("하락세" = "blue", "상승세" = "red"))  # 색상 설정

# 면적코드 4 -> 대부분 하락세

# 거래동향 요약
apartments_code2_3_4 %>% 
  group_by(면적코드, 거래동향) %>% 
  summarise(빈도수 = n()) %>%
  ggplot(aes(x = factor(면적코드), y = 빈도수, fill = 거래동향)) +
  geom_bar(stat = "identity", position = "dodge") +  # 막대그래프 그리기
  labs(title = "면적코드별 거래동향 빈도", x = "면적코드", y = "빈도수") +
  scale_fill_manual(values = c("하락세" = "blue", "상승세" = "red")) +  # 색상 설정
  theme_minimal()

# 면적코드 2, 3, 4인 아파트 데이터 추출
apartments_code2_3_4 <- a_clean %>%
  filter(면적코드 %in% c(2, 3, 4))

# 거래동향을 파악하는 산점도 그리기 (법정동 코드별로 분리)
ggplot(apartments_code2_3_4, aes(x = factor(면적코드), y = 거래동향, color = 거래동향)) +
  geom_point(size = 3, position = position_jitter(width = 0.1, height = 0)) +  # 점 크기 설정 및 위치 조정
  geom_text_repel(aes(label = 아파트명), size = 3, max.overlaps = Inf) +  # 아파트명 추가, 겹치지 않게
  labs(title = "법정동 코드별 면적코드와 거래동향 비교", 
       x = "면적코드", 
       y = "거래동향") +
  scale_x_discrete(breaks = c("2", "3", "4")) +  # x축에 면적코드 2, 3, 4만 표시
  theme_minimal() +  # 테마 설정
  scale_color_manual(values = c("하락세" = "blue", "상승세" = "red")) +  # 색상 설정
  facet_wrap(~ 법정동코드)  # 법정동 코드별로 그래프 분리
# 거래동향 결측값 제외
apartments_code2_3_4_clean <- apartments_code2_3_4 %>%
  filter(!is.na(거래동향))

# 거래동향 별 개수 계산
trend_counts <- apartments_code2_3_4_clean %>%
  group_by(거래동향) %>%
  summarise(count = n())

# 거래동향 레이블에 개수 추가
trend_labels <- paste(trend_counts$거래동향, "(", trend_counts$count, "건)", sep = "")

# 거래동향을 파악하는 산점도 그리기 (법정동 코드별로 분리, 범례에 거래동향 개수 추가)
ggplot(apartments_code2_3_4_clean, aes(x = factor(면적코드), y = 거래동향, color = 거래동향)) +
  geom_point(size = 3, position = position_jitter(width = 0.1, height = 0)) +  # 점 크기 설정 및 위치 조정
  geom_text_repel(aes(label = 아파트명), size = 3, max.overlaps = Inf) +  # 아파트명 추가, 겹치지 않게
  labs(title = "법정동 코드별 면적코드와 거래동향 비교", 
       x = "면적코드", 
       y = "거래동향") +
  scale_x_discrete(breaks = c("2", "3", "4")) +  # x축에 면적코드 2, 3, 4만 표시
  theme_minimal() +  # 테마 설정
  scale_color_manual(values = c("하락세" = "blue", "상승세" = "red"),
                     labels = trend_labels) +  # 거래동향 별 개수 레이블로 표시
  facet_wrap(~ 법정동코드)  # 법정동 코드별로 그래프 분리

# 면적코드 별 선호도를 나타내는 법정동 코드별 막대 그래프
ggplot(apartments_code2_3_4_clean, aes(x = factor(면적코드), fill = 거래동향)) +
  geom_bar(position = "dodge") +  # 막대를 거래동향에 따라 나눔
  facet_wrap(~ 법정동코드) +  # 법정동 코드별로 그래프 분리
  labs(title = "법정동 코드별 선호 면적코드와 거래동향 비교",
       x = "면적코드",
       y = "거래 건수") +
  scale_fill_manual(values = c("하락세" = "blue", "상승세" = "red")) +  # 색상 설정
  theme_minimal()

# 면적코드 별 거래동향을 나타내는 법정동 코드별 누적 막대 그래프
ggplot(apartments_code2_3_4_clean, aes(x = factor(면적코드), fill = 거래동향)) +
  geom_bar(position = "fill") +  # 누적 막대 그래프 (비율)
  facet_wrap(~ 법정동코드) +  # 법정동 코드별로 그래프 분리
  labs(title = "법정동 코드별 선호 면적코드와 거래동향 비교 (비율)",
       x = "면적코드",
       y = "비율") +
  scale_fill_manual(values = c("하락세" = "blue", "상승세" = "red")) +  # 색상 설정
  theme_minimal()

###################################################

# 힐스테이트와 엘에이치강남아이파크  강남한양수자인 강남브리즈힐 순서임
# 전용면적 84 기준
# 이 둘의 사회통계 이용해서 비교
# 전용면적별 중위 공시가격 이용
# 예시 데이터 프레임
# 기존 아파트 데이터와 추가 아파트 데이터를 결합
# 데이터 프레임 생성
data <- data.frame(
  가격 = c(936000000, 1298000000, 1248000000, 947000000),  # 아파트 가격
  총인구수 = c(7365, 4231, 8159, 4167),  # 총인구수
  생산인구수 = c(4805, 3317, 5568, 2833),  # 생산인구수
  유소년인구수 = c(1295, 579, 1543, 816)  # 유소년인구수
)

# 다중 회귀 모델 생성
model_simple <- lm(가격 ~ 총인구수 + 생산인구수 + 유소년인구수, data = data)

# 모델 요약
summary(model_simple)

# 다중 회귀 모델 생성
model <- lm(가격 ~ 총인구수 + 생산인구수 + 유소년인구수, data = data)

# 모델 요약
summary(model)

# 잔차 플롯
par(mfrow = c(2, 2))
plot(model)
# 새로운 데이터 예측
new_data <- data.frame(
  총인구수 = c(8000),
  생산인구수 = c(4000),
  유소년인구수 = c(1300),
  노년인구수 = c(500),
  인구밀도 = c(900),
  총부양비 = c(30),
  유소년부양비 = c(20),
  노년부양비 = c(10)
)

predictions <- predict(model, new_data)
print(predictions)

################################################3

a<-read.csv("실거래가.csv",header=T)
str(a)
options(scipen = 999)  # 지수 표기법 방지
print(a)               # a 출력
# 변수명 변경
colnames(a) <- c("uid","아파트코드", "아파트명", "도로명주소", "법정동코드", "기준년월", "면적코드", "거래량", "누적거래량",
                 "회전율", "면적코드별최소실거래가", "면적코드별최대실거래가", "면적코드별평균실거래가", 
                 "면적코드별중위실거래가", "최근거래일자", "거래동향", "인근아파트거래동향", "해당읍면동거래동향", 
                 "경도", "위도")
b<-read.csv("공시가격.csv",header = T)
b$시도공시가격순위
str(b)
# 면적코드별 공시가격을 나타내는 박스 플롯
ggplot(b, aes(x = factor(면적코드), y = 전용면적별평균공시가격)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  # 박스 플롯
  labs(title = "면적코드별 전용면적별 평균 공시가격 분포",
       x = "면적코드",
       y = "전용면적별 평균 공시가격 (원)") +
  theme_minimal() + 
  scale_y_continuous(labels = scales::comma)  # Y축에 콤마 형식 추가
# 법정동 코드별 시도공시가격을 나타내는 바이올린 플롯
ggplot(b, aes(x = factor(법정동코드), y = 시도공시가격백분위수)) +
  geom_violin(fill = "lightgreen", color = "darkgreen") +  # 바이올린 플롯
  labs(title = "법정동 코드별 시도 공시가격 백분위수 분포",
       x = "법정동 코드",
       y = "시도 공시가격 백분위수") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)  # Y축에 퍼센트 형식 추가
# 법정동 코드별 시도 공시가격 순위를 나타내는 박스 플롯
ggplot(b, aes(x = factor(법정동코드), y = 시도공시가격순위)) +
  geom_boxplot(fill = "lightcoral", color = "darkred") +  # 박스 플롯
  labs(title = "법정동 코드별 시도 공시가격 순위 분포",
       x = "법정동 코드",
       y = "시도 공시가격 순위") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)  # Y축에 콤마 형식 추가
