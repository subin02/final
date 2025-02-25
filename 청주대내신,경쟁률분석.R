library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(RColorBrewer)
install.packages("ggrepel")
library(ggrepel)

samp <- read_excel("C:/Users/tnql1/OneDrive/문서/통계모델링/일반전형.xlsx")

head(samp)

#비지니스대학
bi <- samp %>% filter(대학 == "비즈니스대학")
bi

#공과대학
go <- samp %>% filter(대학 == "공과대학")
go

#사범대학
sa <- samp %>% filter(대학 == "사범대학")
sa

#예술대학
ye <- samp %>% filter(대학 == "예술대학")
ye

#직활학부
gi <- samp %>% filter(대학 == "직할학부")
gi

summarise(samp)

##대학별 모집인원
mo <- samp %>% select(대학, 모집인원24, 모집인원23) %>% group_by(대학) %>%
  summarise(모집인원24 = sum(모집인원24), 모집인원23 = sum(모집인원23))
mo

mo_dl <- pivot_longer(
  mo, 
  cols = starts_with("모집인원"), 
  names_to = "year",
  values_to = "모집인원")
mo_dl

### ggplot을 사용한 막대 그래프
ggplot(mo_dl, aes(x = 대학, y = 모집인원, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  geom_text(aes(label = 모집인원),
            position = position_dodge2(width = 0.7),  # 텍스트 위치 정렬
            vjust = -0.5,  # 텍스트 막대 위로 이동
            size = 2.5,
            color = "black")+
  labs(title = "대학별 모집인원 (2023 vs 2024)",
       x = "대학",
       y = "모집인원") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel3",
                    labels = c("2023", "2024")) + 
  ylim(0, max(mo_dl$모집인원) * 1.2)

##대학별 경쟁률
rat <- samp %>% select(대학, 경쟁률23, 경쟁률24) %>% group_by(대학) %>% summarise(경쟁률23 = mean(경쟁률23), 경쟁률24 = mean(경쟁률24))
rat

rat_dl <- pivot_longer(
  rat, 
  cols = starts_with("경쟁률"), 
  names_to = "year",
  values_to = "경쟁률평균")
rat_dl

### ggplot을 사용한 막대 그래프
ggplot(rat_dl, aes(x = 대학, y = 경쟁률평균, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  geom_text(aes(label = round(경쟁률평균, 2)),
            position = position_dodge2(width = 0.7),  # 텍스트 위치 정렬
            vjust = -0.5,  # 텍스트 막대 위로 이동
            size = 2.5,
            color = "black")+
  labs(title = "대학별 경쟁률 (2023 vs 2024)",
       x = "대학",
       y = "경쟁률") + 
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024")) + 
  ylim(0, max(rat_dl$경쟁률평균) * 1.2)

#비지니스대 경쟁률
bi_ra <-  samp %>% filter(대학 == "비즈니스대학") %>% select(모집단위, 경쟁률23, 경쟁률24)
bi_ra

bi_data_long <- pivot_longer(
  bi_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")

# ggplot을 사용한 막대 그래프
ggplot(bi_data_long, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +  # 옆으로 비교
  labs(title = "비즈니스대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") +
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024"))

##인문사회회대 경쟁률
in_ra <-  samp %>% filter(대학 == "인문사회대학") %>% select(모집단위, 경쟁률23, 경쟁률24)
in_ra

in_dl <- pivot_longer(
  in_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")

### ggplot을 사용한 막대 그래프
ggplot(in_dl, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  geom_text(aes(label = round(경쟁률, 2)),
            position = position_dodge2(width = 0.7),  # 텍스트 위치 정렬
            vjust = -0.5,  # 텍스트 막대 위로 이동
            size = 2.5,
            color = "black")+
  labs(title = "인문사회대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1",
                    labels = c("2023", "2024")) + 
  ylim(0, max(in_dl$경쟁률) * 1.2)

#공과대학 경쟁률
go_ra <-  samp %>% filter(대학 == "공과대학") %>% select(모집단위, 경쟁률23, 경쟁률24)

go_data_long <- pivot_longer(
  go_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")
go_data_long

# ggplot을 사용한 막대 그래프
ggplot(go_data_long, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +  # 옆으로 비교
  labs(title = "공과대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") +
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024"))

#사범대학 경쟁률
sa_ra <-  samp %>% filter(대학 == "사범대학") %>% select(모집단위, 경쟁률23, 경쟁률24)

sa_data_long <- pivot_longer(
  sa_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")
sa_data_long

# ggplot을 사용한 막대 그래프
ggplot(sa_data_long, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +  # 옆으로 비교
  labs(title = "사범범대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") +
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024"))

##보건의료과학대학 경쟁률
bo_ra <-  samp %>% filter(대학 == "보건의료과학대학") %>% select(모집단위, 경쟁률23, 경쟁률24)

bo_dl <- pivot_longer(
  bo_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")
bo_dl

### ggplot을 사용한 막대 그래프
ggplot(bo_dl, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  geom_text(aes(label = round(경쟁률, 2)),
            position = position_dodge2(width = 0.7),  # 텍스트 위치 정렬
            vjust = -0.5,  # 텍스트 막대 위로 이동
            size = 2.5,
            color = "black")+
  labs(title = "보건의료과학대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") + 
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024")) + 
  ylim(0, max(bo_dl$경쟁률) * 1.2)

#예술대학 경쟁률
ye_ra <-  samp %>% filter(대학 == "예술대학") %>% select(모집단위, 경쟁률23, 경쟁률24)

ye_data_long <- pivot_longer(
  ye_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")
ye_data_long

# ggplot을 사용한 막대 그래프
ggplot(ye_data_long, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +  # 옆으로 비교
  labs(title = "예술대학 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") +
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024"))


#직활학부 경쟁률
ji_ra <-  samp %>% filter(대학 == "직할학부") %>% select(모집단위, 경쟁률23, 경쟁률24)

ji_data_long <- pivot_longer(
  ji_ra, 
  cols = starts_with("경쟁률"), 
  names_to = "year", 
  values_to = "경쟁률")
ji_data_long

# ggplot을 사용한 막대 그래프
ggplot(ji_data_long, aes(x = 모집단위, y = 경쟁률, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +  # 옆으로 비교
  labs(title = "직활학부 경쟁률 (2023 vs 2024)",
       x = "모집단위",
       y = "경쟁률") +
  theme_minimal() +
  scale_fill_manual(values = c("경쟁률23" = "#FBB4AE",
                               "경쟁률24" = "#B3CDE3"),
                    labels = c("2023", "2024"))


#내신평균
##비지니스대 내신등급평균
bi_ne <-  samp %>% filter(대학 == "비즈니스대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
bi_ne

bine_dl <- pivot_longer(
  bi_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
bine_dl

### ggplot을 사용한 막대 그래프
ggplot(bine_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "비즈니스대학 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(bine_dl$내신등급) * 1.2)

#공과대학 내신등급평균
go_ne <-  samp %>% filter(대학 == "공과대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
go_ne

gone_dl <- pivot_longer(
  go_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")

# ggplot을 사용한 막대 그래프
ggplot(gone_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "공과대학 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(gone_dl$내신등급) * 1.2)

##사범대학 내신등급평균
sa_ne <-  samp %>% filter(대학 == "사범대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
sa_ne

sane_dl <- pivot_longer(
  sa_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
sane_dl

### ggplot을 사용한 막대 그래프
ggplot(sane_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "사범대학 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(sane_dl$내신등급) * 1.2)

##예술대학 내신등급평균
ye_ne <-  samp %>% filter(대학 == "예술대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
ye_ne

yene_dl <- pivot_longer(
  ye_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
yene_dl

### ggplot을 사용한 막대 그래프
ggplot(yene_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "예술대학 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(yene_dl$내신등급) * 1.2)

##보건의료과학대학 내신등급평균
bo_ne <-  samp %>% filter(대학 == "보건의료과학대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
bo_ne

bone_dl <- pivot_longer(
  bo_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
bone_dl

### ggplot을 사용한 막대 그래프
ggplot(bone_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "보건의료과학대학 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(bone_dl$내신등급) * 1.2)

##직할학부 내신등급평균
gi_ne <-  samp %>% filter(대학 == "직할학부") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
gi_ne

gine_dl <- pivot_longer(
  gi_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
gine_dl

### ggplot을 사용한 막대 그래프
ggplot(gine_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "직할학부 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_manual(values = c("내신등급_평균23" = "#FBB4AE",
                               "내신등급_평균24" = "#B3CDE3"),
                    labels = c("2023", "2024")) +
  ylim(0, max(gine_dl$내신등급) * 1.2)

##인문사회대학학 내신등급평균
in_ne <-  samp %>% filter(대학 == "인문사회대학") %>% select(모집단위, 내신등급_평균23,  내신등급_평균24)
in_ne

inne_dl <- pivot_longer(
  in_ne, 
  cols = starts_with("내신등급_평균"), 
  names_to = "year", 
  values_to = "내신등급")
inne_dl

### ggplot을 사용한 막대 그래프
ggplot(inne_dl, aes(x = 모집단위, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(내신등급, 2)),
            position = position_dodge2(width = 0.7),
            vjust = -0.5,
            size = 2.5,
            color = "black") + 
  labs(title = "직할학부 내신등급_평균 (2023 vs 2024)",
       x = "모집단위",
       y = "내신등급") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2",
                    labels = c("2023", "2024")) +
  ylim(0, max(inne_dl$내신등급) * 1.2)

##대학별 내신평균
nes <- samp %>% select(대학, 내신등급_평균23, 내신등급_평균24) %>% group_by(대학) %>% 
  summarise(내신등급23 = mean(내신등급_평균23), 내신등급24 = mean(내신등급_평균24))
nes

nes_dl <- pivot_longer(
  nes, 
  cols = starts_with("내신등급"), 
  names_to = "year",
  values_to = "내신등급")
nes_dl

### ggplot을 사용한 막대 그래프
ggplot(nes_dl, aes(x = 대학, y = 내신등급, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  
  geom_text(aes(label = round(내신등급,2)),
            position = position_dodge2(width = 0.7),  # 텍스트 위치 정렬
            vjust = -0.5,  # 텍스트 막대 위로 이동
            size = 2.5,
            color = "black")+
  labs(title = "대학별 내신등급 (2023 vs 2024)",
       x = "대학",
       y = "내신등급") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2",
                    labels = c("2023", "2024")) + 
  ylim(0, max(nes_dl$내신등급) * 1.2)

##상관분석
library(GGally)
library(ggplot2)

df_gen$cate2 <- ifelse(df_gen$cate=="공과대학", 
                       "공과대학", ifelse(df_gen$cate=="인문사회대학", 
                                      "인문사회대학", "나머지"))   #cate2 변수- 공과대학 vs 나머지
table(df_gen$cate2)

ggpairs(df_gen, columns=3:5, 
        mapping = ggplot2::aes(colour=cate2, alpha=0.8)

        
        
head(samp)

##대학간 경쟁률변화율, 내신등급변화율
samp_diff <- samp %>%
  group_by(대학) %>%
  summarise(내신등급_변화율 = ((내신등급_평균24 - 내신등급_평균23) / 내신등급_평균23) * 100,
    경쟁률_변화율 = ((경쟁률24 - 경쟁률23) / 경쟁률23) * 100)
samp_diff

ggplot(samp_diff, aes(x = 내신등급_변화율, y = 경쟁률_변화율, color = 대학)) +
  geom_point() +
  labs(title = "대학별 내신 변화율과 경쟁률 변화율의 상관관계",
       x = "내신 변화율 (%)",
       y = "경쟁률 변화율 (%)")

ggpairs(
  samp_diff, 
  columns = c("내신등급_변화율", "경쟁률_변화율"), # 비교할 변수 선택
  mapping = aes(colour = 대학, alpha = 0.8),           # 그룹별 색상 및 투명도 설정             # 대각선: 밀도 그래프
) +
  theme_minimal() +                                     # 깔끔한 테마 추가
  labs(title = "대학별 내신 변화율과 경쟁률 변화 관계",  # 그래프 제목
       colour = "대학 그룹")  
table(samp_diff$대학)
