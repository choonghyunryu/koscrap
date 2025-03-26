# search_list <- search_naver(
#   "교보생명",
#   do_done = TRUE
# )
#
# search_list |>
#   distinct()
#
#
# search_list1 <- search_naver(
#   "한화생명",
#   max_record = 100L
# )
#
#
# search_list2 <- search_naver(
#   "한화생명",
#   chunk_no = 101L,
#   max_record = 100L
# )
#
# search_list3 <- search_naver(
#   "한화생명",
#   chunk_no = 201L,
#   max_record = 100L
# )
#
# search_list4 <- search_naver(
#   "한화생명",
#   chunk_no = 301L,
#   max_record = 100L
# )
#
# search_list1 |>
#   bind_rows(search_list2) |>
#   bind_rows(search_list3) |>
#   bind_rows(search_list4) |>
#   distinct() |>
#   dim()
#
# search_list |>
#   summarise(min(publish_date), max(publish_date))
#
#
#
# search_list <- search_naver(
#   "교보생명",
#   do_done = TRUE
# )
#
# search_list |>
#   mutate(publish_date = as.Date(publish_date, format = "%Y-%m-%d")) |>
#   group_by(publish_date) |>
#   summarise(n = n()) |>
#   ggplot(aes(publish_date, n)) +
#   geom_col(fill = "lightblue") +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 days") +  # 날짜 형식 설정
#   labs(title = "날짜별 교보생명 관련 기사 건수 변화",
#        x = "날짜",
#        y = "기사 건수") +
#   theme_minimal(base_family = "NanumSquare")


library(stringr)

search_list |>
  select(originallink) |>
  mutate(originallink = str_remove(originallink, "https?:\\/\\/")) |>
  mutate(originallink = str_remove(originallink, "\\/[[:print:]]*")) |>
  mutate(originallink = str_remove_all(originallink, "www.|.co.kr|.com|.kr")) |>
  count(originallink, sort = TRUE) -> domain

# 패키지 로드
library(jsonlite)

# JSON 데이터 (JavaScript 배열을 JSON 형식으로 변환)
json_data <- '[
  {"site": "insight", "kor": "인사이트"},
  {"site": "topstarnews", "kor": "톱스타뉴스"},
  {"site": "yna", "kor": "연합뉴스"},
  {"site": "hankookilbo", "kor": "한국일보"},
  {"site": "mbn", "kor": "mbn뉴스"},
  {"site": "joynews24", "kor": "아이뉴스24"},
  {"site": "edaily", "kor": "이데일리"},
  {"site": "seoul", "kor": "서울신문"},
  {"site": "sedaily", "kor": "서울경제"},
  {"site": "biz.chosun", "kor": "조선일보"},
  {"site": "chosun", "kor": "조선일보"},
  {"site": "mk", "kor": "매일경제"},
  {"site": "news.sbs", "kor": "sbs뉴스"},
  {"site": "hankyung", "kor": "한경"},
  {"site": "news1", "kor": "뉴스1"},
  {"site": "tvchosun", "kor": "tv조선뉴스"},
  {"site": "nocutnews", "kor": "노컷뉴스"},
  {"site": "imaeil", "kor": "매일신문"},
  {"site": "munhwa", "kor": "문화일보"},
  {"site": "segye", "kor": "세계일보"},
  {"site": "gukjenews", "kor": "국제뉴스"},
  {"site": "fnnews", "kor": "파이낸셜뉴스"},
  {"site": "busan", "kor": "부산일보"},
  {"site": "asiae", "kor": "아시아경제"},
  {"site": "dt", "kor": "디지털타임스"},
  {"site": "christiantoday", "kor": "크리스천투데이"},
  {"site": "economist", "kor": "이코노미스트"},
  {"site": "kmib", "kor": "국민일보"},
  {"site": "inews24", "kor": "아이뉴스24"},
  {"site": "moneys", "kor": "머니S"},
  {"site": "news.tf", "kor": "더팩트"},
  {"site": "sports.khan", "kor": "스포츠경향"},
  {"site": "dailian", "kor": "데일리안"},
  {"site": "isplus", "kor": "일간스포츠"},
  {"site": "starnewskorea", "kor": "스타뉴스"},
  {"site": "wikitree", "kor": "위키트리"},
  {"site": "news.kbs", "kor": "KBS뉴스"},
  {"site": "daejonilbo", "kor": "대전일보"},
  {"site": "heraldcorp", "kor": "헤럴드경제"},
  {"site": "joongang", "kor": "중앙일보"},
  {"site": "xportsnews", "kor": "엑스포츠뉴스"},
  {"site": "tvreport", "kor": "티비리포트"},
  {"site": "heraldpop", "kor": "헤럴드팝"},
  {"site": "newsen", "kor": "뉴스엔"},
  {"site": "osen", "kor": "오센"},
  {"site": "yonhapnewstv", "kor": "연합뉴스TV"},
  {"site": "mydaily", "kor": "마이데일리"},
  {"site": "ytn", "kor": "YTN"},
  {"site": "mediapen", "kor": "미디어펜"},
  {"site": "stoo", "kor": "스포츠투데이"},
  {"site": "sisajournal", "kor": "시사저널"},
  {"site": "news.jtbc", "kor": "JTBC뉴스"},
  {"site": "etoday", "kor": "이투데이"},
  {"site": "marketnews", "kor": "마켓뉴스"},
  {"site": "woman.chosun", "kor": "여성조선"},
  {"site": "newspim", "kor": "뉴스핌"},
  {"site": "goodkyung", "kor": "굿모닝경제"},
  {"site": "siminilbo", "kor": "시민일보"},
  {"site": "ggilbo", "kor": "금강일보"},
  {"site": "sports.chosun", "kor": "스포츠조선"},
  {"site": "kihoilbo", "kor": "기호일보"},
  {"site": "dealsite", "kor": "딜사이트"},
  {"site": "mediatoday", "kor": "미디어오늘"},
  {"site": "lawtimes", "kor": "법률신문"},
  {"site": "fpn119", "kor": "소방방재신문"},
  {"site": "nbntv", "kor": "NBN"},
  {"site": "veritas-a", "kor": "베리타스알파"},
  {"site": "kpinews", "kor": "KPI뉴스"},
  {"site": "newscj", "kor": "천지일보"},
  {"site": "kyeonggi", "kor": "경기일보"},
  {"site": "khan", "kor": "경향신문"},
  {"site": "sports.donga", "kor": "스포츠동아"},
  {"site": "ebn", "kor": "EBN사업경제"},
  {"site": "journalist", "kor": "한국기자협회"},
  {"site": "getnews", "kor": "글로벌경제신문"},
  {"site": "mediaus", "kor": "미디어스"},
  {"site": "ekn", "kor": "에너지경제"},
  {"site": "donga", "kor": "동아일보"},
  {"site": "m-economynews", "kor": "이코노미뉴스"},
  {"site": "hani", "kor": "한겨레"},
  {"site": "pinpointnews", "kor": "핀포인트뉴스"},
  {"site": "boannews", "kor": "보안뉴스"},
  {"site": "etnews", "kor": "전자신문"},
  {"site": "jjan", "kor": "전북일보"},
  {"site": "ajunews", "kor": "아주경제"},
  {"site": "ohmynews", "kor": "오마이뉴스"},
  {"site": "lecturernews", "kor": "한국강사신문"},
  {"site": "news.ebs", "kor": "EBS뉴스"},
  {"site": "pointdaily", "kor": "포인트데일리"},
  {"site": "joongboo", "kor": "중부일보"},
  {"site": "asiatoday", "kor": "아시아투데이"},
  {"site": "suwonilbo", "kor": "수원일보"},
  {"site": "news.kmib", "kor": "국민일보"},
  {"site": "newsis", "kor": "뉴시스"},
  {"site": "news.joins", "kor": "중앙일보"},
  {"site": "news.jtbc", "kor": "JTBC뉴스"},
  {"site": "news.kbs", "kor": "KBS뉴스"},
  {"site": "news.mt", "kor": "머니투데이"},
  {"site": "news.sbs", "kor": "SBS뉴스"},
  {"site": "wsobi", "kor": "여성소비자신문"},
  {"site": "biz.heraldcorp", "kor": "헤럴드경제"},
  {"site": "insnews", "kor": "한국보험신문"},
  {"site": "biz.newdaily", "kor": "뉴데일리 경제"},
  {"site": "fins", "kor": "보험매일"},
  {"site": "thebell", "kor": "더벨"},
  {"site": "view.asiae", "kor": "아시아경제"},
  {"site": "daily.hankooki", "kor": "데일리한국"},
  {"site": "g-enews", "kor": "글로벌이코노믹"},
  {"site": "businessplus", "kor": "비즈니스플러스"},
  {"site": "ceoscoredaily", "kor": "CEOSCOREDAILY"},
  {"site": "econovill", "kor": "이코노믹리뷰"},
  {"site": "ftoday", "kor": "파이낸셜투데이"},
  {"site": "enewstoday", "kor": "이뉴스투데이"},
  {"site": "sisaon", "kor": "시사오늘"},
  {"site": "smedaily", "kor": "중소기업신문"},
  {"site": "4th", "kor": "포쓰저널"},
  {"site": "investchosun", "kor": "인베스트조선"},
  {"site": "joseilbo", "kor": "조세일보"},
  {"site": "kfenews", "kor": "한국금융경제신문"},
  {"site": "mhns", "kor": "문화뉴스"},
  {"site": "psnews", "kor": "퍼블릭뉴스"},
  {"site": "thefairnews", "kor": "더페어"},
  {"site": "thefirstmedia", "kor": "더퍼스트미디어"},
  {"site": "asiatime", "kor": "아시아타임즈"},
  {"site": "cnbnews", "kor": "CNB뉴스"},
  {"site": "cstimes", "kor": "컨슈머타임스"},
  {"site": "fetv", "kor": "FETV"},
  {"site": "fntimes", "kor": "한국금융신문"},
  {"site": "joongangenews", "kor": "중앙일보"},
  {"site": "newscape", "kor": "뉴스케이프"},
  {"site": "newsworks", "kor": "뉴스웍스"},
  {"site": "seoulwire", "kor": "서울와이어"},
  {"site": "businesspost", "kor": "비즈니스포스트"},
  {"site": "bloter", "kor": "블로터"},
  {"site": "dnews", "kor": "대한경제"},
  {"site": "insightkorea", "kor": "인사이트코리아"},
  {"site": "it.chosun", "kor": "IT조선"},
  {"site": "lcnews", "kor": "라이센스뉴스"},
  {"site": "metroseoul", "kor": "메트로신문"},
  {"site": "nbnews", "kor": "NBN NEWS"},
  {"site": "news.einfomax", "kor": "연합인포맥스"},
  {"site": "wolyo", "kor": "월요신문"},
  {"site": "dailysmart", "kor": "스마트경제"},
  {"site": "kukinews", "kor": "쿠키뉴스"}
]'

# JSON을 R 데이터프레임으로 변환
press_info <- fromJSON(json_data)


search_list |>
  select(originallink) |>
  mutate(originallink = str_remove(originallink, "https?:\\/\\/")) |>
  mutate(originallink = str_remove(originallink, "\\/[[:print:]]*")) |>
  mutate(originallink = str_remove_all(originallink, "www.|.co.kr|.com|.kr|.net")) |>
  count(originallink, sort = TRUE) |>
  left_join(press_info, by = c("originallink" = "site")) -> domain


search_list |>
  select(originallink) |>
  mutate(originallink = str_remove(originallink, "https?:\\/\\/")) |>
  mutate(originallink = str_remove(originallink, "\\/[[:print:]]*")) |>
  mutate(originallink = str_remove_all(originallink, "www.|.co.kr|.com|.kr|.net")) |>
  count(originallink, sort = TRUE) |>
  left_join(press_info, by = c("originallink" = "site")) |>
  group_by(kor) |>
  summarise(n = sum(n)) |>
  arrange(desc(n)) |>
  filter(!is.na(kor)) |>
  filter(row_number() <= 10) -> domain_top10



write.csv(search_list$title_text, file = "title_text.csv", fileEncoding = "UTF-8")

Article_Clusters |>
  group_by(cluster) |>
  summarise(n = n()) |>
  arrange(desc(n))

Article_Clusters |>
  filter(cluster == 28) |>
  select(x) |>
  print(n = 200)


교보생명 광화문글판 103
교보생명 풋옵션 분쟁 해소 86
교보생명 웹 드라마 사이 좁은 이웃 앤어워드 위너 수상 41
교보DTS AIFT와 파트너십 체결 40
교보생명 2025 희망다솜 장학생 증서수여식 개최 38


library(ggplot2)

# 데이터 생성
years <- c(2020, 2021, 2022, 2023)
general_account <- c(8836.6, 9560.7, 12658.4, NA)  # 2023년 데이터 없음
special_account <- c(5443.4, 6247.6, 8077.7, NA)   # 2023년 데이터 없음
total <- c(14280.0, 15808.3, 20736.1, 18987.0)

# 데이터프레임 생성
df <- data.frame(
  year = rep(years, 3),
  amount = c(general_account, special_account, total),
  category = rep(c("일반계정", "특별계정", "총계"), each = length(years))
)

# ggplot을 사용한 그래프
ggplot(df, aes(x = year, y = amount, color = category, group = category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "교보생명 연도별 수입보험료 추이",
    x = "연도",
    y = "수입보험료 (십억원)"
  ) +
  theme_minimal(base_family = "NanumSquare") +
  scale_x_continuous(breaks = years)



# 데이터 생성
years <- c(2020, 2021, 2022, 2023)
paid_insurance <- c(38500, 40200, 42000, 43217)

# 데이터프레임 생성
df <- data.frame(
  year = years,
  paid_insurance = paid_insurance
)

# ggplot을 사용한 그래프
ggplot(df, aes(x = year, y = paid_insurance)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(
    title = "교보생명 연도별 지급보험금 추이",
    x = "연도",
    y = "지급보험금 (억원)"
  ) +
  theme_minimal(base_family = "NanumSquare") +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(labels = scales::comma)

