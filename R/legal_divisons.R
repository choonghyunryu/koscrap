#' 행정구역 코드 정보
#'
#' @description
#' 행정표준관리시스템의 법정동코드 전체자료로부터 추출한 광역시도,시군구, 읍면동 레벨의 코드 정보.
#'
#' @details
#' 공공데이터포털에서 Open API로 행정구역 관련 데이터를 수집할 때, 입력 파라미터로 조직 코드를 사용할 경우가 많습니다.
#' 이때 이 정보를 이용해서 원하는 지역의 정보를 수집할 수 있습니다.
#'
#' @format 45953 관측치와 9 변수를 갖는 data.frame. 다음과 같은 변수를 포함합니다.:
#' \describe{
#'   \item{DIVISION_ID}{charcter. 법정동 코드.}
#'   \item{DIVISION_NM}{charcter. 법정동 이름.}
#'   \item{MAINTAIN}{logical. 유지여부.}
#'   \item{MEGA_CD}{charcter. 광역시도 코드.}
#'   \item{MEGA_NM}{charcter. 광역시도 이름.}
#'   \item{CTY_CD}{charcter. 시군구 코드.}
#'   \item{CTY_NM}{charcter. 시군구 이름.}
#'   \item{ADMI_CD}{charcter. 읍면동 코드.}
#'   \item{ADMI_NM}{charcter. 읍면동 이름.}
#' }
#' @docType data
#' @keywords datasets
#' @name legal_divisions
#' @usage data(legal_divisions)
#' @source {
#' "행정표준관리시스템" <https://www.code.go.kr/stdcodesrch/codeAllDownloadL.do>
#' }
NULL

# library(dplyr)
# fname <- here::here("inst", "meta", "법정동코드 전체자료.txt")
# legal_divisions <- fname %>%
#   read.table(sep = "\t", header = TRUE, fileEncoding = "cp949",
#              col.names = c("DIVISION_ID", "DIVISION_NM", "MAINTAIN")) %>%
#   mutate(DIVISION_ID = format(DIVISION_ID, scientific = FALSE, trim = TRUE)) %>%
#   mutate(MAINTAIN = case_when(
#     MAINTAIN == "존재" ~ "Y",
#     MAINTAIN == "폐지" ~ "N")
#   ) %>%
#   mutate(MEGA_CD = substr(DIVISION_ID, 1, 2),
#          MEGA_NM = stringr::str_extract(DIVISION_NM, "^[\\w]+")) %>%
#   mutate(CTY_CD = substr(DIVISION_ID, 1, 5),
#          CTY_NM = stringr::str_extract(DIVISION_NM, " [\\w]+") %>%
#            stringr::str_remove("\\s")) %>%
#   mutate(ADMI_CD = substr(DIVISION_ID, 1, 8),
#          ADMI_NM = stringr::str_remove(DIVISION_NM, "^[\\w]+ [\\w]+ ")) %>%
#   filter(!stringr::str_detect(DIVISION_ID, "000000$"))
#
# save(legal_divisions, file = "data/legal_divisions.rda")
#
#
# db_name <- here::here("inst", "meta", "GISDB.sqlite")
#
# con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
# DBI::dbWriteTable(con, "TB_LEGAL_DIVISIONS", legal_divisions, overwrite = TRUE)
# DBI::dbDisconnect(con)
