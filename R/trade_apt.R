#' 아파트 실거래 데이터 가져오기
#'
#' @description 공공데이터포털에서 REST open API로 아파트 실거래 데이터를 수집합니다.
#'
#' @details 공공데이터포털에서 발급받은 API 인증키는 개인이 발급받은 키를 사용하며,
#' 유출되어서는 안됩니다.
#'
#' @param auth_key character. 공공데이터포털에서 발급받은 API 인증키
#' @param LAWD_CD character. 지역코드. 각 지역별 코드 행정표준코드관리시스템
#' (www.code.go.kr)의 법정동코드 10자리 중 앞 5자리
#' @param DEAL_YMD character. 실거래 자료의 계약년월(6자리)
#' @param chunk_no integer. 페이지번호
#' @param chunk integer. 한 페이지 결과 수
#' @param do_done logical. 한번의 호출로 모든 조회 결과를 가져오지 못할 경우,
#' 추가로 호출해서 모든 결과를 가져올지의 여부
#'
#' @return data.frame
#' 변수 목록은 다음과 같음.:
#' \itemize{
#' \item LAWD_CD : character. 지역코드
#' \item DEAL_DATE : character. 거래일자
#' \item SERIAL : character. 일련번호
#' \item BUILD_NM : character. 아파트 이름
#' \item FLOOR : integer. 층
#' \item BUILD_YEAR : integer. 건축년도
#' \item AREA : numeric. 전용면적
#' \item AMOUNT : integer. 거래금액
#' \item ROAD_CD : character. 도로명코드
#' \item ROAD_NM : character. 도로명
#' \item BUILD_MAJOR : character. 도로명건물본번호코드
#' \item BUILD_MINOR : character. 도로명건물부번호코드
#' \item ROAD_SEQ : character. 도로명일련번호코드
#' \item BASEMENT_FLAG : character. 도로명지상지하코드
#' \item LAND_NO : character. 지번
#' \item DONG_NM : character. 법정동
#' \item DONG_MAJOR : character. 법정동본번코드
#' \item DONG_MINOR : character. 법정동부번코드
#' \item EUBMYNDONG_CD : character. 법정동읍면동코드
#' \item DONG_LAND_NO : character. 법정동지번코드
#' \item DEALER_ADDR : character. 중개사소재지
#' \item CANCEL_DEAL : character. 해제여부
#' \item CANCEL_DATE : character. 해제사유발생일
#' }
#'
#' @examples
#' \donttest{
#' # Your authorized API keys
#' auth_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#'
#' result <- trade_apt(LAWD_CD = "11680", DEAL_YMD = "202104", auth_key = auth_key)
#' result <- trade_apt(LAWD_CD = "11680", DEAL_YMD = "202104", do_done = TRUE,
#'                     auth_key = auth_key)
#'
#' }
#'
#' @import dplyr
#' @importFrom XML xmlParse getNodeSet xmlValue xmlToDataFrame
#' @importFrom stringr str_pad
#' @importFrom purrr map_df
#' @importFrom glue glue
#' @export
trade_apt <- function(LAWD_CD = "11110", DEAL_YMD = "202112",
                      chunk_no = 1, chunk = 400, do_done = FALSE,
                      auth_key = Sys.getenv("PUBGOV_API_KEY")) {
  library(dplyr)

  get_list <- function(doc) {
    dframe <- doc %>%
      XML::getNodeSet("//item") %>%
      XML::xmlToDataFrame()

    if (NROW(dframe) == 0) {
      return(data.frame())
    }

    # vname <- c("AMOUNT", "DEAL_TYPE", "BUILD_YEAR", "YEAR", "ROAD_NM",
    #            "BUILD_MAJOR", "BUILD_MINOR", "ROAD_ADMI", "ROAD_SEQ",
    #            "BASEMENT_FLAG", "ROAD_CD", "DONG_NM", "DONG_MAJOR",
    #            "DONG_MINOR", "DONG_ADMI", "EUBMYNDONG_CD", "DONG_LAND_NO",
    #            "BUILD_NM", "MONTH", "DAY", "SERIAL", "AREA", "DEALER_ADDR",
    #            "LAND_NO", "LAWD_CD", "FLOOR", "CANCEL_DATE", "CANCEL_DEAL")
    names(dframe) <- names(dframe) |> toupper()

    dframe %>%
      mutate(DEALDATE = glue::glue("{DEALYEAR}-{str_pad(DEALMONTH, width = 2, pad = '0')}-{
                                  str_pad(DEALDAY, width = 2, pad = '0')}")) %>%
      mutate(DEALAMOUNT = as.integer(stringr::str_remove_all(DEALAMOUNT, ","))) %>%
      mutate(FLOOR = as.integer(FLOOR)) %>%
      mutate(BUILDYEAR = as.integer(BUILDYEAR))
  }

  api <- "http://apis.data.go.kr/1613000/RTMSDataSvcAptTradeDev/getRTMSDataSvcAptTradeDev"
  url <- glue::glue(
    "{api}?ServiceKey={auth_key}&pageNo={chunk_no}&numOfRows={chunk}&LAWD_CD={LAWD_CD}&DEAL_YMD={DEAL_YMD}"
  )

  doc <- XML::xmlParse(url, encoding = "UTF-8")

  resultCode <- doc %>%
    XML::getNodeSet("//resultCode") %>%
    XML::xmlValue()

  if (resultCode != "000") {
    result_msg <- doc %>%
      XML::getNodeSet("//resultMsg") %>%
      XML::xmlValue()

    stop(result_msg)
  }

  total_count <- doc %>%
    XML::getNodeSet("//totalCount") %>%
    XML::xmlValue() %>%
    as.integer()

  deal_list <- doc %>%
    get_list()

  records <- NROW(deal_list)

  if (!do_done | records >= total_count) {
    return(deal_list)
  } else {
    cnt <- total_count %/% chunk
    if (total_count %% chunk == 0) {
      cnt <- cnt - 1
    }

    add_list <- (seq(cnt) + 1) %>%
      purrr::map_df({
        function(x) {
          url <- glue::glue(
            "{api}?ServiceKey={auth_key}&pageNo={x}&numOfRows={chunk}&LAWD_CD={LAWD_CD}&DEAL_YMD={DEAL_YMD}"
          )

          XML::xmlParse(url) %>%
            get_list()
        }
      })

    deal_list %>%
      bind_rows(
        add_list
      ) %>%
      return()
  }
}




