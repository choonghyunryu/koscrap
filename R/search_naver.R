#' 네이버 뉴스 검색
#'
#' @description 네이버 뉴스 검색 결과를 출력해주는 REST API를 호출하여, 뉴스 정보를 검색합니다.
#'
#' @details 네이버에서 발급받은 Client ID, Client Secret는 개인이 발급받은 키를 사용하며,
#' 유출되어서는 안됩니다.
#'
#' @param query character. 검색을 원하는 문자열
#' @param chunk_no integer. 검색 시작 위치로 최대 1000까지 가능
#' @param chunk integer. 검색 결과 출력 건수 지정 (1~100)
#' @param do_done logical. 한번의 호출로 모든 조회 결과를 가져오지 못할 경우,
#' 추가로 호출해서 모든 결과를 가져올지의 여부
#' @param sort character. 정렬 옵션: sim (유사도순), date (날짜순)
#' @param max_record integer. 최대 조회할 건수. 실제로 검색한 건수는 max_record와 정확히 일치하지 않을 수 있습니다.
#' chunk의 개수로 데이터를 수집하기 때문에 일반적인 경우에는 max_record보다 같거나  큰 chunk의 배수만큼 데이터를 가져옵니다.
#' do_done가 FALSE일 경우에는 적용되지 않습니다.
#' @param client_id character. 애플리케이션 등록 시 발급받은 Client ID
#' @param client_secret character. 애플리케이션 등록 시 발급받은 Client Secret
#'
#' @return data.frame
#' 변수 목록은 다음과 같음.:
#' \itemize{
#' \item title : character. 기사의 타이틀
#' \item originallink : character. 검색 결과 문서의 제공 언론사 하이퍼텍스트 link
#' \item link : character. 검색 결과 문서의 제공 네이버 하이퍼텍스트 link
#' \item description : character. 검색 결과 문서의 내용을 요약한 패시지 정보.
#' 문서 전체의 내용은 link를 따라가면 읽을 수 있음. 패시지에서 검색어와 일치하는 부분은 태그로 감싸져 있음
#' \item publish_date : POSIXct. 검색 결과 문서가 네이버에 제공된 시간
#' \item title_text : character. 타이틀에서 HTML 태크를 제거한 텍스트
#' \item description_text : character. 요약한 패시지 정보에서 HTML 태크를 제거한 텍스트
#' }
#'
#' @examples
#' \donttest{
#' # Your authorized API keys
#' client_id <- "XXXXXXXXXXXXXXXXXXXXXXX"
#' client_secret <- "XXXXXXXXX"
#'
#' search_list <- search_naver(
#'   "불평등", client_id = client_id, client_secret = client_secret
#' )
#'
#' search_list <- search_naver(
#'   "불평등", client_id = client_id, client_secret = client_secret,
#'   do_done = TRUE, max_record = 350
#' )
#'
#' }
#'
#' @import dplyr
#' @importFrom XML xmlParse getNodeSet xmlValue xmlToDataFrame
#' @importFrom httr GET add_headers
#' @importFrom purrr map_df
#' @importFrom glue glue
#' @export
#'
search_naver <- function(query = NULL, chunk = 100, chunk_no = 1,
                         sort = c("date", "sim"), do_done = FALSE,
                         max_record = NULL, client_id = NULL,
                         client_secret = NULL, verbose = TRUE) {
  if (is.null(query)) {
    stop("검색 키워드인 query를 입력하지 않았습니다.")
  }

  if (chunk < 1 & chunk > 100) {
    stop("chunk 요청 변수값이 허용 범위(1~100)인지 확인해 보세요.")
  }

  if (chunk_no < 1 & chunk_no > 100) {
    stop("chunk_no 요청 변수값이 허용 범위(1~1000)인지 확인해 보세요.")
  }

  sort <- match.arg(sort)

  get_list <- function(doc) {
    doc %>%
      XML::getNodeSet("//item") %>%
      XML::xmlToDataFrame() %>%
      rename("publish_date" = pubDate) %>%
      mutate(publish_date = as.POSIXct(publish_date,
                                       format = "%a, %d %b %Y %H:%M:%S %z")) %>%
      mutate(title_text = stringr::str_remove_all(
        title, "&\\w+;")) %>%
      mutate(title_text = stringr::str_remove_all(
        title_text, "[[:punct:]]*")) %>%
      mutate(description_text = stringr::str_remove_all(
        description,
        "&\\w+;|<[[:punct:]]*b>|[“”]"))
  }

  searchUrl <- "https://openapi.naver.com/v1/search/news.xml"

  query <- query %>%
    enc2utf8() %>%
    URLencode()

  url <- glue::glue("{searchUrl}?query={query}&display={chunk}&start={chunk_no}&sort={sort}")

  doc <- url %>%
    httr::GET(
      httr::add_headers(
        "X-Naver-Client-Id"     = client_id,
        "X-Naver-Client-Secret" = client_secret
      )
    ) %>%
    toString() %>%
    XML::xmlParse()

  total_count <- doc %>%
    XML::getNodeSet("//total") %>%
    XML::xmlValue() %>%
    as.integer()

  if (verbose) {
    glue::glue("* 검색된 총 기사 건수는 {total_count}건입니다.\n\n") %>%
      cat()

    glue::glue("  - ({chunk}/{min(total_count, max_record)})건 호출을 진행합니다.\n\n") %>%
      cat()
  }

  search_list <- doc %>%
    get_list()

  records <- NROW(search_list)

  if (!do_done | records >= total_count) {
    return(search_list)
  } else {
    total_count <- min(total_count, max_record)

    cnt <- total_count %/% chunk
    if (total_count %% chunk == 0) {
      cnt <- cnt - 1
    }

    idx <- (seq(cnt) + 1)

    add_list <- idx[idx <= 1000] %>%
      purrr::map_df({
        function(x) {
          if (verbose) {
            glue::glue("  - ({chunk * x}/{total_count})건 호출을 진행합니다.\n\n") %>%
              cat()
          }

          glue::glue(
            "{searchUrl}?query={query}&display={chunk}&start={x}&sort={sort}"
          ) %>%
            httr::GET(
              httr::add_headers(
                "X-Naver-Client-Id"     = client_id,
                "X-Naver-Client-Secret" = client_secret
              )
            ) %>%
            toString() %>%
            XML::xmlParse() %>%
            get_list()
        }
      })

    search_list %>%
      bind_rows(
        add_list
      ) %>%
      return()
  }
}
