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
#' @importFrom tibble as_tibble
#' @export
#'
search_naver <- function(query = NULL, chunk = 100, chunk_no = 1,
                         sort = c("date", "sim"), do_done = FALSE,
                         max_record = 1000L,
                         client_id = Sys.getenv("NAVER_CLIENT_ID"),
                         client_secret = Sys.getenv("NAVER_CLIENT_SECRET"),
                         verbose = TRUE) {
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
    doc |>
      XML::getNodeSet("//item") |>
      XML::xmlToDataFrame() |>
      rename("publish_date" = pubDate) |>
      mutate(publish_date = as.POSIXct(publish_date,
                                       format = "%a, %d %b %Y %H:%M:%S %z")) |>
      mutate(title_text = stringr::str_remove_all(
        title, "&\\w+;|<[[:punct:]]*b>")) |>
      mutate(title_text = stringr::str_remove_all(
        title_text, "[[:punct:]]*")) |>
      mutate(description_text = stringr::str_remove_all(
        description,
        "&\\w+;|<[[:punct:]]*b>|[“”]"))
  }

  searchUrl <- "https://openapi.naver.com/v1/search/news.xml"

  query <- query |>
    enc2utf8() |>
    URLencode()

  url <- glue::glue("{searchUrl}?query={query}&display={chunk}&start={chunk_no}&sort={sort}")

  doc <- url |>
    httr::GET(
      httr::add_headers(
        "X-Naver-Client-Id"     = client_id,
        "X-Naver-Client-Secret" = client_secret
      )
    ) |>
    toString() |>
    XML::xmlParse()

  total_count <- doc |>
    XML::getNodeSet("//total") |>
    XML::xmlValue() |>
    as.integer()

  if (verbose) {
    glue::glue("* 검색된 총 기사 건수는 {total_count}건입니다.\n\n") |>
      cat()

    glue::glue("  - ({chunk}/{min(total_count, max_record)})건 호출을 진행합니다.\n\n") |>
      cat()
  }

  search_list <- doc |>
    get_list()

  records <- NROW(search_list)

  if (!do_done | records >= total_count | records >= max_record) {
    return(search_list)
  } else {
    total_count <- min(total_count, max_record)

    cnt <- total_count %/% chunk
    if (total_count %% chunk == 0) {
      cnt <- cnt - 1
    }

    idx <- (seq(cnt) + 1)

    add_list <- idx[idx <= 1000] |>
      purrr::map_df({
        function(x) {
          if (verbose) {
            glue::glue("  - ({chunk * x}/{total_count})건 호출을 진행합니다.\n\n") |>
              cat()
          }

          glue::glue(
            "{searchUrl}?query={query}&display={chunk}&start={(x - 1) * chunk + chunk_no}&sort={sort}"
          ) |>
            httr::GET(
              httr::add_headers(
                "X-Naver-Client-Id"     = client_id,
                "X-Naver-Client-Secret" = client_secret
              )
            ) |>
            toString() |>
            XML::xmlParse() |>
            get_list()
        }
      })

    search_list |>
      bind_rows(
        add_list
      ) |>
      tibble::as_tibble()
  }
}


#' 네이버 통합 검색어 트렌드 검색
#'
#' @description 네이버 통합 검색어 트렌드 검색를 출력해주는 REST API를 호출하여, 통합 검색어 트렌드를 검색합니다.
#'
#' @details 네이버에서 발급받은 Client ID, Client Secret는 개인이 발급받은 키를 사용하며,
#' 유출되어서는 안됩니다.
#'
#' @param keywords character. 주제어에 해당하는 검색어. 최대 20개의 검색어를 설정할 수 있음.
#' @param titles character. 주제어. 검색어 묶음을 대표하는 이름.
#' @param start_date character. 조회 기간 시작 날짜(yyyy-mm-dd 형식). 2016년 1월 1일부터 조회할 수 있음.
#' @param end_date character. 조회 기간 종료 날짜(yyyy-mm-dd 형식).
#' @param time_unit character. 구간 단위. "month"(월), "date"(일), "week"(주) 중 하나.
#' @param device character. 범위. 검색 환경에 따른 조건. "all"(전체), "pc"(PC), "mo"(모바일) 중 하나.
#' @param gender character. 성별. 검색 사용자의 성별에 따른 조건. "all"(전체), "m"(남성), "f"(여성) 중 하나.
#' @param ages character. 연령대. 검색 사용자의 연령대에 따른 조건. "all"(전체), "0∼12", "13∼18", "19∼24",
#' "25∼29", "30∼34", "35∼39", "40∼44", "45∼49", "50∼54", "55∼59", "60~" 중 선택. 복수 선택 가능.
#' @param client_id character. 애플리케이션 등록 시 발급받은 Client ID
#' @param client_secret character. 애플리케이션 등록 시 발급받은 Client Secret
#'
#' @return tibble
#' 변수 목록은 다음과 같음.:
#' \itemize{
#' \item title : character. 검색어의 타이틀(주제어)
#' \item keywords : character. 주제어에 해당하는 검색어
#' \item period : character. 구간별 시작 날짜(yyyy-mm-dd 형식)
#' \item ratio : numeric. 구간별 검색량의 상대적 비율. 구간별 결과에서 가장 큰 값을 100으로 설정한 상댓값.
#' }
#'
#' @examples
#' \donttest{
#' # Your authorized API keys
#' client_id <- "XXXXXXXXXXXXXXXXXXXXXXX"
#' client_secret <- "XXXXXXXXX"
#'
#' get_naver_trend(keywords = c("AI,인공지능", "ChatGPT,챗GPT"),
#'   titles = c("AI", "ChatGPT"),
#'   start_date = "2024-01-01",
#'   end_date = "2025-03-15",
#'   time_unit = "month",
#'   device = "pc",
#'   ages = c("13∼18", "19∼24"),
#'   gender = "all",
#'   client_id = client_id,
#'   client_secret = client_secret
#'   )
#'
#' }
#'
#' @importFrom purrr map map_df
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom tibble as_tibble
#' @export
#'
get_naver_trend <- function(keywords = NULL, titles = NULL,
                            start_date = NULL, end_date = NULL,
                            time_unit = c("month", "date", "week"),
                            device = c("all", "pc", "mo"),
                            ages = c("all", "0∼12", "13∼18", "19∼24", "25∼29",
                                     "30∼34", "35∼39", "40∼44", "45∼49", "50∼54",
                                     "55∼59", "60~"),
                            gender = c("all", "m", "f"),
                            client_id = Sys.getenv("NAVER_CLIENT_ID"),
                            client_secret = Sys.getenv("NAVER_CLIENT_SECRET")) {
  # get arguments
  time_unit <- match.arg(time_unit)
  device <- match.arg(device)
  gender <- match.arg(gender)

  ages <- which(c("0∼12", "13∼18", "19∼24", "25∼29",
  "30∼34", "35∼39", "40∼44", "45∼49", "50∼54",
  "55∼59", "60~") %in% ages) |>
    as.character()

  if (is.null(keywords)) {
    stop("검색 키워드인 keywords를 입력하지 않았습니다.")
  }

  if (is.null(client_id) | is.null(client_secret)) {
    stop("네이버 API 인증 정보인 client_id와 client_secret를 입력하지 않았습니다.")
  }

  if (is.null(start_date) | is.null(end_date)) {
    stop("검색 기간인 start_date와 end_date를 입력하지 않았습니다.")
  }

  if (start_date > end_date) {
    stop("검색 기간의 시작 날짜가 종료 날짜보다 늦습니다.")
  }

  if (start_date < "2016-01-01") {
    stop("검색 기간의 시작 날짜가 2016년 1월 1일 이전입니다. 2016년 1월 1일부터 조회할 수 있습니다.")
  }

  # 네이버 API URL
  url <- "https://openapi.naver.com/v1/datalab/search"

  # 검색어 그룹 설정 (키워드 리스트를 자동 변환)
  keyword_groups <- purrr::map(seq(keywords), function(x) {
    if (is.null(titles)) {
      gname <- keywords[x]
    } else {
      gname <- titles[x]
    }

    list(groupName = gname, keywords = unlist(strsplit(keywords[x], ",")))
  })

  # 요청할 데이터 설정
  body <- list(
    startDate = start_date,  # 시작 날짜 (YYYY-MM-DD)
    endDate = end_date,      # 종료 날짜
    timeUnit = time_unit,    # "date", "week", "month" 선택 가능
    keywordGroups = keyword_groups,
    device = device,         # "pc", "mobile", "all" 가능
    ages = ages,             # 연령대 (NULL = 전체 연령)
    gender = gender         # "m" (남성), "f" (여성), "all" 가능
  )

  if (gender == "all") {
    body[["gender"]] <- NULL
  }

  if (any(ages %in% "all")) {
    body[["ages"]] <- NULL
  }

  if (device == "all") {
    body[["device"]] <- NULL
  }

  # API 요청
  response <- httr::POST(
    url,
    httr::add_headers(
      "X-Naver-Client-Id" = client_id,
      "X-Naver-Client-Secret" = client_secret,
      "Content-Type" = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json"
  )

  # 응답 데이터 확인
  data <- httr::content(response, "text", encoding = "UTF-8")
  parsed_data <- jsonlite::fromJSON(data)

  if (response$status_code != "200") {
    stop(parsed_data$errorMessage)
  }

  # 데이터프레임 변환
  purrr::map_df(seq(NROW(parsed_data$results)), function(x) {
    title <- parsed_data$results$title[[x]]
    result <- data.frame(keywords = parsed_data$results$keywords[[x]] |> paste(collapse = ", "))
    cbind(title, result, as.data.frame(parsed_data$results$data[[x]]))
  }) |>
    tibble::as_tibble()
}




