#' 뉴스 내용을 스크래핑
#'
#' @description 뉴스 URL 주소를 입력받아 뉴스 내용을 스크래핑합니다.
#'
#' @param url character. 뉴스 스크래핑 대상 URL.
#' @param file_name character. 스크래핑 결과를 저장할 파일 이름.
#' @param verbose logical. 스크래핑 결과를 파일에 저장할 때, 그 내용을 화면에 출력할지 여부.
#'
#' @return character.
#'
#' @examples
#' \donttest{
#'
#' scrap_news("https://n.news.naver.com/mnews/article/366/0001062385?sid=101", type = "content")
#'
#' scrap_news("https://www.finomy.com/news/articleView.html?idxno=224464", type = "content")
#'
#' }
#'
#' @import rvest
#' @importFrom stringr str_detect
#' @export
#'
scrap_news <- function(url = NULL, file_name = NULL, type = c("all", "content"),
                       verbose = FALSE) {
  if (is.null(url)) {
    stop("뉴스 게시 주소 url을 입력하지 않았습니다.")
  }

  is_naver <- stringr::str_detect(url, "n.news.naver.com")

  # 웹페이지 HTML 읽기
  webpage <- read_html(url)

  if (is_naver) {
    target_tag <- "article"

    if (type == "all") {
      target_tag <- "div.media_end_summary"
    }
  } else {
    target_tag <- "p"
  }

  # 특정 태그 선택 (예: <p> 태그의 내용 가져오기)
  content <- webpage |>
    html_nodes(target_tag) |>
    html_text()

  # 파일로 저장 (선택 사항)
  if (!is.null(file_name)) {
    writeLines(content, file_name)

    if (verbose) {
      cat(paste(content, collapse = "\n"))
    }
  } else {
    return(paste(content, collapse = "\n"))
  }
}

