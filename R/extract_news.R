#' 네이버 뉴스 프린트 페이지 html 추출
#'
#' @description 저작권을 고려해서 네이버 프린트 페이지의 약식 html을 추출합니다.
#'
#' @details 매체명, 제목, 입력일자, 본문의 일부, 뉴스의 URL등 네이버 뉴스 본문의 일부를 추출합니다.
#'
#' @param url character. 추출을 원하는 뉴스 URL
#' @param photo logical. 이미지 포함 여부.
#' @param remove_selectors character. html에서 제거할 요소의 사용자 정의.
#' @param file_name character. 출력할 html 파일 이름
#'
#' @examples
#' \donttest{
#' url <- "https://n.news.naver.com/article/print/138/0002197754"
#' extract_news(url)
#'
#' }
#'
#' @importFrom rvest read_html html_nodes
#' @importFrom xml2 xml_remove
#' @export
#'
extract_news <- function(url, photo = TRUE, remove_selectors = NULL,
                         file_name = "short_page.html") {
  # 웹페이지 읽기
  page <- rvest::read_html(url)

  # 기본적으로 제거할 요소들
  default_removes <- c(
    ".byline",     # 기자
    ".copyright",  # 광고 클래스
  )

  if (!photo) {
    default_removes <- c(default_removes, ".end_photo_org")  # 사진 제거
  }

  # 사용자 지정 선택자와 기본 선택자 결합
  all_selectors <- c(default_removes, remove_selectors)

  # 각 선택자에 대해 요소 제거
  for(selector in all_selectors) {
    nodes_to_remove <- try(rvest::html_nodes(page, selector), silent = TRUE)
    if(!inherits(nodes_to_remove, "try-error") && length(nodes_to_remove) > 0) {
      xml2::xml_remove(nodes_to_remove)
    }
  }

  clean_html <- as.character(page)
  writeLines(clean_html, file_name)
}
