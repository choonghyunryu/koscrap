#' 네이버 뉴스 프린트 페이지 html 추출
#'
#' @description 저작권을 고려해서 네이버 프린트 페이지의 약식 html을 추출합니다.
#'
#' @details 매체명, 제목, 입력일자, 본문의 일부, 뉴스의 URL등 네이버 뉴스 본문의 일부를 추출합니다.
#'
#' @param url character. 추출을 원하는 뉴스 URL
#' @param photo logical. 이미지 포함 여부.
#' @param remove_selectors character. html에서 제거할 요소의 사용자 정의.
#' @param n_chars integer. 본문에서 추출할 글자 수.
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
#' @importFrom xml2 xml_remove xml_find_all xml_text xml_add_child xml_root
#' @importFrom purrr walk
#' @importFrom stringr str_length str_sub
#' @export
#'
extract_news <- function(url, photo = TRUE, remove_selectors = NULL, n_chars = 150,
                         file_name = "short_page.html") {
  # 웹페이지 읽기
  page <- rvest::read_html(url)

  node_url <- xml2::xml_find_all(page, "//*[@class='print_footer']")
  ref_url <- xml2::xml_text(node_url) |>
    stringr::str_remove_all("\n|\t|[가-힣]|\\s")

  # 기본적으로 제거할 요소들
  default_removes <- c(
    ".byline",       # 기자
    ".copyright",    # 광고 클래스
    ".print_footer"  # 프린트 페이지 하단
  )

  if (!photo) {
    default_removes <- c(default_removes, c(".end_photo_org", ".nbd_table"))  # 사진 제거
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

  # 이미지 1개만 남기고 제거
  if (photo) {
    for(selector in c(".end_photo_org")) {
      nodes_to_remove <- try(rvest::html_nodes(page, selector), silent = TRUE)
      if(!inherits(nodes_to_remove, "try-error") && length(nodes_to_remove) > 1) {
        purrr::walk(2:length(nodes_to_remove), function(i) {
          xml2::xml_remove(nodes_to_remove[i])
        })
      }
    }
  }

  # 본문 텍스트 노드 추출
  node <- xml2::xml_find_all(page, "//*[@id='dic_area']/text()")
  n_nodes <- length(node)

  first_doc <- paste(xml2::xml_text(node[1]),
                     xml2::xml_text(node[2]), sep = " ") |>
    extract_sentences_by_count()

  len_first <- stringr::str_length(first_doc)

  if (len_first > n_chars) {
    first_doc <- stringr::str_c(stringr::str_sub(first_doc, end = n_chars), "...")
  }

  link_url <- stringr::str_c("<br><p>- 원문을 보려면 <a href='",
                              ref_url,
                              "' target='_blank'>[<font color='blue'>기사 전문 링크</font>]</a>를 클릭하세요.</p><br>")

  purrr::walk(seq_len(n_nodes), function(i) {
    xml2::xml_text(node[i]) <- ifelse(i > 1, "", first_doc)
  })

  node_br <- xml2::xml_find_all(page, "//*[@id='dic_area']/br")

  purrr::walk(2:length(node_br), function(i) {
    if (i > 2) {
      xml2::xml_remove(node_br[i])
    }
  })

  # 원문 링크 추가
  node <- xml2::xml_find_first(page, "//*[@id='dic_area']")
  paragraph <- rvest::read_html(link_url)
  xml2::xml_add_child(node, xml2::xml_root(paragraph))

  clean_html <- as.character(page)
  writeLines(clean_html, file_name)
}


