#' Naver News Explorer
#'
#' @description 네이버 뉴스 탐색 작업을 위한 Shiny 앱 호출
#' @return 없음
#' @author 유충현
#' @seealso \code{\link{search_naver}}
#' @examples
#' \dontrun{
#'  library(koscrap)
#'
#'  ## 텍스트 데이터 탐색기(Shiny Web Application) 호출
#'  explore_news()
#' }
#' @export
#'
explore_news <- function() {
  library(shiny)

  runApp(system.file("shiny/explore_news", package="koscrap"))
}
