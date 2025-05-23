% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrap_news.R
\name{scrap_news}
\alias{scrap_news}
\title{뉴스 내용을 스크래핑}
\usage{
scrap_news(
  url = NULL,
  file_name = NULL,
  type = c("all", "content"),
  verbose = FALSE
)
}
\arguments{
\item{url}{character. 뉴스 스크래핑 대상 URL.}

\item{file_name}{character. 스크래핑 결과를 저장할 파일 이름.}

\item{verbose}{logical. 스크래핑 결과를 파일에 저장할 때, 그 내용을 화면에 출력할지 여부.}
}
\value{
character.
}
\description{
뉴스 URL 주소를 입력받아 뉴스 내용을 스크래핑합니다.
}
\examples{
\donttest{

scrap_news("https://n.news.naver.com/mnews/article/366/0001062385?sid=101", type = "content")

scrap_news("https://www.finomy.com/news/articleView.html?idxno=224464", type = "content")

}

}
