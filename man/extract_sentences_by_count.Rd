% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_sentences_by_count}
\alias{extract_sentences_by_count}
\title{문자열에서 지정 개수의 문장 추출}
\usage{
extract_sentences_by_count(text, n_sentences = 2)
}
\arguments{
\item{text}{character. 문자열을 추출할 텍스트.}
}
\description{
문자열에서 지정한 문장의 개수에 해당하는 문자열을 앞부분부터 추출하는 함수
}
\details{
네이버 뉴스의 본문에서 앞 몇 문장을 추출하기 위해서 지정한 개수의 문장을 추출하는 함수 정의함. 서두의 기자명은 제거함.
}
\examples{
\donttest{
# 사용 예시
example_text <- "무궁화 꽃이 피었습니다. 활짝 피었습니다. 삼천리 금수강산에 피었습니다.\n"
extract_sentences_by_count(example_text)
}
}
