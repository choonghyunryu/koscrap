% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/env.R
\name{get_api_key}
\alias{get_api_key}
\title{Get API keys from package file}
\usage{
get_api_key()
}
\description{
패키지 파일에 등록된 공공데이터 포털 API key와 NAVER API key를 조회합니다.
}
\details{
regist_api_key를 사용하지 않고, set_api_key()로 API key를 설정한 경우라면, get_api_key() 대신에
Sys.getenv("PUBGOV_API_KEY"), Sys.getenv("NAVER_CLIENT_ID"), Sys.getenv("NAVER_CLIENT_SECRET")를 사용하세요.
}
\examples{
\dontrun{
# get_api_key()
}
}
