#' Get API keys from package file
#' @description 패키지 파일에 등록된 공공데이터 포털 API key와 NAVER API key를 조회합니다.
#' @details regist_api_key를 사용하지 않고, set_api_key()로 API key를 설정한 경우라면, get_api_key() 대신에
#' Sys.getenv("PUBGOV_API_KEY"), Sys.getenv("NAVER_CLIENT_ID"), Sys.getenv("NAVER_CLIENT_SECRET")를 사용하세요.
#' @examples
#' \dontrun{
#' # get_api_key()
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64decode
get_api_key <- function() {
  pubgov_file <- system.file(".pubgov", package = "koscrap")
  naver_file  <- system.file(".naver", package = "koscrap")

  # for 공공데이터 포털 API key
  if (pubgov_file != "") {
    con <- file(pubgov_file, "r")

    tryCatch({
      pubgov_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()
    },
    finally = {
      close(con)
    })
  } else {
    pubgov_api_key = NULL
  }

  # for NAVER API key
  if (naver_file != "") {
    con <- file(naver_file, "r")

    tryCatch({
      naver_api_key <- readLines(con) %>%
        base64enc::base64decode() %>%
        rawToChar()

      naver_api_key <- strsplit(naver_api_key, ":")

      naver_client_id <- naver_api_key[[1]][1]
      naver_client_secret <- naver_api_key[[1]][2]
    },
    finally = {
      close(con)
    })
  } else {
    naver_client_id = NULL
    naver_client_secret = NULL
  }

  list(pubgov_api_key = pubgov_api_key,
       naver_client_id = naver_client_id,
       naver_client_secret = naver_client_secret)
}


#' Set API key to system environment
#' @description Open API와 인터페이스하기 위한 API key를 설정합니다.
#' @param type character. 등록할 공공데이터 포털 API key의 종류.
#' @param api_key character. 등록할 API key.
#' @param client_id character. 등록할 NAVER client id.
#' @param client_secret character. 등록할 NAVER client secret.
#' @details 만약에 여러 사용자가 사용하는 환경이 아닌 개인 컴퓨터에 koscrap 패키지를 설치한 경우라면,
#' set_api_key() 대신에 매번 API key를 등록할 필요없는 regist_api_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 공공데이터 포털 API key를 사용합니다.
#' # set_api_key("pubgov", api_key = "XXXXXXXXXXX")
#'
#' # 실제 사용자가 할당받은 NAVER API key를 사용합니다.
#' # set_api_key("naver", client_id = "XXXXXX", client_secret = "XXXXXXXXX")
#' }
#' @export
set_api_key <- function(type = c("pubgov", "naver"), api_key = NULL,
                        client_id = NULL, client_secret = NULL) {
  type <- match.arg(type)

  if (type == "pubgov") {
    if (is.null(api_key)) {
      stop("API key is required.")
    }

    do.call(Sys.setenv, setNames(list(api_key), "PUBGOV_API_KEY"))
  }

  if (type == "naver") {
    if (is.null(client_id)) {
      stop("NAVER client ID is required.")
    }

    if (is.null(client_secret)) {
      stop("NAVER client secret is required.")
    }

    do.call(Sys.setenv, setNames(list(client_id), "NAVER_CLIENT_ID"))
    do.call(Sys.setenv, setNames(list(client_secret), "NAVER_CLIENT_SECRET"))
  }
}


#' Regist API key to package file
#' @description Open API와 인터페이스하기 위한 API key를 등록합니다.
#' @param type character. 등록할 API key의 종류.
#' @param api_key character. 등록할 공공데이터 포털 API key.
#' @param client_id character. 등록할 NAVER client id.
#' @param client_secret character. 등록할 NAVER client secret.
#' @details 만약에 개인 컴퓨터가 아닌 여러 사용자가 사용하는 환경에 koscrap 패키지를 설치한 경우라면,
#' API key의 보안을 위해서 regist_api_key()대신 set_api_key()를 사용하세요.
#' @examples
#' \dontrun{
#' # 실제 사용자가 할당받은 공공데이터 포털 API key를 사용합니다.
#' # regist_api_key("pubgov", api_key = "XXXXXXXXXXX")
#' }
#' @export
#' @import dplyr
#' @importFrom base64enc base64encode
regist_api_key <- function(type = c("pubgov", "naver"), api_key = NULL,
                           client_id = NULL, client_secret = NULL) {
  type <- match.arg(type)

  if (type == "pubgov") {
    if (is.null(api_key)) {
      stop("API key is required.")
    }

    key_file <- file.path(system.file(package = "koscrap"), paste0(".", "pubgov"))

    decode_api_key <- api_key |>
      charToRaw() |>
      base64enc::base64encode()

    set_api_key(type, api_key)
  }

  if (type == "naver") {
    if (is.null(client_id)) {
      stop("NAVER client ID is required.")
    }

    if (is.null(client_secret)) {
      stop("NAVER client secret is required.")
    }

    key_file <- file.path(system.file(package = "koscrap"), paste0(".", "naver"))

    decode_api_key <- paste0(client_id, ":", client_secret) |>
      charToRaw() |>
      base64enc::base64encode()

    set_api_key(type, client_id, client_secret)
  }

  if (!file.exists(key_file)) {
    con <- file(key_file, "w")
    tryCatch({
      cat(decode_api_key, file = con, sep = "\n")
    }, finally = {
      close(con)
    })
  }
}
