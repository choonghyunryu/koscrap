#' 괄호 안의 문자열 추출
#'
#' @description 괄호 안의 문자열을 추출하기
#'
#' @details 열린 괄호와 닫힌 괄호 사이의 문자열을 추출하는 함수로, 여러 개의 괄호가 있을 경우에도 추출 가능함
#'
#' @param text character. 괄호 안의 문자열을 추출할 텍스트.
#'
#' @examples
#' \donttest{
#' # 사용 예시
#' text <- "이것은 (예시) 문장입니다. 여기 (또 다른 예시)가 있습니다."
#' extract_between_brackets(text)
#' }
#' @export
#'
# 괄호 사이의 문자를 추출하는 함수
extract_between_brackets <- function(text) {
  # 정규표현식을 사용하여 괄호 사이의 내용 추출
  # \\( : 열린 괄호 매칭 (이스케이프 처리 필요)
  # ([^()]+) : 괄호가 아닌 문자 하나 이상 캡처
  # \\) : 닫힌 괄호 매칭 (이스케이프 처리 필요)
  pattern <- "\\(([^()]+)\\)"
  matches <- regmatches(text, gregexpr(pattern, text))

  # 추출된 패턴에서 괄호 제거하고 결과 반환
  result <- gsub("\\(|\\)", "", unlist(matches))

  return(result)
}


#' 괄호 앞의 문자열 추출
#'
#' @description 시작 문자부터 열린 괄호 앞까지의 문자열을 추출하는 함수
#'
#' @details 시작 문자부터 열린 괄호 앞까지의 문자열을 추출하는 함수로 괄호가 없을 경우에는 문자열 전체 반환
#'
#' @param text character. 괄호 앞의 문자열을 추출할 텍스트.
#'
#' @examples
#' \donttest{
#' # 사용 예시
#' text1 <- "이것은 예시 문장(괄호 안의 내용)입니다."
#' text2 <- "괄호가 없는 문장입니다."
#'
#' extract_before_bracket(text1)
#' extract_before_bracket(text2)
#' }
#' @export
#'
extract_before_bracket <- function(text) {
  # 정규표현식을 사용하여 문자열 시작부터 열린 괄호 전까지 추출
  # ^ : 문자열의 시작
  # (.*?) : 모든 문자를 최소한으로 캡처 (non-greedy)
  # \\( : 열린 괄호 매칭 (이스케이프 처리 필요)
  pattern <- "^(.*?)\\("

  # 열린 괄호가 있는 경우 추출
  if(grepl("\\(", text)) {
    matches <- regmatches(text, regexpr(pattern, text))
    # 캡처된 그룹만 가져오기 (열린 괄호 제외)
    result <- gsub("\\($", "", unlist(matches))
    return(result)
  } else {
    # 열린 괄호가 없는 경우 원본 문자열 반환
    return(text)
  }
}


#' 중괄호와 개행 사이 문자열 추출
#'
#' @description 닫힌 중괄호(]) 부터 개행문자 앞까지의 문자열을 추출하는 함수
#'
#' @details 네이버 뉴스의 본문을 추출하기 위해서 중괄호(])와 개행문자 사이의 문자열을 추출하는 함수 정의
#'
#' @param text character. 중괄호와 개행 사이의 문자열을 추출할 텍스트.
#'
#' @examples
#' \donttest{
#' # 사용 예시
#' example_text <- "여기는 [키: 값] 첫 번째 추출할 내용\n다음 줄입니다. [태그] 두 번째 추출할 내용\n마지막 줄 [마크] 세 번째 내용\n"
#' extract_between_bracket_and_newline(example_text)
#' }
#' @export
#'
extract_between_bracket_and_newline <- function(text) {
  # 정규표현식 패턴: "] "와 "\n" 사이의 문자열을 찾음
  # \\] : 닫힌 대괄호 (이스케이프 처리)
  # \\s : 공백 문자
  # (.*?) : 최소한의 문자열 매칭 (non-greedy)
  # \\n : 줄바꿈 문자
  pattern <- "\\]\\s(.*?)\\n"

  # 정규 표현식으로 매칭되는 부분 찾기
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))

  # 매칭된 결과에서 "] "와 "\n" 제거
  result <- gsub("\\]\\s|\\n", "", unlist(matches))

  # 결과가 없으면 빈 문자열 벡터 반환
  if(length(result) == 0) {
    return(character(0))
  }

  return(result)
}


#' 문자열에서 지정 개수의 문장 추출
#'
#' @description 문자열에서 지정한 문장의 개수에 해당하는 문자열을 앞부분부터 추출하는 함수
#'
#' @details 네이버 뉴스의 본문에서 앞 몇 문장을 추출하기 위해서 지정한 개수의 문장을 추출하는 함수 정의함. 서두의 기자명은 제거함.
#'
#' @param text character. 문자열을 추출할 텍스트.
#'
#' @examples
#' \donttest{
#' # 사용 예시
#' example_text <- "무궁화 꽃이 피었습니다. 활짝 피었습니다. 삼천리 금수강산에 피었습니다.\n"
#' extract_sentences_by_count(example_text)
#' }
#' @importFrom stringr str_split str_remove
#' @export
#'
extract_sentences_by_count <- function(text, n_sentences = 2) {
  # 문장 분리 (한국어 기준)
  sentences <- stringr::str_split(text, "[.!?]\\s*")[[1]]
  sentences <- sentences[nzchar(sentences)]  # 빈 문자열 제거

  # 지정된 개수만큼 문장 추출
  if (length(sentences) >= n_sentences) {
    text <- paste(sentences[1:n_sentences], collapse = ". ")
  } else {
    text <- paste(sentences, collapse = ". ")
  }

  text |>
    stringr::str_remove("\\[[:print:]+\\]") |>
    stringr::str_remove(" $") |>
    paste(".", collapse = "", sep = "")
}

