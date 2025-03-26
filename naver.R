get_naver_trend(keywords = c("AI,인공지능", "ChatGPT,챗GPT"),
                titles = c("AI", "ChatGPT"),
                start_date = "2024-01-01",
                end_date = "2025-03-15",
                time_unit = "month",
                device = "pc",
                ages = c("13∼18", "19∼24"),
                gender = "all",
                client_id = client_id,
                client_secret = client_secret
)

get_naver_trend(keywords = c("AI"),
                titles = c("AI"),
                start_date = "2024-01-01",
                end_date = "2025-03-15",
                time_unit = "month",
                device = "pc",
                ages = c("13∼18", "19∼24"),
                gender = "all",
                client_id = client_id,
                client_secret = client_secret
)


curl https://openapi.naver.com/v1/datalab/search \
--header "X-Naver-Client-Id: LSsT12eb0hywEtYBP9y8" \
--header "X-Naver-Client-Secret: rjSyBJpl8K" \
--header "Content-Type: application/json" \
-d @<(cat <<EOF
      {
        "startDate": "2017-01-01",
        "endDate": "2017-04-30",
        "timeUnit": "month",
        "keywordGroups": [
          {
            "groupName": "한글",
            "keywords": [
              "한글",
              "korean"
            ]
          },
          {
            "groupName": "영어",
            "keywords": [
              "영어",
              "english"
            ]
          }
        ],
        "device": "pc",
        "ages": [
          "1",
          "2"
        ],
        "gender": "f"
      }
      EOF
)

t9iL0Yqma3NV8lTJow87
QQ3gvfN8CE



webpage |>
  html_nodes(target_tag) |>
  html_text()


# title text in Naver news
webpage |>
  rvest::html_element(css = "#title_area") |>
  rvest::html_text()

# category in Naver news
webpage |>
  rvest::html_nodes("span.Nitem_link_menu")

idx <- webpage |>
  rvest::html_nodes("a.Nitem_link") |>
  rvest::html_attr("aria-selected") |>
  stringr::str_detect("true") |>
  which()

webpage |>
  rvest::html_nodes("span.Nitem_link_menu") |>
  rvest::html_text() |>
  purrr::pluck(idx)


# journalist name in Naver news
webpage |>
  rvest::html_nodes("em.media_end_head_journalist_name") |>
  rvest::html_text() |>
  stringr::str_remove_all(" 기자")

# journalist email in Naver news
webpage |>
  rvest::html_nodes("span.byline_s") |>
  rvest::html_text() |>
  stringr::str_remove_all("^[[:print:]]+[[:space:]]")

# media name in Naver news
webpage |>
  rvest::html_nodes("span.media_end_head_top_logo_text.light_type") |>
  rvest::html_text()

# 입력일자 in Naver news
webpage |>
  rvest::html_nodes("span.media_end_head_info_datestamp_time._ARTICLE_DATE_TIME") |>
  rvest::html_attr("data-date-time")

# 수정일자 in Naver news
webpage |>
  rvest::html_nodes("span.media_end_head_info_datestamp_time._ARTICLE_MODIFY_DATE_TIME") |>
  rvest::html_attr("data-modify-date-time")

# 인쇄하기
pr_url <- webpage |>
  rvest::html_nodes(".media_end_print_link") |>
  rvest::html_attr("data-print-url")
glue::glue("https://n.news.naver.com/{pr_url}")

pagedown::chrome_print(input = glue::glue("https://n.news.naver.com/{pr_url}"),
                       output = "naver_news.pdf", options = list(landscape = FALSE))
# qpdf::pdf_compress(input = "naver_news.pdf")


# image URL
webpage |>
  rvest::html_element(css = "#img2") |>
  rvest::html_attr("data-src")

# image alt text
webpage |>
  rvest::html_element(css = "#img2") |>
  rvest::html_attr("alt")





