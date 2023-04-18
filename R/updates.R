#' Check for rswap updates
#'
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>%
#' @importFrom crayon red bold yellow
check_rswap_updates <- function() {
  url <- "https://api.github.com/repos/moritzshore/rswap/releases/latest"
  rswap_data <-
    jsonlite::fromJSON(url)

  newest_version <- rswap_data$name

  current_version <-
    packageVersion("rswap") %>% as.character() %>% enc2utf8()

  if ((newest_version == current_version)) {
    cat(
      crayon::yellow(underline("rswap")),
      crayon::yellow("version"),
      crayon::yellow(crayon::underline(current_version)),
      crayon::green(crayon::bold("[up to date]")),
      "\n"
    )
  } else{
    cat(
      crayon::yellow(underline("rswap")),
      crayon::yellow("version"),
      crayon::yellow(crayon::underline(current_version)),
      crayon::green(crayon::red(crayon::bold("[out of date]"))),
      "\n"
    )
    cat(
      crayon::red("newest version"),
      underline(newest_version),
      (crayon::red(
        "please update using following command:"
      )),
      crayon::blue("devtools::install_github('moritzshore/rswap')"),
      "\n"
    )
  }
}
