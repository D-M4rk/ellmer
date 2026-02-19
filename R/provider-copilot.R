#' @include provider.R
#' @include provider-openai-compatible.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with a model via GitHub Copilot
#'
#' @description
#' Uses the GitHub Copilot endpoint (`api.githubcopilot.com`) which provides
#' access to a wide range of models including Claude, GPT, and Gemini with
#' large context windows. Requires an active GitHub Copilot subscription.
#'
#' Authentication is handled automatically using credentials stored by any
#' Copilot-enabled editor (VS Code, Positron, JetBrains, Neovim, etc.).
#'
#' Note that `api.githubcopilot.com` is an undocumented endpoint — it may
#' change without notice.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param model The model to use. Run [models_copilot()] to see available
#'   models on your account. Defaults to `"claude-sonnet-4.5"`.
#' @param credentials A zero-argument function returning authentication
#'   headers, or `NULL` to use ambient Copilot credentials from your editor.
#' @param params Common model parameters, usually created by [params()].
#' @param api_args Named list of arbitrary extra arguments appended to the body
#'   of every chat API call.
#' @param api_headers Named character vector of arbitrary extra headers appended
#'   to every chat API call.
#' @param echo One of the following options:
#'   * `none`: don't emit any output (default when running in a function).
#'   * `output`: echo text and tool-calling output as it streams in (default
#'     when running at the console).
#'   * `all`: echo all input and output.
#'
#'   Note this only affects the `chat()` method.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
#' @examples
#' \dontrun{
#' chat <- chat_copilot()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_copilot <- function(
  system_prompt = NULL,
  model = NULL,
  credentials = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  model <- set_default(model, "claude-sonnet-4.5")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_copilot",
    copilot_credentials(),
    credentials = credentials
  )

  chat_openai_compatible(
    name = "GitHub Copilot",
    system_prompt = system_prompt,
    base_url = "https://api.githubcopilot.com",
    credentials = credentials,
    model = model,
    params = params,
    api_args = api_args,
    echo = echo,
    api_headers = c(
      `Copilot-Integration-Id` = "vscode-chat",
      `Editor-Version` = "vscode/1.96.0",
      api_headers
    )
  )
}

#' @rdname chat_copilot
#' @export
models_copilot <- function(credentials = NULL) {
  credentials <- as_credentials(
    "models_copilot",
    copilot_credentials(),
    credentials = credentials
  )

  provider <- ProviderOpenAICompatible(
    name = "GitHub Copilot",
    model = "",
    base_url = "https://api.githubcopilot.com",
    credentials = credentials,
    extra_headers = c(
      `Copilot-Integration-Id` = "vscode-chat",
      `Editor-Version` = "vscode/1.96.0"
    )
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  data.frame(id = sort(id))
}

# Credentials ------------------------------------------------------------------

copilot_credentials <- function() {
  cache <- new.env(parent = emptyenv())

  function() {
    now <- as.numeric(Sys.time())
    if (!is.null(cache$token) && now < cache$expires_at - 60) {
      return(list(Authorization = paste("Bearer", cache$token)))
    }

    oauth_token <- copilot_oauth_token()

    res <- httr2::request("https://api.github.com/copilot_internal/v2/token") |>
      httr2::req_headers(
        Authorization = paste("token", oauth_token),
        `Editor-Version` = "vscode/1.96.0"
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    cache$token <- res$token
    cache$expires_at <- res$expires_at

    list(Authorization = paste("Bearer", cache$token))
  }
}

copilot_oauth_token <- function() {
  path <- copilot_apps_path()

  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Could not find GitHub Copilot credentials.",
      "i" = "Sign into GitHub Copilot in VS Code, Positron, or another supported editor.",
      "i" = "Expected credentials at {.path {path}}."
    ))
  }

  apps <- jsonlite::fromJSON(path)
  token <- apps[[1]]$oauth_token

  if (is.null(token) || !nzchar(token)) {
    cli::cli_abort(c(
      "GitHub Copilot credentials found but no OAuth token present.",
      "i" = "Try signing out and back into GitHub Copilot in your editor."
    ))
  }

  token
}

copilot_apps_path <- function() {
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("LOCALAPPDATA"), "github-copilot", "apps.json")
  } else {
    file.path("~", ".config", "github-copilot", "apps.json")
  }
}
