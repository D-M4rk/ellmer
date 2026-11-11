#' @include provider-openai-compatible.R
#' @include provider-claude.R
NULL

#' Chat with a model via GitHub Copilot
#'
#' @description
#' Uses the endpoint that GitHub Copilot editors talk to, which provides access
#' to a wide range of models including Claude, GPT, and Gemini. Requires an
#' active GitHub Copilot subscription.
#'
#' Authentication reuses the sign-in stored by a Copilot-enabled editor in
#' `github-copilot/apps.json`. The API host is taken from your account, so
#' Copilot Business and Enterprise seats are routed to their own endpoint.
#'
#' Note that this endpoint is undocumented and may change without notice.
#'
#' Claude models are sent to Copilot's Anthropic-format endpoint, in the same
#' way as [chat_anthropic()], because Copilot's OpenAI-format endpoint garbles
#' their conversation history after tool calls. All other models use the
#' OpenAI-format endpoint, in the same way as [chat_openai_compatible()].
#' Switching model with `$set_model()` switches endpoint as needed.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param base_url The base URL to the endpoint. The default, `NULL`, uses the
#'   endpoint that GitHub reports for your account.
#' @param model The model to use. Run [models_copilot()] to see the models
#'   available to your account. Defaults to `"claude-sonnet-5.5"`.
#' @param credentials A zero-argument function returning authentication
#'   headers, or `NULL` to use the Copilot sign-in stored by your editor.
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
  base_url = NULL,
  model = NULL,
  credentials = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  model <- set_default(model, "claude-sonnet-5.5")
  echo <- check_echo(echo)

  session <- copilot_session()
  base_url <- base_url %||% copilot_base_url(session, credentials)
  credentials <- as_credentials(
    "chat_copilot",
    session$credentials,
    credentials = credentials
  )

  provider <- copilot_provider(
    model = model,
    base_url = base_url,
    extra_headers = c(copilot_headers(), api_headers),
    credentials = credentials
  )
  model_obj <- Model(
    name = model,
    params = params %||% params(),
    extra_args = api_args
  )
  Chat$new(
    provider = provider,
    model = model_obj,
    system_prompt = system_prompt,
    echo = echo
  )
}

# Providers --------------------------------------------------------------------

# Copilot serves Claude models in both wire formats, but its OpenAI-format
# endpoint mangles their history once it contains tool results, so Claude
# models use the Anthropic-format endpoint instead.
ProviderCopilotAnthropic <- new_class(
  "ProviderCopilotAnthropic",
  parent = ProviderAnthropic
)

ProviderCopilotOpenAI <- new_class(
  "ProviderCopilotOpenAI",
  parent = ProviderOpenAICompatible
)

copilot_provider <- function(
  model,
  base_url,
  extra_headers,
  credentials,
  name = "GitHub Copilot"
) {
  host <- copilot_host(base_url)

  if (is_claude_model(model)) {
    ProviderCopilotAnthropic(
      name = name,
      base_url = paste0(host, "/v1"),
      extra_headers = extra_headers,
      credentials = credentials,
      cache = "5m"
    )
  } else {
    ProviderCopilotOpenAI(
      name = name,
      base_url = host,
      extra_headers = extra_headers,
      credentials = credentials
    )
  }
}

method(provider_set_model, ProviderCopilotAnthropic) <- function(
  provider,
  name
) {
  copilot_provider_switch(provider, name)
}

method(provider_set_model, ProviderCopilotOpenAI) <- function(provider, name) {
  copilot_provider_switch(provider, name)
}

copilot_provider_switch <- function(provider, model) {
  was_claude <- S7_inherits(provider, ProviderCopilotAnthropic)
  if (was_claude == is_claude_model(model)) {
    return(provider)
  }

  copilot_provider(
    model = model,
    base_url = provider@base_url,
    extra_headers = provider@extra_headers,
    credentials = provider@credentials,
    name = provider@name
  )
}

# The Anthropic-format endpoint lives under /v1; the OpenAI-format one does not.
copilot_host <- function(base_url) {
  sub("/v1/?$", "", sub("/$", "", base_url))
}

#' @rdname chat_copilot
#' @export
models_copilot <- function(base_url = NULL, credentials = NULL) {
  session <- copilot_session()
  base_url <- base_url %||% copilot_base_url(session, credentials)
  credentials <- as_credentials(
    "models_copilot",
    session$credentials,
    credentials = credentials
  )

  req <- request(copilot_host(base_url))
  req <- ellmer_req_credentials(req, credentials(), "Authorization")
  req <- req_headers(req, !!!copilot_headers())
  req <- ellmer_req_user_agent(req)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  data.frame(id = sort(unique(id)))
}

# The Copilot endpoint rejects requests that don't identify an editor.
copilot_headers <- function() {
  c(
    `Copilot-Integration-Id` = "vscode-chat",
    `Editor-Version` = copilot_editor_version()
  )
}

copilot_editor_version <- function() {
  "vscode/1.96.0"
}

copilot_default_base_url <- function() {
  "https://api.githubcopilot.com"
}

# Custom credentials can't tell us which host the account uses, so fall back
# to the generic one.
copilot_base_url <- function(session, credentials = NULL) {
  if (is.null(credentials)) {
    session$base_url()
  } else {
    copilot_default_base_url()
  }
}

# Session ----------------------------------------------------------------------

# Exchanges the editor's long-lived OAuth token for a short-lived session
# token, cached until shortly before it expires.
copilot_session <- function() {
  cache <- new.env(parent = emptyenv())

  refresh <- function() {
    now <- as.numeric(Sys.time())
    if (is.null(cache$token) || now >= cache$expires_at - 60) {
      res <- copilot_token_exchange(copilot_oauth_token())
      cache$token <- res$token
      cache$expires_at <- res$expires_at %||% (now + 600)
      cache$base_url <- res$endpoints$api %||% copilot_default_base_url()
    }
    invisible()
  }

  list(
    credentials = function() {
      refresh()
      list(Authorization = paste("Bearer", cache$token))
    },
    base_url = function() {
      refresh()
      cache$base_url
    }
  )
}

copilot_token_exchange <- function(oauth_token) {
  req <- request("https://api.github.com/copilot_internal/v2/token")
  req <- req_headers_redacted(req, Authorization = paste("token", oauth_token))
  req <- req_headers(req, `Editor-Version` = copilot_editor_version())
  req <- ellmer_req_user_agent(req)
  req <- req_error(req, is_error = function(resp) FALSE)
  resp <- req_perform(req)

  status <- resp_status(resp)
  if (status %in% c(401, 403, 404)) {
    cli::cli_abort(c(
      "GitHub rejected the stored Copilot sign-in (HTTP {status}).",
      "i" = "Check that your account has an active GitHub Copilot subscription.",
      "i" = "Try signing out and back into GitHub Copilot in your editor."
    ))
  } else if (status != 200) {
    cli::cli_abort(
      "Failed to get a GitHub Copilot session token (HTTP {status})."
    )
  }

  json <- resp_body_json(resp)
  if (!is_string(json$token) || !nzchar(json$token)) {
    cli::cli_abort("GitHub did not return a Copilot session token.")
  }
  json
}

# Stored sign-in ---------------------------------------------------------------

copilot_oauth_token <- function(path = copilot_apps_path()) {
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Could not find GitHub Copilot credentials.",
      "i" = "Sign into GitHub Copilot in a supported editor.",
      "i" = "Expected credentials at {.path {path}}."
    ))
  }

  apps <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  tokens <- compact(lapply(apps, function(app) {
    token <- app$oauth_token
    if (is_string(token) && nzchar(token)) token
  }))

  if (length(tokens) == 0) {
    cli::cli_abort(c(
      "GitHub Copilot credentials found but no OAuth token present.",
      "i" = "Try signing out and back into GitHub Copilot in your editor."
    ))
  }

  # Entries are keyed by "<host>:<app id>". Prefer github.com over other hosts
  # (e.g. GitHub Enterprise Server), which need a different token endpoint.
  is_github <- grepl("^github\\.com(:|$)", names(tokens))
  if (any(is_github)) {
    tokens <- tokens[is_github]
  }
  tokens[[1]]
}

copilot_apps_path <- function() {
  if (.Platform$OS.type == "windows") {
    config <- Sys.getenv("LOCALAPPDATA")
  } else {
    config <- Sys.getenv("XDG_CONFIG_HOME", file.path("~", ".config"))
  }
  file.path(config, "github-copilot", "apps.json")
}
