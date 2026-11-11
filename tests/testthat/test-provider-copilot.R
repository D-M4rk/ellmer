# Live tests need a Copilot sign-in on this machine and make real requests
skip_if_no_copilot <- function() {
  skip_on_ci()
  skip_on_cran()
  if (!file.exists(copilot_apps_path())) {
    testthat::skip("No GitHub Copilot credentials found")
  }
}

write_apps <- function(apps, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  jsonlite::write_json(apps, path, auto_unbox = TRUE)
  path
}

# Stored sign-in ---------------------------------------------------------------

test_that("copilot_apps_path() uses LOCALAPPDATA on windows", {
  skip_if(.Platform$OS.type != "windows")
  withr::local_envvar(LOCALAPPDATA = "C:/fake")
  expect_equal(copilot_apps_path(), "C:/fake/github-copilot/apps.json")
})

test_that("copilot_apps_path() respects XDG_CONFIG_HOME on unix", {
  skip_if(.Platform$OS.type == "windows")
  withr::local_envvar(XDG_CONFIG_HOME = "/fake")
  expect_equal(copilot_apps_path(), "/fake/github-copilot/apps.json")

  withr::local_envvar(XDG_CONFIG_HOME = NA)
  expect_equal(copilot_apps_path(), "~/.config/github-copilot/apps.json")
})

test_that("copilot_oauth_token() errors if apps.json not found", {
  path <- file.path(withr::local_tempdir(), "apps.json")
  expect_error(
    copilot_oauth_token(path),
    "Could not find GitHub Copilot credentials"
  )
})

test_that("copilot_oauth_token() errors if no token is stored", {
  path <- write_apps(list("github.com:abc" = list(user = "me")))
  expect_error(copilot_oauth_token(path), "no OAuth token present")
})

test_that("copilot_oauth_token() reads a single entry", {
  path <- write_apps(list(
    "github.com:abc" = list(user = "me", oauth_token = "tok-1")
  ))
  expect_equal(copilot_oauth_token(path), "tok-1")
})

test_that("copilot_oauth_token() prefers github.com over other hosts", {
  path <- write_apps(list(
    "ghe.example.com:abc" = list(user = "me", oauth_token = "tok-ghe"),
    "github.com:abc" = list(user = "me", oauth_token = "tok-github")
  ))
  expect_equal(copilot_oauth_token(path), "tok-github")
})

# Session ----------------------------------------------------------------------

test_that("copilot_session() caches the token and reports the account host", {
  calls <- 0
  local_mocked_bindings(
    copilot_oauth_token = function(...) "oauth",
    copilot_token_exchange = function(oauth_token) {
      calls <<- calls + 1
      list(
        token = "session",
        expires_at = as.numeric(Sys.time()) + 3600,
        endpoints = list(api = "https://api.business.example.com")
      )
    }
  )

  session <- copilot_session()
  expect_equal(session$credentials(), list(Authorization = "Bearer session"))
  expect_equal(session$base_url(), "https://api.business.example.com")
  expect_equal(calls, 1)
})

test_that("copilot_session() refreshes an expired token", {
  calls <- 0
  local_mocked_bindings(
    copilot_oauth_token = function(...) "oauth",
    copilot_token_exchange = function(oauth_token) {
      calls <<- calls + 1
      list(token = paste0("session-", calls), expires_at = 0)
    }
  )

  session <- copilot_session()
  expect_equal(session$credentials()$Authorization, "Bearer session-1")
  expect_equal(session$credentials()$Authorization, "Bearer session-2")
  expect_equal(session$base_url(), copilot_default_base_url())
})

# chat_copilot() ---------------------------------------------------------------

local_copilot_session <- function(env = parent.frame()) {
  local_mocked_bindings(
    copilot_oauth_token = function(...) "oauth",
    copilot_token_exchange = function(oauth_token) {
      list(
        token = "session",
        expires_at = as.numeric(Sys.time()) + 3600,
        endpoints = list(api = "https://api.business.example.com")
      )
    },
    .env = env
  )
}

test_that("chat_copilot() sends Claude models to the Anthropic-format endpoint", {
  local_copilot_session()

  chat <- chat_copilot()
  provider <- chat$get_provider()
  expect_equal(chat$get_model(), "claude-sonnet-5.5")
  expect_true(S7_inherits(provider, ProviderCopilotAnthropic))
  expect_true(S7_inherits(provider, ProviderAnthropic))
  expect_equal(provider@base_url, "https://api.business.example.com/v1")
  expect_equal(
    provider@extra_headers[["Copilot-Integration-Id"]],
    "vscode-chat"
  )
  expect_contains(names(provider@extra_headers), "Editor-Version")
})

test_that("chat_copilot() sends other models to the OpenAI-format endpoint", {
  local_copilot_session()

  chat <- chat_copilot(model = "gpt-4.1")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderCopilotOpenAI))
  expect_true(S7_inherits(provider, ProviderOpenAICompatible))
  expect_equal(provider@base_url, "https://api.business.example.com")
  expect_equal(
    provider@extra_headers[["Copilot-Integration-Id"]],
    "vscode-chat"
  )
  expect_false(provider@strict)
})

test_that("chat_copilot() switches endpoint when the model family changes", {
  local_copilot_session()

  chat <- chat_copilot(model = "claude-sonnet-5.5", api_headers = c(x = "y"))
  chat$set_model("gpt-4.1")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderCopilotOpenAI))
  expect_equal(provider@base_url, "https://api.business.example.com")
  expect_equal(provider@extra_headers[["x"]], "y")

  chat$set_model("claude-haiku-5.5")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderCopilotAnthropic))
  expect_equal(provider@base_url, "https://api.business.example.com/v1")
  expect_equal(provider@extra_headers[["x"]], "y")
})

test_that("chat_copilot() keeps the provider when the model family is unchanged", {
  local_copilot_session()

  chat <- chat_copilot(model = "claude-sonnet-5.5")
  chat$set_model("claude-opus-5.5")
  provider <- chat$get_provider()
  expect_equal(chat$get_model(), "claude-opus-5.5")
  expect_true(S7_inherits(provider, ProviderCopilotAnthropic))
  expect_equal(provider@base_url, "https://api.business.example.com/v1")
})

test_that("chat_copilot() falls back to the generic host for custom credentials", {
  chat <- chat_copilot(
    model = "gpt-4.1",
    credentials = function() list(Authorization = "Bearer custom")
  )
  expect_equal(chat$get_provider()@base_url, copilot_default_base_url())
})

test_that("copilot_host() strips the Anthropic path", {
  expect_equal(copilot_host("https://x.com/v1"), "https://x.com")
  expect_equal(copilot_host("https://x.com/v1/"), "https://x.com")
  expect_equal(copilot_host("https://x.com/"), "https://x.com")
  expect_equal(copilot_host("https://x.com"), "https://x.com")
})

# Live -------------------------------------------------------------------------

test_that("can make simple request", {
  skip_if_no_copilot()
  chat <- chat_copilot("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
})

test_that("can make simple request with a non-Claude model", {
  skip_if_no_copilot()
  chat <- chat_copilot(
    "Be as terse as possible; no punctuation",
    model = "gpt-4.1"
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
})

test_that("Claude models don't re-answer earlier turns after a tool call", {
  skip_if_no_copilot()
  chat <- chat_copilot("Be as terse as possible")
  chat$register_tool(tool(
    function(a, b) a + b,
    name = "add",
    description = "Add two numbers.",
    arguments = list(a = type_number("first"), b = type_number("second"))
  ))
  chat$chat("What is the capital of France? One word.", echo = FALSE)
  resp <- chat$chat("Use the add tool to compute 2 + 3.", echo = FALSE)
  expect_match(resp, "5")
  expect_no_match(resp, "Paris")
})

test_that("can list models", {
  skip_if_no_copilot()
  test_models(models_copilot)
})
