# Helpers ---------------------------------------------------------------------

skip_if_no_copilot <- function() {
  path <- copilot_apps_path()
  if (!file.exists(path)) {
    testthat::skip("No GitHub Copilot credentials found")
  }
}

# Tests -----------------------------------------------------------------------

test_that("copilot_apps_path() returns windows path on windows", {
  skip_if(.Platform$OS.type != "windows")
  path <- copilot_apps_path()
  expect_true(grepl("github-copilot", path, fixed = TRUE))
  expect_true(grepl("apps.json", path, fixed = TRUE))
  expect_true(grepl(Sys.getenv("LOCALAPPDATA"), path, fixed = TRUE))
})

test_that("copilot_apps_path() returns unix path on non-windows", {
  skip_if(.Platform$OS.type == "windows")
  path <- copilot_apps_path()
  expect_true(grepl(".config/github-copilot/apps.json", path, fixed = TRUE))
})

test_that("copilot_oauth_token() errors if apps.json not found", {
  withr::with_envvar(
    list(LOCALAPPDATA = withr::local_tempdir()),
    expect_error(copilot_oauth_token(), "Could not find GitHub Copilot credentials")
  )
})

test_that("copilot_credentials() returns a zero-arg function", {
  creds <- copilot_credentials()
  expect_true(is.function(creds))
  expect_equal(length(formals(creds)), 0)
})

test_that("chat_copilot() uses claude-sonnet-4.5 as default model", {
  skip_if_no_copilot()
  chat <- chat_copilot()
  expect_equal(chat$get_provider()@model, "claude-sonnet-4.5")
})

test_that("chat_copilot() sets the correct base_url", {
  skip_if_no_copilot()
  chat <- chat_copilot()
  expect_equal(
    chat$get_provider()@base_url,
    "https://api.githubcopilot.com"
  )
})

test_that("chat_copilot() includes required Copilot headers", {
  skip_if_no_copilot()
  chat <- chat_copilot()
  headers <- chat$get_provider()@extra_headers
  expect_equal(headers[["Copilot-Integration-Id"]], "vscode-chat")
  expect_true("Editor-Version" %in% names(headers))
})
