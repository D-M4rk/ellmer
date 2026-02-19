# Test multiple models via GitHub Copilot endpoint
# Auth flow validated in COPILOT_ENDPOINT_FINDINGS.md

library(httr2)
library(jsonlite)

# ── Step 1: Auth ──────────────────────────────────────────────────────────────

apps_path <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "github-copilot",
  "apps.json"
)
apps <- jsonlite::fromJSON(apps_path)
oauth_token <- apps[[1]]$oauth_token
cat("OAuth token found:", substr(oauth_token, 1, 10), "...\n\n")

res <- request("https://api.github.com/copilot_internal/v2/token") |>
  req_headers(
    Authorization = paste("token", oauth_token),
    `Editor-Version` = "vscode/1.96.0"
  ) |>
  req_perform() |>
  resp_body_json()

session_token <- res$token
cat("Session token acquired, expires:", res$expires_at, "\n\n")

add_copilot_headers <- function(req, session_token) {
  req_headers(
    req,
    Authorization = paste("Bearer", session_token),
    `Copilot-Integration-Id` = "vscode-chat",
    `Editor-Version` = "vscode/1.96.0"
  )
}

# ── Step 2: List available models ─────────────────────────────────────────────

cat("── Available models on this account ──\n")
models_res <- tryCatch(
  {
    request("https://api.githubcopilot.com/models") |>
      add_copilot_headers(session_token) |>
      req_perform() |>
      resp_body_json()
  },
  error = function(e) {
    cat("Could not fetch model list:", conditionMessage(e), "\n")
    NULL
  }
)

available_ids <- character(0)
if (!is.null(models_res)) {
  available_ids <- vapply(models_res$data, `[[`, character(1), "id")
  cat(paste("-", sort(available_ids), collapse = "\n"), "\n\n")
}

# ── Step 3: Test specific models ──────────────────────────────────────────────

models_to_test <- c(
  "gpt-4o",
  "gpt-4.1",
  "claude-haiku-4.5",
  "claude-sonnet-4",
  "claude-sonnet-4.5",
  "gemini-2.5-pro",
  "gpt-4o-mini",
  "gpt-5-mini",
  "gpt-5.1"
)

prompt <- "Reply with only 5 words confirming you work."

cat("── Model tests ──\n")
results <- list()

for (model in models_to_test) {
  in_list <- if (length(available_ids) > 0) model %in% available_ids else NA
  status_note <- if (isFALSE(in_list)) " [not in model list]" else ""
  cat(sprintf("Testing %-25s%s... ", model, status_note))

  result <- tryCatch(
    {
      resp <- request("https://api.githubcopilot.com/chat/completions") |>
        add_copilot_headers(session_token) |>
        req_body_json(list(
          model = model,
          messages = list(list(role = "user", content = prompt))
        )) |>
        req_perform()

      body <- resp_body_json(resp)
      reply <- body$choices[[1]]$message$content
      usage <- body$usage
      cat(sprintf(
        "OK | tokens: %d in / %d out\n",
        usage$prompt_tokens,
        usage$completion_tokens
      ))
      list(
        model = model,
        status = "ok",
        reply = reply,
        usage = usage,
        in_list = in_list
      )
    },
    error = function(e) {
      # Try to get the actual API error message from the response body
      msg <- tryCatch(
        {
          body <- resp_body_json(httr2:::last_response())
          body$error$message %||% conditionMessage(e)
        },
        error = function(...) conditionMessage(e)
      )
      cat("FAILED |", msg, "\n")
      list(model = model, status = "failed", reply = msg, in_list = in_list)
    }
  )

  results[[model]] <- result
}

# ── Step 4: Token limit probe ─────────────────────────────────────────────────

cat("\n── Token limit probe (gpt-4o) ──\n")
cat("Sending ~10k token prompt to check context handling... ")

big_prompt <- paste(
  rep("The quick brown fox jumps over the lazy dog.", 5000),
  collapse = " "
)
big_prompt <- paste(big_prompt, "Now reply with only the word: DONE")

tryCatch(
  {
    resp <- request("https://api.githubcopilot.com/chat/completions") |>
      add_copilot_headers(session_token) |>
      req_body_json(list(
        model = "gpt-4o",
        messages = list(list(role = "user", content = big_prompt))
      )) |>
      req_perform() |>
      resp_body_json()

    usage <- resp$usage
    cat(sprintf(
      "OK | tokens: %d in / %d out (total: %d)\n",
      usage$prompt_tokens,
      usage$completion_tokens,
      usage$total_tokens
    ))
  },
  error = function(e) {
    cat("FAILED |", conditionMessage(e), "\n")
  }
)

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n── Summary ──\n")
cat(sprintf("%-28s %-12s %s\n", "Model", "Status", "In account list"))
cat(strrep("-", 55), "\n")
for (r in results) {
  in_list_str <- if (is.na(r$in_list)) {
    "unknown"
  } else if (r$in_list) {
    "yes"
  } else {
    "no"
  }
  cat(sprintf("%-28s %-12s %s\n", r$model, r$status, in_list_str))
}
