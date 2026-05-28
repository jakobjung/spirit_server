# Entry point for shiny-server (it auto-runs a file named app.R).
#
# Robust to placement: works whether this app.R sits at the repo root OR inside
# scripts/. shiny-server sets the working directory to the app's folder, which
# may be either, so we locate the repo root ourselves (the dir that contains
# both SPIRIT.sh and scripts/app_spirit.R).
#
# We then:
#   - tell the app where its static files live (SPIRIT_APP_DIR -> scripts/, holds www/)
#   - set the working directory to the repo root (so ./SPIRIT.sh, ./data/* resolve)
#   - build the app WITHOUT a fixed port, so shiny-server controls the port.

find_repo_root <- function() {
  for (cand in c(".", "..")) {
    if (file.exists(file.path(cand, "SPIRIT.sh")) &&
        file.exists(file.path(cand, "scripts", "app_spirit.R"))) {
      return(normalizePath(cand))
    }
  }
  stop("app.R: cannot locate repo root (need SPIRIT.sh + scripts/app_spirit.R) ",
       "from working dir: ", getwd())
}

repo_root <- find_repo_root()
Sys.setenv(SPIRIT_APP_DIR = file.path(repo_root, "scripts"))
setwd(repo_root)

source(file.path("scripts", "app_spirit.R"), local = FALSE)

shinyApp(ui = ui, server = server)
