# Entry point for shiny-server (shiny-server auto-runs a file named app.R).
#
# shiny-server sets the working directory to the folder that holds this app.R,
# so this file lives at the REPO ROOT — that makes cwd = repo root, which is what
# the app's relative paths (./SPIRIT.sh, ./data/default, scripts/…) expect.
#
# Sourcing app_spirit.R defines `ui` and `server`. We then build the app object
# WITHOUT a fixed host/port, so shiny-server assigns the port itself (a hardcoded
# port would break its proxy). Manual runs still use scripts/app_spirit.R, which
# keeps its own SPIRIT_PORT launcher.

# Safety check: we must be at the repo root (where SPIRIT.sh lives).
if (!file.exists("SPIRIT.sh")) {
  stop("app.R must run from the repo root (SPIRIT.sh not found in working dir: ",
       getwd(), "). Point shiny-server's app dir at the repo root.")
}

source("scripts/app_spirit.R", local = FALSE)

shinyApp(ui = ui, server = server)
