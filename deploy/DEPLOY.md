# Deploying SPIRIT on a cluster

The whole SPIRIT stack — R + all R packages, Python + matplotlib/numpy, and the
bioinformatics CLI tools (IntaRNA, RNAfold, bedtools, seqtk, esl-shuffle,
py_fasta_validator) — lives in **one conda environment**. There are no `pip` or
`install.packages()` side-loads, so reproducing that conda env on the cluster
gives you a complete, working install. This was verified from the local `PINTS`
env on 2026-05-26 (linux-64).

## 1. Recreate the environment

Pick one of the three specs in this folder. All three resolve only from
conda-forge/bioconda/r (`nodefaults`) — none touch the Anaconda commercial
`defaults` channel, so they work on clusters that block/forbid it.

| File | Use when | Command |
|------|----------|---------|
| `environment.linux-64.lock` | **Recommended for a linux-64 cluster.** Byte-exact replica of the tested dev env (same R/Python package versions you ran locally), fast, no solve. | `conda create -n spirit --file environment.linux-64.lock` |
| `environment.yml` | Non-linux-64, or you're fine with slightly newer R packages. Curated + readable; verified to solve (381 pkgs). CLI tools + R/Python pinned exactly; R-package patch versions float. | `conda env create -f environment.yml` |
| `environment.full.yml` | You want every transitive dependency pinned too, but still cross-platform (no build strings). | `conda env create -f environment.full.yml` |

> Note: `environment.yml` intentionally lets R-package versions float, so the
> solver pulls current conda-forge builds (e.g. shiny 1.11, DT 0.34) rather than
> the exact ones tested locally (shiny 1.9.1, DT 0.30). For the *exact* tested
> versions, use the lockfile.

```bash
# on the cluster (mamba is much faster than conda if available)
conda env create -f environment.yml      # or: mamba env create -f environment.yml
conda activate spirit
```

Verify the tools resolved:

```bash
for t in IntaRNA RNAfold bedtools seqtk esl-shuffle py_fasta_validator Rscript python; do
  printf "%-20s " "$t"; command -v "$t" || echo MISSING
done
Rscript -e 'invisible(lapply(c("shiny","tidyverse","rtracklayer","plotly","DT","evd"), library, character.only=TRUE)); cat("R packages OK\n")'
```

## 2. Run the pipeline (batch)

```bash
conda activate spirit
sh SPIRIT.sh -f <genome.fasta> -g <genome.gff> -s <srna.fasta> -a <experiment.csv>
```

## 3. Serve the Shiny app online

The app is a standard Shiny app (`scripts/app_spirit.R`). Options, simplest first:

**a) Run it on a port and reach it over SSH / the cluster network**

```bash
conda activate spirit
Rscript -e 'shiny::runApp("scripts/app_spirit.R", host="0.0.0.0", port=3838)'
```
Then from your laptop tunnel in:
`ssh -L 3838:<compute-node>:3838 user@cluster` and open http://localhost:3838.
Good for testing; the process dies when you log out (use `tmux`/`nohup` to keep it up).

**b) Shiny Server (persistent, multi-user)** — if the cluster admins can install it,
point its `site_dir`/app dir at this repo and have it launch R from the `spirit`
env (set `exec` or `PATH` so it uses `~/conda/envs/spirit/bin/R`). This is the
"real" online deployment.

**c) Reverse proxy** — put nginx/Apache in front of option (a) or (b) for a clean
URL and TLS. The app already binds cleanly to a fixed port.

### Notes for cluster execution
- The pipeline shells out to the CLI tools by name, so they must be on `PATH` —
  i.e. run everything with the `spirit` env **activated** (or hard-code the env's
  `bin/` on PATH in your launcher script / Shiny Server config).
- Pre-rendered sRNA structure PNGs are committed under
  `scripts/www/structures/`, so the app shows structures without RNAfold at
  request time; RNAfold is only needed for on-demand SVG/PDF export.
- The app writes timestamped run folders under `data/` — make sure that path is
  writable on the cluster (or point `-o` / the app's output root elsewhere).
