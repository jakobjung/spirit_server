#!/bin/bash
# Pre-compute RNA secondary-structure PNGs for every sRNA in the
# K12 / SL1344 / B. theta multi-fastas. Uses RNAfold -p to get the MFE
# layout and partition-function pair probabilities, then renders each
# structure with matplotlib (viridis colormap, colored by per-base
# pair probability) via scripts/render_srna_structure.py.
#
# Output: scripts/www/structures/<organism>/<sanitized_name>.png
# Run once after regenerating any of the sRNA multi-fastas.

set -Eeuo pipefail

PROJ_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DEFAULT_DIR="$PROJ_DIR/data/default"
OUT_BASE="$PROJ_DIR/scripts/www/structures"
RENDERER="$PROJ_DIR/scripts/render_srna_structure.py"

gen_one_organism() {
  local fasta="$1" organism="$2"
  local out_dir="$OUT_BASE/$organism"
  mkdir -p "$out_dir"
  local work
  work="$(mktemp -d)"

  awk -v out="$work" '
    /^>/ {
      name = substr($1, 2);
      gsub(/[^A-Za-z0-9_-]/, "_", name);
      fname = out "/" name ".fa";
      # use sanitized header so RNAfold writes <name>_ss.ps / <name>_dp.ps
      print ">" name > fname;
      next;
    }
    { print > fname }
  ' "$fasta"

  (
    cd "$work"
    set +e
    local failed=()
    for fa in *.fa; do
      local name="${fa%.fa}"
      if ! RNAfold -p < "$fa" > /dev/null 2> "rnafold_err_$name.log"; then
        failed+=("RNAfold:$name"); continue
      fi
      if ! python3 "$RENDERER" "${name}_ss.ps" "${name}_dp.ps" \
           "$out_dir/$name.png" 2> "render_err_$name.log"; then
        failed+=("render:$name")
        continue
      fi
      rm -f "rnafold_err_$name.log" "render_err_$name.log"
    done
    # Surface any per-file error logs that survived
    if compgen -G "*_err_*.log" > /dev/null; then
      echo "--- error logs ($organism) ---"
      for log in *_err_*.log; do
        echo ">>> $log"; cat "$log"
      done
    fi
    if [ "${#failed[@]}" -gt 0 ]; then
      printf '[%s] FAIL: %s\n' "$organism" "${failed[*]}"
    fi
  )

  rm -rf "$work"
  echo "[$organism] generated $(find "$out_dir" -maxdepth 1 -name '*.png' | wc -l) structures"
}

gen_one_organism "$DEFAULT_DIR/sRNAs_K12.fasta"    k12
gen_one_organism "$DEFAULT_DIR/sRNAs_SL1344.fasta" sl1344
gen_one_organism "$DEFAULT_DIR/sRNAs_Btheta.fasta" btheta

echo "Total PNGs: $(find "$OUT_BASE" -name '*.png' | wc -l)"
