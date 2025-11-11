#!/usr/bin/env bash
# SPIRIT main pipeline

# --- Safer shell settings ---
set -Eeuo pipefail
IFS=$'\n\t'

# --- (Optional) conda activation with guard ---
if command -v conda >/dev/null 2>&1; then
  # shellcheck disable=SC1091
  source "$(conda info --base)/etc/profile.d/conda.sh" || true
  conda activate spirit || true
else
  echo "Note: conda not found in PATH; assuming required tools are available."
fi

################################################################################
# Help                                                                         #
################################################################################
Help()
{
  echo "SPIRIT: Simple P-value Integration of Regulatory Interaction Targets"
  echo
  echo "Usage:  sh SPIRIT.sh -f <fasta_file> -g <gff_file> -s <sRNA_fasta> -a <csv1> [options]"
  echo "        Results will be saved in a timestamped folder (default root: './data')."
  echo
  echo "Required:"
  echo "  -f  FASTA file of target organism"
  echo "  -g  GFF file of target organism"
  echo "  -s  sRNA FASTA file"
  echo "  -a  CSV with '<id>, p_value' columns (default id: 'locus_tag' or set via -i)"
  echo
  echo "Optional:"
  echo "  -b  CSV/XLSX (same format)"
  echo "  -c  CSV/XLSX (same format)"
  echo "  -d  CSV/XLSX (same format)"
  echo "  -w  Weights for Fisher's test, comma-separated"
  echo "  -i  Gene identifier column (default: 'locus_tag')"
  echo "  -o  Output root directory (default: './data')"
  echo
  echo "Other:"
  echo "  -h  Print help"
  echo "  -V  Print version"
  echo
  echo "Version: v1.0.0"
  echo "Docs & Code: https://github.com/jakobjung/SPIRIT"
  echo "Mail:        jakobjung@tutanota.com"
  echo "Authors:      Hoda Kooshapour, Jakob J. Jung"
}

version="v1.0.0"

# --- Resolve script directory for robust relative paths ---
SCRIPT_DIR="$(cd -- "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

################################################################################
# Process Flags                                                                #
################################################################################
while getopts f:g:s:a:b:c:d:w:i:o:hV flag
do
  case "${flag}" in
    f) fasta=${OPTARG};;
    g) gff=${OPTARG};;
    s) srna=${OPTARG};;
    a) file1=${OPTARG};;
    b) file2=${OPTARG};;
    c) file3=${OPTARG};;
    d) file4=${OPTARG};;
    w) weights=${OPTARG};;
    i) id=${OPTARG};;
    o) output_dir=${OPTARG};;
    h) Help; exit;;
    V) echo "$version"; exit;;
    *) echo "Invalid option: -$OPTARG" >&2; Help; exit 1;;
  esac
done

# Defaults
id="${id:-locus_tag}"
output_dir="${output_dir:-./data}"

# --- Validate required inputs exist & are readable ---
req_file() { [ -r "$1" ] || { echo "ERROR: cannot read $2: $1"; exit 1; }; }
req_file "${fasta:-}" "FASTA (-f)"
req_file "${gff:-}"   "GFF (-g)"
req_file "${srna:-}"  "sRNA FASTA (-s)"
req_file "${file1:-}" "CSV (-a)"

# --- Prepare output directories (absolute path) ---
output_dir="$(mkdir -p "$output_dir" && cd "$output_dir" && pwd)"
timestamp="$(date +"%Y_%m_%d_T_%H_%M_%S")"
RUN_DIR="${output_dir}/${timestamp}"
mkdir -p "${RUN_DIR}/data" "${RUN_DIR}/results" "${RUN_DIR}/logs"

# --- Logging: everything to console + logfile ---
LOG="${RUN_DIR}/logs/pipeline.log"
exec > >(tee -a "$LOG") 2>&1

echo "Logging to: $LOG"
echo "SPIRIT version: $version"
echo "Run directory:  $RUN_DIR"

# --- Trap errors nicely ---
cleanup() { echo "An error occurred. See log: $LOG"; }
trap cleanup ERR

# --- Tool availability check ---
for tool in py_fasta_validator bedtools IntaRNA Rscript seqtk esl-shuffle
do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "ERROR: '$tool' not found in PATH. Please install or add it to PATH."
    exit 1
  fi
done

echo "Tool versions (where available):"
{ Rscript --version 2>/dev/null || true; } | head -1
{ bedtools --version 2>/dev/null || true; } | head -1
{ IntaRNA --version 2>/dev/null || true; } | head -1
{ seqtk 2>/dev/null || true; } | head -1
{ esl-shuffle -h 2>/dev/null || true; } | head -1

# --- Manifest for reproducibility ---
{
  echo "SPIRIT version: $version"
  echo "Date (UTC): $(date -u +%FT%TZ)"
  echo "FASTA: $fasta"
  echo "GFF: $gff"
  echo "sRNA: $srna"
  echo "CSV files:"
  echo "  - $file1"
  [ -n "${file2:-}" ] && echo "  - $file2"
  [ -n "${file3:-}" ] && echo "  - $file3"
  [ -n "${file4:-}" ] && echo "  - $file4"
  echo "ID column: $id"
  echo "Weights: ${weights:-none}"
  echo "Run dir: $RUN_DIR"
} > "${RUN_DIR}/run_manifest.txt"


# --- check if input files are the default ones. if so, copy them to the run directory ---

# --- Validate FASTA quickly ---
if ! py_fasta_validator -f "$fasta"; then
  echo "ERROR: Invalid FASTA file '$fasta'"
  exit 1
fi

# --- Collect CSVs in an array & validate with R helper ---
csvs=("$file1")
[ -n "${file2:-}" ] && csvs+=("$file2")
[ -n "${file3:-}" ] && csvs+=("$file3")
[ -n "${file4:-}" ] && csvs+=("$file4")

for file in "${csvs[@]}"; do
  Rscript "${SCRIPT_DIR}/scripts/check_csv.R" "$RUN_DIR" "$file" "$id"
done

# --- Combine CSVs (inner-join on <id>) ---
Rscript "${SCRIPT_DIR}/scripts/combine_tables.R" "$RUN_DIR" "$id" "${csvs[@]}"

# --- Process GFF to BED ---
Rscript "${SCRIPT_DIR}/scripts/read_gff_file.R" "$RUN_DIR" "$gff" "$id"

# --- Prepare IntaRNA target FASTA with bedtools ---
echo "Running bedtools getfasta on ${RUN_DIR}/data/combined_tables.bed ..."
bedtools getfasta -fi "$fasta" \
  -bed "${RUN_DIR}/data/combined_tables.bed" \
  -fo "${RUN_DIR}/data/IntaRNA_file.fasta" \
  -s -name+ \
  || { echo "bedtools getfasta failed. Check BED and FASTA."; exit 1; }

# --- IntaRNA on real sequences ---
echo "Running IntaRNA (real targets) ..."
IntaRNA -q "$srna" -t "${RUN_DIR}/data/IntaRNA_file.fasta" \
  --out "${RUN_DIR}/data/IntaRNA_output.csv" --outMode C
echo "IntaRNA (real) done."

# --- Shuffle: use record count, not line count ---
records=$(grep -c '^>' "${RUN_DIR}/data/IntaRNA_file.fasta" || true)
echo "Detected $records FASTA records for shuffling."

if   [ "$records" -gt 2000 ]; then N=1
elif [ "$records" -gt 1000 ]; then N=2
elif [ "$records" -gt 500  ]; then N=4
elif [ "$records" -gt 100  ]; then N=20
else
  echo "Less than 100 targets (<100 records). Exiting."
  exit 1
fi

echo "Using esl-shuffle -N ${N} ..."
esl-shuffle -1 ${N:+-N "$N"} \
  -o "${RUN_DIR}/data/IntaRNA_file_shuffled.fasta" \
  "${RUN_DIR}/data/IntaRNA_file.fasta"

# --- Subsample shuffled to 1000 entries (headers-based) ---
seqtk sample -s100 "${RUN_DIR}/data/IntaRNA_file_shuffled.fasta" 1000 \
  > "${RUN_DIR}/data/IntaRNA_file_shuffled_1000.fasta"
rm -f "${RUN_DIR}/data/IntaRNA_file_shuffled.fasta"
echo "Shuffle + subsample done."

# --- IntaRNA on shuffled sequences ---
echo "Running IntaRNA (shuffled) ..."
IntaRNA -q "$srna" -t "${RUN_DIR}/data/IntaRNA_file_shuffled_1000.fasta" \
  --out "${RUN_DIR}/data/IntaRNA_shuffled_output.csv" --outMode C
rm -f "${RUN_DIR}/data/IntaRNA_file_shuffled_1000.fasta"
echo "IntaRNA (shuffled) done."

# --- Compute IntaRNA p-values & integrate ---
Rscript "${SCRIPT_DIR}/scripts/get_pvalues_from_intarna.R" "$RUN_DIR" "$id"

# Combine P-values (Fisher, FDR)
Rscript "${SCRIPT_DIR}/scripts/p_value_integration.R" "$RUN_DIR" "$id" "${weights:-}"

# --- Convenience symlink to latest ---
ln -sfn "$RUN_DIR" "${output_dir}/latest"
echo "Latest run symlink -> ${output_dir}/latest"

echo "SPIRIT pipeline finished successfully."
echo "Results: $RUN_DIR"
echo "Log:     $LOG"
