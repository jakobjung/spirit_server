# SPIRIT

**Simple P-value Integration of Regulatory Interaction Targets**

SPIRIT is a bioinformatics pipeline that predicts sRNA–mRNA regulatory interactions. It integrates [IntaRNA](https://github.com/BackofenLab/IntaRNA) sequence-based interaction predictions with experimental p-values using Fisher's and Stouffer's meta-analysis methods to rank target genes.

## Overview

Given an sRNA of interest and one or more experimental datasets (e.g. RNA-seq, MAPS), SPIRIT:

1. Predicts sRNA–mRNA interactions using IntaRNA
2. Estimates interaction significance by fitting a Gumbel distribution to shuffled-sequence controls
3. Combines IntaRNA p-values with experimental p-values via Fisher's and Stouffer's methods
4. Applies FDR correction and produces ranked target lists with diagnostic plots

## Requirements

### External tools

- [IntaRNA](https://github.com/BackofenLab/IntaRNA) — RNA–RNA interaction prediction
- [bedtools](https://bedtools.readthedocs.io/) — genome arithmetic
- [seqtk](https://github.com/lh3/seqtk) — FASTA/FASTQ toolkit
- [esl-shuffle](http://eddylab.org/infernal/) (from Easel/Infernal) — sequence shuffling
- [py_fasta_validator](https://github.com/linsalrob/PyFastaValidator) — FASTA validation
- [R](https://www.r-project.org/) (≥ 4.0) with `Rscript`

### R packages

**Core:** tidyverse, readxl, writexl, rtracklayer, MASS, evd, cowplot, ggrepel, ggpubr

**Shiny app (optional):** shiny, shinythemes, shinyjs, DT, plotly, promises, future, jsonlite

### Conda (recommended)

If a `spirit` conda environment is available, the pipeline will activate it automatically.

## Usage

```bash
sh SPIRIT.sh -f <genome.fasta> -g <genome.gff> -s <srna.fasta> -a <experiment.csv> [options]
```

### Required arguments

| Flag | Description |
|------|-------------|
| `-f` | FASTA file of the target organism genome |
| `-g` | GFF annotation file |
| `-s` | sRNA FASTA file |
| `-a` | CSV with gene ID and `p_value` columns |

### Optional arguments

| Flag | Description | Default |
|------|-------------|---------|
| `-b`, `-c`, `-d` | Additional CSV/XLSX experiment files | — |
| `-w` | Weights for Fisher's test (comma-separated) | equal |
| `-i` | Gene identifier column name | `locus_tag` |
| `-o` | Output root directory | `./data` |
| `-t` | Number of IntaRNA threads (0 = all) | `0` |
| `-h` | Print help | — |
| `-V` | Print version | — |

### Examples

```bash
# Basic run with one experiment
sh SPIRIT.sh -f genome.fa -g genome.gff -s srna.fa -a rnaseq.csv

# Multiple evidence sources with custom weights and gene ID column
sh SPIRIT.sh -f genome.fa -g genome.gff -s srna.fa \
  -a rnaseq.csv -b maps.csv -w "1,2" -i gene

# Using bundled test data (Salmonella SL1344 + PinT)
sh SPIRIT.sh \
  -f data/default/FQ312003_wplasmids.fa \
  -g data/default/FQ312003.1_srnas_plasmids.gff \
  -s data/default/pinT.fasta \
  -a data/default/spi1_maps_default.csv \
  -b data/default/spi1_pulse_default.csv
```

### Shiny web interface

```bash
Rscript scripts/app_spirit.R
```

## Pipeline steps

1. **Input validation** — checks FASTA integrity and CSV column requirements
2. **Table merging** — inner-joins experiment tables on the gene ID column
3. **GFF parsing** — extracts CDS/gene features, creates a BED file with ±50 bp flanking regions
4. **Sequence extraction** — `bedtools getfasta` extracts target sequences
5. **IntaRNA prediction** — runs IntaRNA on real and shuffled sequences in parallel
6. **P-value estimation** — fits a Gumbel distribution to shuffled MFEs to derive IntaRNA p-values
7. **Meta-analysis** — integrates all p-values with Fisher's and Stouffer's methods, applies BH-FDR correction

## Output

Each run creates a timestamped folder under the output directory (symlinked as `data/latest`):

```
data/<timestamp>/
├── data/           # Combined tables (TSV, XLSX, BED), IntaRNA outputs
├── results/        # Diagnostic PDFs (Gumbel fit, p-value distributions, correlation plots)
├── logs/
│   └── pipeline.log
└── run_manifest.txt
```

## Bundled test data

Pre-loaded organisms in `data/default/`:

- **Salmonella enterica SL1344** + sRNA PinT
- **Bacteroides thetaiotaomicron** + sRNA MasB

## Authors

Hoda Kooshapour, Jakob J. Jung

**Supervision:** Alex Westermann, Lars Barquist

## License

[MIT](LICENSE)
