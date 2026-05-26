#!/usr/bin/env python3
"""Render an RNA secondary-structure PNG colored by per-base pair probability.

Input: <name>_ss.ps and <name>_dp.ps produced by `RNAfold -p`.
Output: PNG with the RNAfold-chosen layout, nucleotide letters drawn on
viridis-colored discs, where the color is the total pair probability
of each position (sum over all partners j of p_ij).

Usage:
    render_srna_structure.py SS_PS DP_PS OUT_PNG
"""
import math
import re
import sys
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import LineCollection
from matplotlib.colors import Normalize


def parse_ss_ps(path: Path):
    text = path.read_text()
    seq_m = re.search(r"/sequence\s*\{\s*\(([^)]*)\)", text, re.S)
    if not seq_m:
        seq_m = re.search(r"/sequence\s*\(([^)]*)\)\s*def", text, re.S)
    seq = re.sub(r"\\\s*\n|\s+", "", seq_m.group(1))

    coord_block = re.search(r"/coor\s*\[(.*?)\]\s*def", text, re.S).group(1)
    coords = [
        (float(x), float(y))
        for x, y in re.findall(r"\[\s*(-?[\d.]+)\s+(-?[\d.]+)\s*\]", coord_block)
    ]

    pairs = []
    pair_m = re.search(r"/pairs\s*\[(.*?)\]\s*def", text, re.S)
    if pair_m:
        pairs = [
            (int(i), int(j))
            for i, j in re.findall(r"\[\s*(\d+)\s+(\d+)\s*\]", pair_m.group(1))
        ]
    return seq, coords, pairs


def parse_dp_ps(path: Path, n: int) -> np.ndarray:
    """Sum p_ij over all j for each i. Values in _dp.ps are sqrt(p)."""
    pp = np.zeros(n + 1)
    text = path.read_text()
    for m in re.finditer(
        r"(\d+)\s+(\d+)\s+([\d.eE+\-]+)\s+ubox", text
    ):
        i, j = int(m.group(1)), int(m.group(2))
        p2 = float(m.group(3)) ** 2
        pp[i] += p2
        pp[j] += p2
    return np.clip(pp[1:], 0.0, 1.0)


def render(ss_path: Path, dp_path: Path, out_path: Path) -> None:
    seq, coords, pairs = parse_ss_ps(ss_path)
    n = len(coords)
    assert len(seq) == n, f"seq length {len(seq)} != coord count {n}"
    pp = parse_dp_ps(dp_path, n)

    xs = np.array([c[0] for c in coords])
    ys = np.array([c[1] for c in coords])

    cmap = plt.get_cmap("viridis")
    norm = Normalize(0.0, 1.0)

    fig_dim = max(4.0, min(10.0, n / 12.0))
    fig, ax = plt.subplots(figsize=(fig_dim + 1.5, fig_dim))
    ax.set_aspect("equal")
    ax.axis("off")

    backbone = [
        [(xs[k], ys[k]), (xs[k + 1], ys[k + 1])] for k in range(n - 1)
    ]
    ax.add_collection(
        LineCollection(backbone, colors="#555555", linewidths=1.0, zorder=1)
    )

    pair_segs = [
        [(xs[i - 1], ys[i - 1]), (xs[j - 1], ys[j - 1])] for i, j in pairs
    ]
    if pair_segs:
        ax.add_collection(
            LineCollection(
                pair_segs, colors="#777777", linewidths=0.9, zorder=2
            )
        )

    marker_size = max(28, int(600 * math.exp(-n / 50)))
    ax.scatter(
        xs, ys, s=marker_size, c=pp, cmap=cmap, norm=norm,
        edgecolors="black", linewidths=0.3, zorder=3,
    )

    font_size = max(3, min(7, int(0.6 * math.sqrt(marker_size))))
    for k in range(n):
        text_color = "white" if pp[k] < 0.55 else "black"
        ax.text(
            xs[k], ys[k], seq[k],
            fontsize=font_size, ha="center", va="center",
            color=text_color, zorder=4,
            fontweight="bold",
        )

    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    cbar = fig.colorbar(sm, ax=ax, shrink=0.55, aspect=22, pad=0.02)
    cbar.set_label("Base-pair probability", fontsize=10)
    cbar.ax.tick_params(labelsize=8)

    pad = (xs.max() - xs.min() + ys.max() - ys.min()) * 0.04 + 5
    ax.set_xlim(xs.min() - pad, xs.max() + pad)
    ax.set_ylim(ys.min() - pad, ys.max() + pad)

    fig.savefig(out_path, dpi=150, bbox_inches="tight", facecolor="white")
    plt.close(fig)


if __name__ == "__main__":
    ss, dp, out = (Path(p) for p in sys.argv[1:4])
    render(ss, dp, out)
