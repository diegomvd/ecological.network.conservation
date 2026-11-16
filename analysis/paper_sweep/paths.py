"""Shared locations for the paper-sweep analysis (paths relative to the repo root, wherever the script is run from)."""
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data" / "paper_sweep"
FIGS = ROOT / "analysis" / "figures" / "paper_sweep"
SWEEP = DATA / "paper_sweep.csv"              # PaperSweep: one row per (C, t, frac, pExt, seed)
CELLSTATS = DATA / "paper_sweep_cellstats.csv"  # summary.py: mean/SE per (C, t, frac, pExt)
PROTECTED = DATA / "protected_sets.csv.gz"    # ProtectedSetExport: one row per population
FIGS.mkdir(parents=True, exist_ok=True)

SR_IR_COLORS = {"SR": "#d95f02", "IR": "#1b9e77"}
