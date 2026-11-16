# Design evidence for the emergent assembly

Small diagnostic runs (3–5 seeds each) that back the parameter choices of the paper model. They are
interpreted in `docs/MODEL_DESIGN_AND_RESULTS.md`, section "Evidence". All were produced with
`econetcons.AssemblyDiagnostic` on 2026-10-12/14; the exact command of each run is the `running ...`
line of its `.log`.

| File | Question | Conclusion |
|---|---|---|
| `emergent_biomass_k.csv`, `emergent_biomass_N0.csv` | Does the number of seeds per species (fixed-k seeding) change the pyramid? | No: consumer biomass ≈ 0.13 × reachable basal biomass for every k and N₀. Led to seeding by range. |
| `assembly_diagnostic_emergent.csv` | First emergent runs under fixed-k seeding, B₀ 1000/3000 | Superseded by the range-seeding runs below; kept for the record. |
| `emergent_range.csv` | Effect of ρ̄, σ_ρ, home-range exponent b | ρ̄ is the main richness / web-density knob; b = 0.5 ≈ 0.75 at half the cost; σ_ρ minor. |
| `emergent_checks_tiles0.csv`, `emergent_checks_tiles13.csv` | B₀ sensitivity; tiles from populations-per-tile (`0`) vs fixed 13×13 grid | B₀ ×10 → +15–30 % richness; populations-per-tile gives an unstable tile count → fixed tile geometry. |
| `emergent_foraging.csv`, `emergent_foraging_species.csv` | Foraging rule supply / equal / niche | No material difference; per-species flow trophic positions. |

Caveats

- `emergent_biomass_*` and `assembly_diagnostic_emergent` were run with an intermediate code version
  (fixed-k seeding; flags `--basal`, `--seedPoints`). They cannot be regenerated with the current
  `AssemblyDiagnostic` and are kept as records of the design path.
- `AssemblyDiagnostic` writes to `assembly_diagnostic.csv` in the working directory unless `--out` is
  given. For new runs pass `--out data/emergent_design/<name>.csv`.
- No script summarises these files; the tables in the design document were read from them directly.
