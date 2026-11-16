# econetcons Performance Optimization — Action Plan

**Purpose:** implement five sets of changes to speed up the simulation, discussed and
agreed upon in design review. Tasks 1–3 and 5 are **behavior-preserving** (same RNG
call sequence, same outputs bit-for-bit given the same seed) and should be done first,
verified with a regression check, then committed as a single low-risk batch. Task 4
**changes simulation behavior** (different population-placement model) and requires a
separate calibration step before it can be trusted for production sweeps.

---

## Priority & risk order

| # | Task | File(s) | Behavior-preserving? | Expected impact |
|---|------|---------|----------------------|------------------|
| 1 | Precompute static prioritization scores; fix O(nTiles) connectivity update | `ManagementLandscape.scala` | Yes | High (dominant cost at large `nTiles`) |
| 2 | O(1) population lookup in `getInteractionRichness` | `ManagementArea.scala`, `ManagementLandscape.scala` | Yes | High (setup cost, scales with P²) |
| 3 | Group populations by tile once instead of per-tile filter | `ManagementLandscape.scala` | Yes | High (scales with nTiles × P) |
| 4 | Replace Thomas-process + k-random-thinning with branching random walk | `Population.scala`, `Utils.scala` | **No — requires calibration** | Likely largest single per-run win, but needs validation before trusting sweep results |
| 5 | Remove hot-path debug I/O; fix `Seq` concatenation in population generation | `World.scala` | Yes | Low-moderate, essentially free |

Implement and verify 1 → 2 → 3 → 5 together first (all behavior-preserving, can be
regression-tested as a batch). Implement 4 last, in isolation, with its own
calibration protocol below.

---

## Task 1 — `ManagementLandscape.applyProtectionPlan` / `protectionRecursion`

**Problem:** every iteration of the greedy tile-selection recursion recomputes
min/max and rescales **all four** score components (species richness, connectivity,
interaction richness, abundance) across all remaining tiles, and `updateConnectivity`
rebuilds the entire connectivity map by checking every remaining tile against the
just-selected tile's neighbor list. Since species richness, interaction richness, and
abundance never change for a tile once computed (only *which tiles remain* changes,
not their raw values), recomputing their scaled scores every iteration is pure waste.
Connectivity is the only component that actually changes at each step, and only for
the ~8 Moore neighbors of the tile just selected — not for every remaining tile.

**Changes:**

1. Before calling `protectionRecursion`, compute the min-max-scaled probability
   contributions for species richness, interaction richness, and abundance **once**:
   ```scala
   val staticScore: Map[Int, Double] = managementAreas.map { a =>
     val bio = scalingMinMax(areasSpeciesRichness(a.id), minSpeciesRichness, maxSpeciesRichness) * conservationParameters.weightSpeciesRichness
     val int = scalingMinMax(areasInteractionRichness(a.id), minInteractionRichness, maxInteractionRichness) * conservationParameters.weightInteractionRichness
     val ab  = scalingMinMax(areasAbundance(a.id), minAbundance, maxAbundance) * conservationParameters.weightAbundance
     (a.id, bio + int + ab)
   }.toMap
   ```
   Pass `staticScore` into `protectionRecursion` instead of the three raw maps.
   Inside the recursion, at each step: `totalConservationProbability(tileId) = staticScore(tileId) + weightConnectivity * scalingMinMax(connectivity(tileId), minConn, maxConn)`.
   Note min/max of connectivity still needs recomputing each step since connectivity
   values change — but this is over a map that's only updated locally (see next point),
   so keep it cheap.

2. Fix `updateConnectivity` to update only the affected neighbors instead of
   scanning every remaining tile:
   ```scala
   def updateConnectivity(areasConnectivity: Map[Int,Int], tileId: Int): Map[Int,Int] =
     val neighbors = grid.neighbors(tileId)
     val withoutSelected = areasConnectivity - tileId
     neighbors.foldLeft(withoutSelected) { (acc, nbrId) =>
       acc.get(nbrId).map(c => acc.updated(nbrId, c + 1)).getOrElse(acc)
     }
   ```
   This is O(8) instead of O(remaining tiles) per step.

3. Since `conservationParameters.weightConnectivity` is currently always `0.0` in
   your production configs, consider (optional, low priority) short-circuiting the
   connectivity computation entirely when the weight is zero — skip building
   `areasConnectivity` and the `updateConnectivity` call altogether. This is not
   required for current runs to be correct, just an extra few percent. **Do not
   skip implementing fix (2) even though it's not on your critical path today** —
   it's cheap to do correctly now and avoids a silent quadratic surprise if
   connectivity weighting is used in future work.

**Verification:** with `weightConnectivity = 0`, results must be bit-identical to
the current implementation for the same seed — this is a pure refactor of *how*
the same score is computed, not a change to the score itself.

---

## Task 2 — O(1) lookup in `ManagementArea.getInteractionRichness`

**Problem:** for every population in every tile, `populationWeb.vertexSet().asScala.find(_.id == popId)`
performs a linear scan over all populations. Called once per population across all
tiles, this makes the setup cost O(P²).

**Why it's safe:** `getInteractionRichness` is called exactly once per simulation, in
`ManagementLandscape.applyProtectionPlan`, immediately after `ManagementLandscape.apply`
constructs the tiles from `populationsInit = populationWeb.vertexSet().asScala.toSeq`
(`World.apply`). At that point every `popId` referenced in any tile's `populations`
map is guaranteed to exist in `populationWeb.vertexSet()`, since both were derived
from the same population set and `Population.id` is unique. A map lookup is provably
equivalent to the linear scan at this call site.

**Change:** in `ManagementLandscape.applyProtectionPlan`, build the lookup map once
and thread it through:
```scala
val idToPop: Map[Int, Population] =
  this.populationWeb.vertexSet().asScala.map(p => p.id -> p).toMap
```
Change `ManagementArea.getInteractionRichness` to accept this map instead of the
raw graph (or accept both and use the map internally):
```scala
def getInteractionRichness(populationWeb: DefaultDirectedGraph[Population, DefaultEdge], idToPop: Map[Int, Population]): Int =
  val speciesInteractions = scala.collection.mutable.Set[(Int, Int)]()
  val populationIds = this.populations.keySet
  populationIds.foreach { popId =>
    idToPop.get(popId).foreach { predatorPop =>
      populationWeb.outgoingEdgesOf(predatorPop).asScala.foreach { edge =>
        val preyPop = populationWeb.getEdgeTarget(edge)
        if (populationIds.contains(preyPop.id)) {
          speciesInteractions += ((predatorPop.species.id, preyPop.species.id))
        }
      }
    }
  }
  speciesInteractions.size
```

**Caution for future use:** if `getInteractionRichness` is ever called again later
in a simulation (e.g., after extinctions, for a diagnostic), the `idToPop` map must
be rebuilt from whatever `populationWeb` is current at that call — don't cache it
stale across a `World` state transition. Add a comment to this effect at the call site.

**Verification:** bit-identical output for the same seed (pure lookup optimization,
no logic change).

---

## Task 3 — `ManagementLandscape.apply`: group populations by tile once

**Problem:** for each of `nTiles` tiles, `populations.collect { case p if landscapeGrid.coordToCell(p.coordinates) == cellId => ... }`
scans all P populations. O(nTiles × P) total.

**Change:**
```scala
def apply(
  landscapeGrid: SquareGrid,
  populations: Seq[Population],
  populationWeb: DefaultDirectedGraph[Population, DefaultEdge],
  rnd: Random
): ManagementLandscape =

  val populationsByTile: Map[Int, Seq[Population]] =
    populations.groupBy(p => landscapeGrid.coordToCell(p.coordinates))

  val areas = landscapeGrid.cellIndices.map { cellId =>
    val pops = populationsByTile.getOrElse(cellId, Seq.empty).map(p => (p.id, p.species.id))
    ManagementArea(cellId, ProtectionStatus.Unprotected, pops.toMap)
  }

  ManagementLandscape(areas, landscapeGrid, populationWeb)
```
O(P) for the groupBy, O(1) amortized lookup per tile.

**Verification:** bit-identical output for the same seed.

---

## Task 4 — Replace Thomas process + k-random-thinning with a branching random walk

**Problem:** `Utils.adaptiveKRandomThinning` / `singlePassThinning` reshuffle the
entire accumulated point set (`rnd.shuffle(allSelected).take(k)`) for every candidate
point considered, and use `Seq.contains` (linear) for exclusion filtering — likely
the single most expensive per-simulation cost for species with large abundances.

**Design decision (agreed):** drop the minimum-spacing/thinning property entirely.
Replace the "sample N points around a fixed center, then thin" approach with an
**Eden-growth-style branching random walk**: each new individual is placed near a
*randomly chosen existing individual of the same species* (not always the most
recent one), rather than radiating from one fixed center. This produces organic,
non-circular, irregular cluster shapes — closer to your original recursive
"random walk" draft — while remaining strictly O(N), single-pass, with no pairwise
distance checks at all.

**Algorithm:**
1. Place the first individual at a random anchor point: `SquareGrid.randomPoint(rnd)`
   (same as today's cluster center).
2. For each subsequent individual `i` (2 to N):
   - Pick a parent uniformly at random from the individuals already placed:
     `parentIdx = rnd.nextInt(currentSize)` — O(1) via array indexing.
   - Draw a short offset from the parent: `distance ~ Exponential(1/stepScale)`,
     `angle ~ Uniform(0, 2π)` (reuse `Utils.exponentialDistribution`, already
     implemented).
   - New point = parent coordinate + (distance·cos(angle), distance·sin(angle)),
     wrapped via `SquareGrid.wrapCoordinate`.
3. No filtering, no rejection, no distance checks against any other point.

**Implementation:**

In `Utils.scala`, add:
```scala
def generateBranchingWalk(
  n: Int,
  stepScale: Double,
  rnd: Random
): Seq[Coordinate] = {
  val points = scala.collection.mutable.ArrayBuffer[Coordinate](SquareGrid.randomPoint(rnd))
  while (points.size < n) {
    val parent = points(rnd.nextInt(points.size))
    val angle = rnd.nextDouble() * 2 * math.Pi
    val distance = exponentialDistribution(1.0 / stepScale, rnd)
    val x = parent.x + distance * math.cos(angle)
    val y = parent.y + distance * math.sin(angle)
    points += SquareGrid.wrapCoordinate(new Coordinate(x, y, 0.0))
  }
  points.toSeq
}
```

In `Population.scala`, replace the body of `createPopulations`:
```scala
def createPopulations(species: Species, populationSeq: Seq[Population], rnd: Random): Seq[Population] =
  val targetPopulations = populationSeq.size
  val stepScale = species.homeRange * stepScaleFactor // see calibration below
  val coordinates = Utils.generateBranchingWalk(targetPopulations, stepScale, rnd)
  coordinates.zip(populationSeq).map { case (coordinate, population) =>
    population.copy(coordinates = coordinate, extinct = false)
  }
```

**Do not delete** `generateThomasProcessCandidates`, `adaptiveKRandomThinning`,
`singlePassThinning` yet — keep them (rename with a `Legacy` suffix or move to a
clearly marked section) so the calibration script in the next section can run both
models side-by-side. Remove them only after the calibration step below is signed off.

### Calibrating `stepScale`

The old model's cluster extent was governed by `clusterRadius = homeRange * targetPopulations * spreadingFactor`
(spreadingFactor = 1000.0), i.e. roughly linear in population count N, sampled from
a fixed center. The new model's extent grows differently: because parents are drawn
uniformly from all existing points (not just the most recent one), the cluster fills
out roughly compactly, with typical radius scaling approximately with `sqrt(N) · stepScale`
rather than linearly with N.

**Do not treat this as solved analytically.** As a starting point for the search,
try `stepScaleFactor` such that `stepScale ≈ homeRange · sqrt(targetPopulations) · spreadingFactor`,
but treat this purely as an initial guess to refine empirically — the constant of
proportionality for an Eden-growth-style cluster's extent is model-specific and not
something to trust from a back-of-envelope scaling argument alone.

**Calibration procedure (run before accepting Task 4):**
1. Pick 2–3 representative `WorldParameters` combinations spanning the range used in
   your sweeps (e.g. `avgPopulationsPerTile` = 2, 20, 160 as in your existing figures).
2. For each, generate populations under both the old model (legacy functions) and
   the new branching-walk model, same seed.
3. Compare, per species (or pooled): the empirical spatial extent (e.g., mean/95th
   percentile pairwise distance within a species' population, or bounding-box area),
   nearest-neighbor distance distribution, and — most importantly for your paper's
   results — the downstream statistics that matter: `avgPopulationsPerTile` realized
   distribution, `landscapeAlphaIR`/`landscapeAlphaSR`, `spIntCorrelation`, and one or
   two extinction-vs-fraction-protected curves (SR vs IR strategies) at a couple of
   `nTiles` values.
4. Adjust `stepScaleFactor` until the realized spatial extent roughly matches the old
   model's, then check whether the downstream ecological statistics in step 3 are
   close enough to not materially change your existing figures. If they diverge
   noticeably, that's a finding to note explicitly (not necessarily a blocker) —
   decide with the group whether existing sweep results need to be regenerated
   under the new population model before being used in the paper.
5. Once a `stepScaleFactor` is chosen and signed off, hardcode it as a named constant
   (not a magic number) in `WorldParameters` or `Population`, and remove the legacy
   Thomas/thinning code.

**This task should be implemented and merged separately from Tasks 1–3/5**, since
it is the only one that changes simulation output for a given seed.

---

## Task 5 — Cleanups

1. **Remove/guard hot-path debug I/O.** In `World.scala`:
   - `secondaryExtinctions` → `extinctionCascades`: remove
     `println(s"  Cascade ${nCascades + 1}: Removing ${extinguishVertices.size} predators without prey")`
     (fires every cascade iteration, every simulation run).
   - `generateMetaWeb`: remove `println(f"Generated metaweb connectance: ...")`.
   - Leave the diagnostic `println`s in `OneSimulation.scala` alone — that file is
     an interactive single-run driver, not called from sweeps, so its I/O is fine.
   - If any of this output is still wanted for interactive debugging, gate it
     behind a `verbose: Boolean = false` parameter rather than deleting outright —
     your call based on how often it's actually used.

2. **Fix `Seq` concatenation in `World.generatePopulations`.** The current
   `foldLeft` does `allPopulations ++ spatiallyArrangedPopulations` per species,
   which is O(n) per concatenation on a default immutable `Seq`. Replace with
   either:
   - A mutable accumulator (`ArrayBuffer`), appended per species then converted
     once at the end, or
   - Precompute per-species ID offsets first (pure, O(nSpecies)) via a running
     sum, then `metaWeb.vertexSet().asScala.toSeq.flatMap { species => ... }`
     directly without threading an accumulator through a fold at all. This also
     opens the door to trivially parallelizing population generation across
     species later if needed (each species' population generation is independent
     given its ID offset), though that's out of scope for this pass.

**Verification:** bit-identical output for the same seed (both changes are pure
refactors of accumulation mechanics, not logic).

---

## Suggested implementation order for the coding session

1. Branch from current `main`/deprecated-remote state.
2. Implement Tasks 1, 2, 3, 5 together (all behavior-preserving).
3. Regression test: run `OneSimulation.scala` (or an equivalent fixed-seed harness)
   before and after, on at least 2–3 different `WorldParameters`/`ConservationParameters`
   combinations, and confirm identical `speciesExtinctions`, `interactionExtinctions`,
   and `nCascades` output. Any discrepancy means a behavior-preserving assumption was
   violated — stop and investigate before merging.
4. Benchmark: time a representative sweep-like batch of runs before/after to quantify
   the speedup from 1–3/5 alone.
5. Commit/merge 1–3/5 as one PR.
6. Implement Task 4 in a separate branch, keeping legacy functions available.
7. Run the calibration procedure in Task 4 above; document the chosen `stepScaleFactor`
   and the comparison results (ideally as a short note/appendix, since this is a
   methodological decision that may need to be justified in the paper's methods
   section).
8. Once signed off, remove legacy Thomas/thinning code, merge Task 4 separately from
   the Task 1–3/5 PR so the behavior-changing commit is isolated and easy to
   reference/revert independently if needed.

## Rollback / safety notes
- Tasks 1–3 and 5 carry effectively no risk to existing results — they are pure
  performance refactors with regression tests. Merge with confidence once the
  bit-identical check passes.
- Task 4 is the only task that requires a judgment call. Keep it in its own commit/PR
  so that if calibration reveals a meaningful shift in downstream statistics, the
  decision to accept or reject the new population model is cleanly separable from
  the (safe) performance work.
