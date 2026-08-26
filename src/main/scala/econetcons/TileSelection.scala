package econetcons

/**
 * Selection rule for choosing the next tile to protect within the weighted-criteria
 * protection recursion (species richness / connectivity / interaction richness / abundance).
 *
 *   - Argmax:     deterministically pick the highest-scoring remaining tile (ties broken
 *                 randomly). This is the meaningful rule for the conservation question —
 *                 "protect the best tiles" — and lets the score be precomputed once.
 *   - Stochastic: roulette-wheel sampling proportional to tile score (legacy behaviour).
 */
enum TileSelection:
  case Argmax
  case Stochastic
