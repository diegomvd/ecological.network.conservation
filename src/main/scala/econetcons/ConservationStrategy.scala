package econetcons

/**
 * Simplified all-or-nothing conservation strategies
 * Each strategy focuses exclusively on a single criterion
 */
enum ConservationStrategy:
  case Random                       // Completely random protection (no preference)
  case SpeciesRichness              // Prioritize areas with highest species diversity
  case InteractionRichness          // Prioritize areas with most ecological interactions (intra-tile only)
  case EgocentricInteractionRichness // Prioritize areas with most interactions where any endpoint is in the tile
  case Abundance                    // Prioritize areas with most populations
  case Connectivity                 // Prioritize areas connected to already protected areas
  case ConnectivityInteraction      // Equal blend of connectivity and interaction richness (0.5 / 0.5)
  case InteractionComplementarity   // Greedy selection maximizing new interactions in protected set
  case SpeciesRarity                // Prioritize areas with range-restricted species (sum of 1/occupancy)
  case InteractionRarity            // Prioritize areas with range-restricted interacting pairs (sum of 1/pair-occupancy)
  case SpeciesComplementarity               // Greedy: maximize unique species covered (random first tile)
  case EgocentricInteractionComplementarity // Greedy: maximize unique egocentric interactions covered (random first tile)
  /**
   * Convert strategy to weight vector (connectivity, speciesRichness, interactionRichness, abundance)
   */
  def toWeights: (Double, Double, Double, Double) = this match
    case Random                     => (0.0, 0.0, 0.0, 0.0)  // All zeros = random selection
    case SpeciesRichness            => (0.0, 1.0, 0.0, 0.0)
    case InteractionRichness              => (0.0, 0.0, 1.0, 0.0)
    case EgocentricInteractionRichness    => (0.0, 0.0, 1.0, 0.0)  // Same weights; scoring differs in ManagementLandscape
    case Abundance                  => (0.0, 0.0, 0.0, 1.0)
    case Connectivity               => (1.0, 0.0, 0.0, 0.0)
    case ConnectivityInteraction    => (0.5, 0.0, 0.5, 0.0)
    case InteractionComplementarity => (0.0, 0.0, 0.0, 0.0)  // Not used — complementarity uses greedy algorithm
    case SpeciesRarity              => (0.0, 0.0, 0.0, 0.0)  // Not used — rarity uses precomputed scores
    case InteractionRarity          => (0.0, 0.0, 0.0, 0.0)  // Not used — rarity uses precomputed scores
    case SpeciesComplementarity               => (0.0, 0.0, 0.0, 0.0)
    case EgocentricInteractionComplementarity => (0.0, 0.0, 0.0, 0.0)

  /**
   * Get human-readable description of the strategy
   */
  def description: String = this match
    case Random                     => "Random protection (no preference)"
    case SpeciesRichness            => "Protect areas with highest species diversity"
    case InteractionRichness              => "Protect areas with most ecological interactions (intra-tile)"
    case EgocentricInteractionRichness    => "Protect areas with most interactions involving any local population (ego-centric)"
    case Abundance                  => "Protect areas with most populations"
    case Connectivity               => "Protect areas connected to existing protected areas"
    case ConnectivityInteraction    => "Equal blend of connectivity and interaction richness"
    case InteractionComplementarity => "Greedy selection maximizing complementarity of protected interactions"
    case SpeciesRarity              => "Protect areas with most range-restricted species"
    case InteractionRarity          => "Protect areas with most range-restricted interacting pairs"
    case SpeciesComplementarity               => "Greedy maximization of unique species covered across protected set (random first tile)"
    case EgocentricInteractionComplementarity => "Greedy maximization of unique egocentric interactions covered across protected set (random first tile)"

end ConservationStrategy
