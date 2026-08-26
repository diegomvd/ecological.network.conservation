package econetcons

/**
 * Conservation parameters supporting both simplified and flexible strategies
 *
 * SIMPLIFIED MODE (recommended for most uses):
 *   Use the `strategy` parameter to select one of the predefined all-or-nothing strategies:
 *   - ConservationStrategy.Random
 *   - ConservationStrategy.SpeciesRichness
 *   - ConservationStrategy.InteractionRichness
 *   - ConservationStrategy.Abundance
 *   - ConservationStrategy.Connectivity
 *
 * FLEXIBLE MODE (advanced):
 *   Set strategy = None and manually specify weights for each criterion.
 *   Weights will be automatically normalized to sum to 1.0
 */
case class ConservationParameters(
    fractionProtected: Double = 0.3,

    // Simplified strategy selection (overrides manual weights if provided)
    strategy: Option[ConservationStrategy] = Some(ConservationStrategy.SpeciesRichness),

    // Manual strategy weights (only used if strategy = None)
    // These get normalized to simplex - sum to 1.0
    strategyConnectivity: Double = 0.0,
    strategySpeciesRichness: Double = 1.0,
    strategyInteractionRichness: Double = 0.0,
    strategyAbundance: Double = 0.0,

    // Number of administrative regions (must be a perfect square >= 1)
    // 1 = global selection (current behaviour); >1 = each region selects independently
    nAdminRegions: Int = 1
) {

  // Determine actual weights based on simplified strategy or manual weights
  private val rawWeights: (Double, Double, Double, Double) = strategy match {
    case Some(strat) =>
      // Use predefined strategy weights
      strat.toWeights
    case None =>
      // Use manual weights
      (strategyConnectivity, strategySpeciesRichness, strategyInteractionRichness, strategyAbundance)
  }

  // Normalize strategy weights to sum to 1.0 (simplex constraint)
  private val totalStrategy: Double = rawWeights._1 + rawWeights._2 + rawWeights._3 + rawWeights._4

  // Handle edge case where all strategy weights are zero (random protection)
  private val normalizedStrategy: (Double, Double, Double, Double) =
    if (totalStrategy == 0.0) {
      // All zeros = truly random protection
      (0.0, 0.0, 0.0, 0.0)
    } else {
      (rawWeights._1 / totalStrategy,
       rawWeights._2 / totalStrategy,
       rawWeights._3 / totalStrategy,
       rawWeights._4 / totalStrategy)
    }

  // Final conservation weights
  val weightConnectivity: Double = normalizedStrategy._1
  val weightSpeciesRichness: Double = normalizedStrategy._2
  val weightInteractionRichness: Double = normalizedStrategy._3
  val weightAbundance: Double = normalizedStrategy._4

  // Diagnostic methods for debugging/analysis
  def getStrategyWeights: (Double, Double, Double, Double) = normalizedStrategy
  def getTotalConservationWeight: Double = weightConnectivity + weightSpeciesRichness +
                                          weightInteractionRichness + weightAbundance

  def isComplementarityProtection: Boolean = strategy.contains(ConservationStrategy.InteractionComplementarity)
  def isSpeciesRarityProtection: Boolean = strategy.contains(ConservationStrategy.SpeciesRarity)
  def isInteractionRarityProtection: Boolean = strategy.contains(ConservationStrategy.InteractionRarity)
  def isEgocentricInteractionRichnessProtection: Boolean = strategy.contains(ConservationStrategy.EgocentricInteractionRichness)
  def isSpeciesComplementarityProtection: Boolean =
    strategy.contains(ConservationStrategy.SpeciesComplementarity)
  def isEgocentricInteractionComplementarityProtection: Boolean =
    strategy.contains(ConservationStrategy.EgocentricInteractionComplementarity)
  def isRandomProtection: Boolean =
    totalStrategy == 0.0 &&
    !isComplementarityProtection &&
    !isSpeciesRarityProtection &&
    !isInteractionRarityProtection &&
    !isSpeciesComplementarityProtection &&
    !isEgocentricInteractionComplementarityProtection
  def isStrategicProtection: Boolean = totalStrategy > 0.0

  override def toString: String = {
    val strategyDesc = strategy.map(s => s"${s.toString}").getOrElse("Custom")
    f"ConservationParameters(protected=${fractionProtected}%.2f, strategy=${strategyDesc}, " +
    f"weights=[conn:${normalizedStrategy._1}%.3f, spp:${normalizedStrategy._2}%.3f, " +
    f"int:${normalizedStrategy._3}%.3f, abun:${normalizedStrategy._4}%.3f])"
  }
}

object ConservationParameters {
  /**
   * Create conservation parameters with a simplified strategy
   */
  def apply(fractionProtected: Double, strategy: ConservationStrategy): ConservationParameters =
    ConservationParameters(fractionProtected = fractionProtected, strategy = Some(strategy))
}