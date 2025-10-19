package econetcons

case class ConservationParameters(
    fractionProtected: Double = 0.3,
    
    // Conservation intensity: 0 = random protection, 1+ = strategic protection
    conservationIntensity: Double = 1.0,
    
    // Strategy allocation weights (get normalized to simplex - sum to 1.0)
    strategyConnectivity: Double = 0.0,
    strategySpeciesRichness: Double = 1.0, 
    strategyInteractionRichness: Double = 0.0,
    strategyAbundance: Double = 0.0
) {
  
  // Normalize strategy weights to sum to 1.0 (simplex constraint)
  private val totalStrategy: Double = strategyConnectivity + strategySpeciesRichness + 
                                     strategyInteractionRichness + strategyAbundance
  
  // Handle edge case where all strategy weights are zero
  private val normalizedStrategy: (Double, Double, Double, Double) = 
    if (totalStrategy == 0.0) {
      // Equal weights if all are zero (uniform strategy)
      (0.25, 0.25, 0.25, 0.25)
    } else {
      (strategyConnectivity / totalStrategy,
       strategySpeciesRichness / totalStrategy,
       strategyInteractionRichness / totalStrategy,
       strategyAbundance / totalStrategy)
    }
  
  // Final conservation weights (intensity × normalized strategy)
  val weightConnectivity: Double = conservationIntensity * normalizedStrategy._1
  val weightSpeciesRichness: Double = conservationIntensity * normalizedStrategy._2
  val weightInteractionRichness: Double = conservationIntensity * normalizedStrategy._3
  val weightAbundance: Double = conservationIntensity * normalizedStrategy._4
  
  // Diagnostic methods for debugging/analysis
  def getStrategyWeights: (Double, Double, Double, Double) = normalizedStrategy
  def getTotalConservationWeight: Double = weightConnectivity + weightSpeciesRichness + 
                                          weightInteractionRichness + weightAbundance
                                          
  def isRandomProtection: Boolean = conservationIntensity == 0.0
  def isStrategicProtection: Boolean = conservationIntensity > 0.0
  
  override def toString: String = {
    f"ConservationParameters(protected=${fractionProtected}%.2f, intensity=${conservationIntensity}%.2f, " +
    f"strategy=[conn:${normalizedStrategy._1}%.3f, spp:${normalizedStrategy._2}%.3f, " +
    f"int:${normalizedStrategy._3}%.3f, abun:${normalizedStrategy._4}%.3f])"
  }
}