package network.conservation

case class ConservationParameters(
    fractionProtected: Double = 0.3,
    weightConnectivity: Double = 0.5,
    weightSpeciesRichness: Double = 0.5,
    weightInteractionRichness: Double = 0.5,
    weightAbundance: Double = 0.5
)