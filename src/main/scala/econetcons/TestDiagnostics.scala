package econetcons

object TestDiagnostics extends App:

  println("\n### NEW OPTIMIZED PARAMETERS (FINER GRID) ###")
  val currentParams = WorldParameters(
    numberOfSpecies = 200,
    connectanceMetaWeb = 0.4,
    areaOverlap = 20.0,
    medianHomeRange = 0.05,
    avgPopulationsPerTile = 10
  )

  val currentReport = ParameterDiagnostics.diagnose(currentParams)
  currentReport.printReport()

  println("\n\n### SUGGESTED PARAMETERS (Conservative) ###")
  val suggestedParams = ParameterDiagnostics.suggestParameters(
    targetSpecies = 100,
    targetPopsPerSpecies = 20,
    targetTileSize = 5.0,
    medianHomeRange = 0.01
  )

  val suggestedReport = ParameterDiagnostics.diagnose(suggestedParams)
  suggestedReport.printReport()

  println("\n\n### PARAMETER SWEEP ###")
  ParameterDiagnostics.parameterSweep()

end TestDiagnostics
