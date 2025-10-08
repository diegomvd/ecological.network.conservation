package network.conservation

import scala.util.Random

private def logNetworkDiagnostics(initialWorld: World, finalWorld: World, nCascades: Int): Unit = {
  
  println(s"\n" + "=" * 60)
  println(s"ECOLOGICAL NETWORK DIAGNOSTICS")
  println(s"=" * 60)
  
  // === POPULATION STATISTICS ===
  val initialPops = initialWorld.populations.count(!_.extinct)
  val finalPops = finalWorld.populations.count(!_.extinct)
  val extinctPops = initialPops - finalPops
  val popSurvivalRate = (finalPops.toDouble / initialPops) * 100
  
  println(s"POPULATION DYNAMICS:")
  println(f"  Initial populations: ${initialPops}")
  println(f"  Final populations: ${finalPops}")
  println(f"  Extinct populations: ${extinctPops}")
  println(f"  Population survival rate: ${popSurvivalRate}%.1f%%")
  println(f"  Extinction cascades: ${nCascades}")
  
  // === SPECIES STATISTICS ===
  val initialSpecies = initialWorld.populations.filter(!_.extinct).map(_.species.id).toSet.size
  val finalSpecies = finalWorld.populations.filter(!_.extinct).map(_.species.id).toSet.size
  val extinctSpecies = initialSpecies - finalSpecies
  val speciesSurvivalRate = (finalSpecies.toDouble / initialSpecies) * 100
  
  println(s"\nSPECIES DYNAMICS:")
  println(f"  Initial species: ${initialSpecies}")
  println(f"  Final species: ${finalSpecies}")
  println(f"  Extinct species: ${extinctSpecies}")
  println(f"  Species survival rate: ${speciesSurvivalRate}%.1f%%")
  
  // === POPULATION NETWORK STATISTICS ===
  import scala.jdk.CollectionConverters.*
  
  val popNetwork = finalWorld.populationWeb
  val popNodes = popNetwork.vertexSet().size()
  val popEdges = popNetwork.edgeSet().size()
  val popMaxPossibleEdges = popNodes * (popNodes - 1) // directed graph
  val popConnectance = if (popMaxPossibleEdges > 0) (popEdges.toDouble / popMaxPossibleEdges) * 100 else 0.0
  
  val popAvgDegree = if (popNodes > 0) (popEdges.toDouble / popNodes) else 0.0
  
  println(s"\nPOPULATION NETWORK:")
  println(f"  Nodes (populations): ${popNodes}")
  println(f"  Edges (interactions): ${popEdges}")
  println(f"  Connectance: ${popConnectance}%.3f%%")
  println(f"  Average degree: ${popAvgDegree}%.2f")
  
  // === SPECIES NETWORK STATISTICS ===
  val speciesNetwork = finalWorld.getRealizedWeb
  val speciesNodes = speciesNetwork.vertexSet().size()
  val speciesEdges = speciesNetwork.edgeSet().size()
  val speciesMaxPossibleEdges = speciesNodes * (speciesNodes - 1) // directed graph
  val speciesConnectance = if (speciesMaxPossibleEdges > 0) (speciesEdges.toDouble / speciesMaxPossibleEdges) * 100 else 0.0
  
  val speciesAvgDegree = if (speciesNodes > 0) (speciesEdges.toDouble / speciesNodes) else 0.0
  
  println(s"\nSPECIES NETWORK (REALIZED):")
  println(f"  Nodes (species): ${speciesNodes}")
  println(f"  Edges (interactions): ${speciesEdges}")
  println(f"  Connectance: ${speciesConnectance}%.3f%%")
  println(f"  Average degree: ${speciesAvgDegree}%.2f")
  
  // === TROPHIC STRUCTURE ===
  val survivingPopulations = finalWorld.populations.filter(!_.extinct)
  val trophicCounts = survivingPopulations.groupBy(_.species.trophicLevel).map {
    case (level, pops) => (level, pops.size, pops.map(_.species.id).toSet.size)
  }.toSeq.sortBy(_._1)
  
  println(s"\nTROPHIC STRUCTURE:")
  trophicCounts.foreach { case (level, popCount, speciesCount) =>
    val levelName = level match {
      case 0 => "Basal"
      case 1 => "Primary consumers" 
      case 2 => "Top predators"
      case _ => s"Level ${level}"
    }
    println(f"  ${levelName}: ${speciesCount} species, ${popCount} populations")
  }
  
  // === CONSERVATION EFFECTIVENESS ===
  val protectedAreas = finalWorld.managementLandscape.managementAreas.count(_.status == ProtectionStatus.Protected)
  val totalAreas = finalWorld.managementLandscape.managementAreas.size
  val actualProtectionFraction = (protectedAreas.toDouble / totalAreas) * 100
  
  println(s"\nCONSERVATION STATUS:")
  println(f"  Protected areas: ${protectedAreas}/${totalAreas} (${actualProtectionFraction}%.1f%%)")
  println(f"  Conservation efficiency: ${speciesSurvivalRate / actualProtectionFraction}%.2fx species/protection")
  
  // === NETWORK ROBUSTNESS INDICATORS ===
  val networkFragmentation = if (initialPops > 0) ((initialPops - popNodes).toDouble / initialPops) * 100 else 0.0
  val trophicCollapse = trophicCounts.count(_._3 == 0) // levels with no species
  
  println(s"\nNETWORK ROBUSTNESS:")
  println(f"  Network fragmentation: ${networkFragmentation}%.1f%% (populations disconnected)")
  println(f"  Trophic levels lost: ${trophicCollapse}/3")
  
  println(s"=" * 60)
}

object OneSimulation extends App:

    // Initialize the World parameters
    private val worldParameters: WorldParameters =  
        WorldParameters(
            numberOfSpecies = 200,
            connectanceMetaWeb = 0.4,
            areaOverlap = 50.0,
            medianHomeRange = 0.05,
            avgPopulationsPerTile = 50,
        )   

    private val conservationParameters: ConservationParameters =
        ConservationParameters(
            fractionProtected = 0.3,
            conservationIntensity = 1.0,
            strategyConnectivity = 1.0,
            strategySpeciesRichness = 1.0,
            strategyInteractionRichness = 1.0,
            strategyAbundance = 1.0
        )        

    // Initialize the World
    private val initialWorld = World(
        worldParameters = worldParameters,
        conservationParameters = conservationParameters,
        rnd = Random(12L)
    )

    private val (finalWorld, nCascades) = initialWorld.extinctions()

    logNetworkDiagnostics(initialWorld, finalWorld, nCascades)

    
