package network.conservation

import scala.util.Random

object OneSimulation extends App:

    // Initialize the World parameters
    private val worldParameters: WorldParameters =  
        WorldParameters(
            numberOfSpecies = 200,
            connectanceMetaWeb = 0.2,
            areaOverlap = 2.0,
            medianHomeRange = 0.01,
            avgPopulationsPerTile = 10,
        )   

    private val conservationParameters: ConservationParameters =
        ConservationParameters(
            fractionProtected = 0.3,
            weightConnectivity = 0.0,
            weightSpeciesRichness = 0.0,
            weightInteractionRichness = 0.0,
            weightAbundance = 0.0
        )        

    // Initialize the World
    private val initialWorld = World(
        worldParameters = worldParameters,
        conservationParameters = conservationParameters,
        rnd = Random(12L)
    )

    private val finalWorld, nCascades = initialWorld.extinctions()

    
