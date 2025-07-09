package network.conservation

import scala.util.Random

object Main extends App:

  // Initialize the World parameters
  private val parameters = 
    WorldParameters(
      numberOfSpecies = 50,
      connectanceMetaWeb = 0.2,
      numberOfPopulations = 1000,
      basalHomeRange = 0.01,
      landscapeRadius = 3,
      fractionProtected = 0.7,
      connectivity = 0.0,
      decayDistancePopulations = 100000)

  // Initialize the World
  private val initialWorld = World(
    numberOfSpecies = parameters.numberOfSpecies,
    connectanceMetaWeb = parameters.connectanceMetaWeb,
    numberOfPopulations = parameters.numberOfPopulations,
    basalHomeRange = parameters.basalHomeRange,
    landscapeRadius = parameters.landscapeRadius,
    fractionProtected = parameters.fractionProtected,
    connectivity = parameters.connectivity,
    decayDistancePopulations = parameters.decayDistancePopulations,
    rnd = Random(12L)
  )

  println("Finished init")

  private val metaWeb = initialWorld.metaWeb
  val nSpeciesMeta = metaWeb.vertexSet().size()
  println(nSpeciesMeta)
  val nLinksMeta = metaWeb.edgeSet().size()
  println(nLinksMeta)
  val connectanceMeta = nLinksMeta.toDouble / ((nSpeciesMeta - 1) * nSpeciesMeta).toDouble
  println(connectanceMeta)
  println(".---------------------.")

  private val finalWorldPre = initialWorld.primaryExtinctions()

  // Perform the simulation
  // val (finalWorld, nCascades) = initialWorld.extinctions()
  private val (finalWorld, nCascades) = initialWorld.secondaryExtinctions()

  //println(nCascades)
  // Recover realized Web
  val finalMetaWeb = finalWorldPre.getRealizedWeb

  val nSpecies = finalMetaWeb.vertexSet().size()
  println(nSpecies)
  val nLinks = finalMetaWeb.edgeSet().size()
  println(nLinks)
  val connectance = nLinks.toDouble/((nSpecies-1)*nSpecies).toDouble

  println(connectance)
  println(".---------------------.")

  private val finalWorld2 = finalWorld.primaryExtinctions()

  //println(nCascades)
  // Recover realized Web
  val finalMetaWeb2 = finalWorld2.getRealizedWeb

  val nSpecies2 = finalMetaWeb2.vertexSet().size()
  println(nSpecies2)
  val nLinks2 = finalMetaWeb2.edgeSet().size()
  println(nLinks2)
  val connectance2 = nLinks2.toDouble / ((nSpecies2 - 1) * nSpecies2).toDouble
  println(connectance2)
  println(".---------------------.")

  private val (finalWorld3, nCascades3) = finalWorld2.secondaryExtinctions()

  //println(nCascades)
  // Recover realized Web
  val finalMetaWeb3 = finalWorld3.getRealizedWeb

  val nSpecies3 = finalMetaWeb3.vertexSet().size()
  println(nSpecies3)
  val nLinks3 = finalMetaWeb3.edgeSet().size()
  println(nLinks3)
  val connectance3 = nLinks3.toDouble / ((nSpecies3 - 1) * nSpecies3).toDouble
  println(connectance3)
  println(".---------------------.")






