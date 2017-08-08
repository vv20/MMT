package info.kwarc.mmt.odk.SCSCP.Server

import info.kwarc.mmt.api.Path
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.CD.{SymbolSet, scscp1, scscp2}
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/**
  * SCSCP server that implements the MitM protocol.
  */
object MitMServer {
  final val defaultPort = 26133
  val server = SCSCPServer("MitMServer", "1.0", "MitMServer")
  var controller: Controller = _

  def run(): Unit = {
//  def main(args: Array[String]): Unit = {
    // connect the handlers for registerServer and removeServer
    server.register(OMSymbol("registerServer", "mitm_transient", None, None), new RegisterServerHandler())
    server.register(OMSymbol("removeServer", "mitm_transient", None, None), new RemoveServerHandler())
    server.register(OMSymbol("getAllServers", "mitm_transient", None, None), new GetAllServersHandler())
    server.register(OMSymbol("registerFunction", "mitm_transient", None, None), new RegisterFunctionHandler())
    server.register(OMSymbol("removeFunction", "mitm_transient", None, None), new RemoveFunctionHandler())

    // and serve it forever
    server.processForever()
  }
}

/**
  * Database that stores and manages the function headers received from CAS clients.
  * Currently implemented in-memory, could in the long run be moved to the file system for extensibility.
  */
object MitMDatabase {
  var mapOfServersToFunctions : Map[String, List[OMSymbol]] = Map()
  var mapOfServerNamesToClients : Map[String, SCSCPClient] = Map()

  /**
    * Adds a new CAS to the list of systems MitM can route to.
    * @param serverAddress the address of the CAS.
    * @param port the port at which the CAS serves SCSCP.
    * @return the OpenMath string of the server name/hander.
    */
  def addServer(serverAddress: String, port: Int): OMString = {
    // create an scscp client for the CAS server
    val client : SCSCPClient = SCSCPClient(serverAddress, port)

    // create a unique handler for the CAS based on its service name
    var serverID : String = client.service_id
    var counter : Int = 0
    while (mapOfServerNamesToClients.contains(serverID)) {
      serverID = client.service_id + counter.toString
      counter = counter + 1
    }

    mapOfServerNamesToClients += (serverID -> client)
    mapOfServersToFunctions += (serverID -> List())
    OMString(serverID, None)
  }

  /**
    * Removes a CAS from the list of systems MitM can route to.
    * @param serverID the handler of the CAS.
    * @return confirmation that the server has been removed.
    */
  def removeServer(serverID: String): OMSymbol = {
    val client = mapOfServerNamesToClients(serverID)
    val listOfFunctions = mapOfServersToFunctions.get(serverID)
    for (symbol : OMSymbol <- listOfFunctions.get) {
      MitMServer.server.unregister(symbol)
    }
    client.quit()
    mapOfServerNamesToClients -= serverID
    mapOfServersToFunctions -= serverID
    scscp1(scscp1.procedureCompleted)
  }

  /**
    * Binds a symbol in one of the global CDs to a function exposed by a CAS.
    * @param serverID the handler of the CAS.
    * @param function the symbol exposed by the CAS.
    * @param boundTo the symbol in a CD that the function is getting bound to.
    */
  def addFunction(serverID: String, function: OMSymbol, boundTo: OMSymbol): OMSymbol = {
    // some predefined convenience symbols
    val signatureReqSym = OMSymbol("get_signature", "scscp2", None, None)
    val signatureSym = OMSymbol("signature", "scscp2", None, None)
    val infinity = OMSymbol("infinity", "nums1", None, None)
    val unboundDomain = scscp2(scscp2.symbolSetAll)

    // get the signature of the function from the CAS
    val signatureRequest = OMApplication(signatureReqSym, List(function), None, None)
    val client = MitMDatabase.mapOfServerNamesToClients(serverID)
    val signature = client(signatureRequest).fetch().get

    signature match {
      case OMApplication(`signatureSym`, args, None, None) =>
        args match {
          // bound number of args + bound domain
          case List(`function`, OMInteger(min, None), OMInteger(max, None), sig:OMApplication) =>
            MitMServer.server.register(boundTo, new RemoteCallHandler(client, min.toInt, max.toInt, sig, function))
          // unbound number of args + bound domain
          case List(`function`, OMInteger(min, None), `infinity`, sig:OMApplication) =>
            MitMServer.server.register(boundTo, new RemoteCallHandler(client, min.toInt, -1, sig, function))
          // bound number of args + unbound domain
          case List(`function`, OMInteger(min, None), OMInteger(max, None), `unboundDomain`) =>
            MitMServer.server.register(boundTo, new RemoteCallHandler(client, min.toInt, max.toInt, SymbolSet(Nil), function))
          // unbound number of args + unbound domain
          case List(`function`, OMInteger(min, None), `infinity`, `unboundDomain`) =>
            MitMServer.server.register(boundTo, new RemoteCallHandler(client, min.toInt, -1, SymbolSet(Nil), function))

          case _ =>
            throw new RemoteServerException
        }
      case _ =>
        throw new RemoteServerException
    }
    scscp1(scscp1.procedureCompleted)
  }

  def removeFunction(serverID: String, function: OMSymbol): OMSymbol = {
    val listOfSymbols = MitMDatabase.mapOfServersToFunctions(serverID)
    // define what it means for symbols to be equal, in this case
    // symbols are considered equal if they have the same name and
    // originate from the same CD
    val symbolEquality = (s: OMSymbol) => s.name == function.name && s.cd == function.cd

    // unregister the handler
    val symbol = listOfSymbols.find(symbolEquality).get
    MitMServer.server.unregister(symbol)

    // remove the function from the database
    MitMDatabase.mapOfServersToFunctions += (serverID -> listOfSymbols.filter(symbolEquality))
    scscp1(scscp1.procedureCompleted)
  }
}

/**
  * Registers the server in the MitM proxy and returns the id of the server.
  * The function takes one compulsory parameter- the address of the server, as a string, and one
  * optional parameter- the port at which the scscp server is running. If this is not given,
  * the default value of 26133 is used.
  */
class RegisterServerHandler() extends SCSCPHandler {
  override val min : Int = 1
  override val max : Int = 2
  // i.e. the signature is address: string, port: integer
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None), OMSymbol("integer", "omptypes", None, None)))

  def handle(client: SCSCPServerClient, arguments : SCSCPCallArguments, parameters: OMExpression* ) : OMExpression = {
    val listOfArgs = parameters.toList
    val address = getArgAsString(listOfArgs, 0)
    if (listOfArgs.length > 1) {
      val port = getArgAsInt(parameters.toList, 1)
      MitMDatabase.addServer(address.text, port.int.toInt)
    }
    else {
      MitMDatabase.addServer(address.text, MitMServer.defaultPort)
    }
  }
}

/**
  * Removes the server from the list of servers MitM communicates with and removes all its functions
  * from the list of symbols MitM exposes.
  * The function takes one compulsory parameter- the name (handler) of the service.
  */
class RemoveServerHandler() extends SCSCPHandler {
  override val min: Int = 1
  override val max: Int = 1
  override val signature = SymbolSet(List(OMSymbol("string", "omtypes", None, None)))
  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val server = getArgAsString(parameters.toList, 0)
    MitMDatabase.removeServer(server.text)
  }
}

/**
  * Retrieves the list of all service names (handlers) that the MitM server is currently aware of.
  */
class GetAllServersHandler() extends SCSCPHandler {
  override val min: Int = 0
  override val max: Int = 0
  override val signature = SymbolSet(Nil)

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    var listOfServers : List[OMString] = List()
    for (server <- MitMDatabase.mapOfServerNamesToClients.keys) {
      listOfServers ::= OMString(server, None)
    }
    SymbolSet(listOfServers)
  }
}

/**
  * Routes a request for one of the symbols of an scscp service to the scscp service that provides it.
  * @param CAS the scscp CAS service from which the symbol originates.
  * @param minArgs the minimum number of arguments the symbol takes.
  * @param maxArgs the maximum number of arguments the symbol takes.
  * @param sig the signature of the arguments of the symbol.
  * @param symbol the symbol that is being requested.
  */
class RemoteCallHandler(CAS: SCSCPClient, minArgs: Int, maxArgs: Int, sig: OMApplication, symbol: OMSymbol) extends SCSCPHandler {
  override val min: Int = minArgs
  override val max: Int = maxArgs
  override val signature: OMApplication = sig

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression =
    CAS(new SCSCPCall(symbol, SCSCPCallArguments(CAS.newCallId, Some(SCSCPReturnObject), null), parameters: _*)).fetch().get
}

/**
  * Binds a function on a remote CAS to a symbol in a CD that MitM is aware of.
  * The function takes three parameters:
  * - server name (handler)
  * - function (symbol) exposed by the server
  * - name of the symbol from a global CD that the function should be bound to
  * - name of the CD containing the symbol
  */
class RegisterFunctionHandler() extends SCSCPHandler {
  override val min: Int = 4
  override val max: Int = 4
  override val signature: OMApplication =
    SymbolSet(List(
      OMSymbol("string", "omtypes", None, None),
      OMSymbol("symtype", "omtypes", None, None),
      OMSymbol("string", "omtypes", None, None),
      OMSymbol("string", "omtypes", None, None)
    ))

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val serverID = getArgAsString(parameters.toList, 0).text
    val function = getArgAsSymbol(parameters.toList, 1)
    val symbolName = getArgAsString(parameters.toList, 2).text
    val CD = getArgAsString(parameters.toList, 3).text

    MitMDatabase.addFunction(serverID, function, OMSymbol(symbolName, CD, None, None))
  }
}

/**
  * Stops MitM from routing requests to the given function of the given server.
  * The fucntion takes two arguments:
  * - server name (handler)
  * - function (symbol) exposed by the server.
  */
class RemoveFunctionHandler() extends SCSCPHandler {
  override val min: Int = 2
  override val max: Int = 2
  override val signature: OMApplication =
    SymbolSet(List(
      OMSymbol("string", "omtypes", None, None),
      OMSymbol("symtype", "omtypes", None, None)
    ))

  override def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val serverID = getArgAsString(parameters.toList, 0).text
    val symbol = getArgAsSymbol(parameters.toList, 1)
    MitMDatabase.removeFunction(serverID, symbol)
  }
}