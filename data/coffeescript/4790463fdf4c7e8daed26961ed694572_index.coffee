KNode = require('./node')

KRPC  =
    Emulator:  require('./rpc/emulator')
    Interface: require('./rpc/interface')
    JSONUDP:   require('./rpc/jsonudp')

KStorage =
    Memory: require('./storage/memory')

exports.createNode = ( args... ) ->
    return KNode args...

exports.RPC =
    Emulator:  KRPC.Emulator
    Interface: KRPC.Interface.RPC
    JSONUDP:   KRPC.JSONUDP

exports.Storage =
    Memory: KStorage.Memory

exports.errors =
    NoPeersError:    KNode.NoPeersError
    NotFoundError:   KNode.NotFoundError
    RPCTimeoutError: KRPC.Interface.RPCTimeoutError
