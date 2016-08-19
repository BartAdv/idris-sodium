module FRP.Sodium.Vertex

import Data.IORef

%access public export

VertexID : Type
VertexID = Integer

Rank : Type
Rank = Integer

mutual
  record Source where
    constructor MkSource
    origin     : VertexRef
    register   : Maybe (IO (IO ()))
    registered : Bool
    deregister : Maybe (IO ())

  record Vertex where
    constructor MkVertex
    name     : String
    vertexID : VertexID
    rank     : Rank
    sources  : List SourceRef
    targets  : List VertexRef
    children : List VertexRef
    visited  : Bool

  SourceRef : Type
  SourceRef = IORef Source

  VertexRef : Type
  VertexRef = IORef Vertex

%access private

refCount : VertexRef -> IO Nat
refCount vertex = do
  pure $ length $ targets !(readIORef vertex)

ensureBiggerThan : VertexRef -> Rank -> IO Bool
ensureBiggerThan vertexRef limit = do
  vertex <- readIORef vertexRef
  if rank vertex > limit || visited vertex
  then pure False
  else do
    let rank' = limit + 1
    modifyIORef vertexRef (record{visited = True, rank = rank'})
    traverse_ (\t => ensureBiggerThan t rank') (targets vertex)
    modifyIORef vertexRef (record{visited = False})
    pure True

mutual
  incRefCount : VertexRef -> VertexRef -> IO Bool
  incRefCount vertex target = do
    MkVertex _ _ rank sources _ children _ <- readIORef vertex
    when (!(refCount vertex) == 0) $
      traverse_ (\s => registerSource s vertex) sources
    modifyIORef vertex (record{children = vertex::children})
    ensureBiggerThan target rank
    -- totalRegistrations++

  decRefCount : VertexRef -> VertexRef -> IO ()
  decRefCount vertex target = do
    targetChildren <- map children $ readIORef target
    -- finding index and removing with it just because the list contains io refs...
    Just matched <- findIdx (vertexID !(readIORef vertex)) targetChildren | Nothing => pure ()
    modifyIORef target (record{children = let (xs,ys) = splitAt matched targetChildren
                                          in (xs ++ ys)})
    sources <- map sources $ readIORef vertex
    when (!(refCount vertex) == 0) $ traverse_ (\s => deregisterSource s vertex) sources
      -- totalRegistrations --

  where
    findIdx : VertexID -> List VertexRef -> IO (Maybe Nat)
    findIdx vid = findIdx' 0 where
      findIdx' : Nat -> List VertexRef -> IO (Maybe Nat)
      findIdx' _ []          = pure Nothing
      findIdx' idx (v::vs) = do
        if vid == vertexID !(readIORef v)
        then pure $ Just idx
        else findIdx' (S idx) vs

  increment : VertexRef -> VertexRef -> IO Bool
  increment = incRefCount

  decrement : VertexRef -> VertexRef -> IO ()
  decrement vertex referrer = do
    decRefCount vertex referrer
    -- TODO: release/possibleRoots

  registerSource : SourceRef -> VertexRef -> IO ()
  registerSource source target = do
    MkSource origin register registered _ <- readIORef source
    when (not registered) $
      modifyIORef source (record {registered = True})
    case register of
      Just reg => do
        dereg <- reg
        modifyIORef source (record {deregister = Just dereg})
      Nothing => do
        increment origin target
        modifyIORef source (record {deregister = Just $ decrement origin target})

  deregisterSource : SourceRef -> VertexRef -> IO ()
  deregisterSource sourceRef targetRef = do
    source <- readIORef sourceRef
    when (registered source) $ do
      modifyIORef sourceRef (record{registered = False})
      case (deregister source) of
        Just dereg => dereg
        Nothing => pure ()

%access export

registerVertex : VertexRef -> VertexRef -> IO Bool
registerVertex = increment

deregisterVertex : VertexRef -> VertexRef -> IO ()
deregisterVertex vertex target = do
  decrement vertex target
  --TODO: collectCycles
