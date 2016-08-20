module FRP.Sodium.Vertex

import Data.IORef
import FRP.Sodium.Reactive

%access public export

VertexID : Type
VertexID = Integer

Rank : Type
Rank = Integer

Register : Type
Register = Reactive (Reactive ())

Deregister : Type
Deregister = Reactive ()

mutual
  record Source where
    constructor MkSource
    origin     : VertexRef
    register   : Maybe Register
    registered : Bool
    deregister : Maybe Deregister

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

refCount : VertexRef -> Reactive Nat
refCount vertex = do
  pure $ length $ targets !(readRef vertex)

ensureBiggerThan : VertexRef -> Rank -> Reactive Bool
ensureBiggerThan vertexRef limit = do
  vertex <- readRef vertexRef
  if rank vertex > limit || visited vertex
  then pure False
  else do
    let rank' = limit + 1
    modifyRef vertexRef (record{visited = True, rank = rank'})
    traverse_ (\t => ensureBiggerThan t rank') (targets vertex)
    modifyRef vertexRef (record{visited = False})
    pure True

mutual
  incRefCount : VertexRef -> VertexRef -> Reactive Bool
  incRefCount vertex target = do
    MkVertex _ _ rank sources _ children _ <- readRef vertex
    when (!(refCount vertex) == 0) $
      traverse_ (\s => registerSource s vertex) sources
    modifyRef vertex (record{children = vertex::children})
    ensureBiggerThan target rank
    -- totalRegistrations++

  decRefCount : VertexRef -> VertexRef -> Reactive ()
  decRefCount vertex target = do
    targetChildren <- map children $ readRef target
    -- finding index and removing with it just because the list contains io refs...
    Just matched <- findIdx (vertexID !(readRef vertex)) targetChildren | Nothing => pure ()
    modifyRef target (record{children = let (xs,ys) = splitAt matched targetChildren
                                          in (xs ++ ys)})
    sources <- map sources $ readRef vertex
    when (!(refCount vertex) == 0) $ traverse_ (\s => deregisterSource s vertex) sources
      -- totalRegistrations --

  where
    findIdx : VertexID -> List VertexRef -> Reactive (Maybe Nat)
    findIdx vid = findIdx' 0 where
      findIdx' : Nat -> List VertexRef -> Reactive (Maybe Nat)
      findIdx' _ []          = pure Nothing
      findIdx' idx (v::vs) = do
        if vid == vertexID !(readRef v)
        then pure $ Just idx
        else findIdx' (S idx) vs

  increment : VertexRef -> VertexRef -> Reactive Bool
  increment = incRefCount

  decrement : VertexRef -> VertexRef -> Reactive ()
  decrement vertex referrer = do
    decRefCount vertex referrer
    -- TODO: release/possibleRoots

  registerSource : SourceRef -> VertexRef -> Reactive ()
  registerSource source target = do
    MkSource origin register registered _ <- readRef source
    when (not registered) $
      modifyRef source (record {registered = True})
    case register of
      Just reg => do
        dereg <- reg
        modifyRef source (record {deregister = Just dereg})
      Nothing => do
        increment origin target
        modifyRef source (record {deregister = Just $ decrement origin target})

  deregisterSource : SourceRef -> VertexRef -> Reactive ()
  deregisterSource sourceRef targetRef = do
    source <- readRef sourceRef
    when (registered source) $ do
      modifyRef sourceRef (record{registered = False})
      case (deregister source) of
        Just dereg => dereg
        Nothing => pure ()

%access export

newSource : VertexRef -> Register -> Source
newSource vertex register = MkSource vertex (Just register) False Nothing

newVertex : String -> Rank -> List SourceRef -> Reactive VertexRef
newVertex name rank sources = do
  vID <- uniqueID
  let vertex = MkVertex name vID rank sources [] [] False
  newRef vertex

registerVertex : VertexRef -> VertexRef -> Reactive Bool
registerVertex = increment

deregisterVertex : VertexRef -> VertexRef -> Reactive ()
deregisterVertex vertex target = do
  decrement vertex target
  --TODO: collectCycles
