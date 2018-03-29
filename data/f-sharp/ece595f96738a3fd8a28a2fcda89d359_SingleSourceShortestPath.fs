module SingleSourceShortestPath
open System.Collections.Generic
open Types
open Util
open Util.TupleArrow

let init (g : Graph<'k,unit,int>) source =
  let g' = annotate g {distance=System.Int32.MaxValue; parent=None}
  g'.[source].meta <- { g'.[source].meta with distance=0 }
  g'
let relax (g : Graph<'k,SingleSource<'k>,int>) (w,(u,v)) =
  let distance' = g.[u].meta.distance + w
  if g.[u].meta.distance <> System.Int32.MaxValue && g.[v].meta.distance > distance' 
  then g.[v].meta <- {g.[v].meta with distance=distance'; parent=Some u}
  else ()
let bellmanFord (g : Graph<'k,unit,int>) source =
  let g' = init g source
  for i = 0 to g.Count - 2 do
    Seq.iter (relax g') (edges g')
  if edges g' |> Seq.exists (fun (w,(u,v)) -> g'.[v].meta.distance > g'.[u].meta.distance + w)
  then None
  else Some g'
let dagShortestPaths (g : Graph<'k,unit,int>) source =
  let g' = init g source
  for u in Basic.topologicalSort g do
    for v in g'.[u].edges do
      relax g' (v.meta,(u,v.edge))
  g'
let dijkstra (g : Graph<'k,unit,int>) source =
  let g' = init g source
  // I have to use MinQueueSimple here for now because relax changes the keys
  // that the heap-based min-q uses to heapify, without calling DecreaseKey as required,
  // because it does not know where in the min-q the vertex pointer is stored.

  // there are a couple of ways to solve this:
  // a simple-ish one is to make MinQueueX, where X has a key field that both
  // the algorithm and the MinQueue know about, and then make relax update the key field instead,
  // then call DecreaseKey. I have a version already customised for Prim's minimum spanning tree.
  // this is annoying and not generic--it contaminates both the min-q and Dijkstra's algorithm.

  // another way would be to create an observer with knowledge of both the algorithm
  // and the min queue, that tracks mappings of vertices inside the queue itself
  // then it could call DecreaseKey whenever a vertex's distance from root changes.
  // this would require intimate knowledge of both pieces of code, but the two pieces of code
  // would be ignorant of the other, as well as the observer.
  let q = MinQueue.MinQueueSimple (g'.Keys, (fun v -> g'.[v].meta.distance))
  while q.Count <> 0 do
    let u = q.ExtractMin ()
    for v in g'.[u].edges do
      relax g' (v.meta,(u,v.edge))
  g'