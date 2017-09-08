module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, getPageX, getPageY, body, closest)

import OutWatch.Helpers.Helpers

import Math
import Global.Unsafe

import Data.Int
import Data.Traversable (for)
import Data.Array
import Data.Maybe
import Data.Array.ST
import Data.Foreign (Foreign)

import Graphics.Canvas
import Graphics.Drawing(render)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref

import DOM.RequestAnimationFrame
import DOM (DOM)

drawLine sx sy ex ey ctx =  strokePath ctx $ do
  _ <- setStrokeStyle "#0000FF" ctx
  _ <- moveTo ctx sx sy
  _ <- lineTo ctx ex ey
  _ <- closePath ctx
  r <- emptySTArray
  void $ pushSTArray r 1

rotateX angle x y z = do
  let rad = angle * pi / 180.0
  let cosa = cos rad
  let sina = sin rad
  let yy = y * cosa - z * sina
  let zz = y * sina + z * cosa
  [x,yy,zz]

rotateY angle x y z = do
  let rad = angle * pi / 180.0
  let cosa = cos rad
  let sina = sin rad
  let xx = z * sina + x * cosa
  let zz = z * cosa - x * sina
  [xx,y,zz]

drawCube st_nodes st_edges ctx = void $ forE 0 12 $ \i ->  do

    ----------------Current edge
    i_edge_temp <- peekSTArray st_edges i    -- Read the value at the specified index in a mutable array.
    let i_edge = fromMaybe [] i_edge_temp --Converts  maybe to array

    let i_edge_s = i_edge !! 0  --Read the value at index returns Maybe
    let i_edge_e = i_edge !! 1
    let e_s = fromMaybe 0 i_edge_s -- Converts maybe to int
    let e_e = fromMaybe 0 i_edge_e


    --------------- Start node
    s_node_temp <- peekSTArray st_nodes e_s
    let s_node = fromMaybe [] s_node_temp

    let s_x_temp = s_node !! 0
    let s_y_temp = s_node !! 1
    let s_x = fromMaybe 0.0 s_x_temp
    let s_y = fromMaybe 0.0 s_y_temp

    ----------------- End node
    e_node_temp <- peekSTArray st_nodes e_e
    let e_node = fromMaybe [] e_node_temp

    let e_x_temp = e_node !! 0
    let e_y_temp = e_node !! 1
    let e_x = fromMaybe 0.0 e_x_temp
    let e_y = fromMaybe 0.0 e_y_temp

    drawLine s_x s_y e_x e_y ctx

---- Mouse events
mouseDown input drag oldx oldy e _ =  do
  pagex <- getPageX e
  pagey <- getPageY e
  _ <- writeSTRef drag true
  _ <- writeSTRef oldx pagex
  _ <- writeSTRef oldy pagey
  r <- emptySTArray
  void $ pushSTArray r 1

mouseUp input drag e _ =  do
  _ <- writeSTRef drag false
  r <- emptySTArray
  void $ pushSTArray r 1

main = void $ unsafePartial do

  drag <- newSTRef false
  old_x <- newSTRef 0.0
  old_y <- newSTRef 0.0

  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  --  canvas_jq <- closest "canvas"

  _ <- translate { translateX: 250.0, translateY:  250.0 } ctx -- translating the canvas

  ------------------- Normal arrays
  let sz = 100.0
  let msz = -100.0
  let nodes = [
        [msz, msz, msz],
        [msz, msz, sz],
        [msz, sz, msz],
        [msz, sz, sz],
        [sz, msz, msz],
        [sz, msz, sz],
        [sz, sz, msz],
        [sz, sz, sz]
  ]
  let edges = [[0, 1],[1, 3],[3, 2],[2, 0],[4, 5],[5, 7],[7, 6],[6, 4],[0, 4],[1, 5],[2, 6],[3, 7]]

  -----------------Converting normal arrays to st_arrays
  -- #Nodes
  st_nodes <- emptySTArray
  void $ forE 0 8 $ \i ->  do
    let xx = (nodes !! i)
    let yy = fromMaybe [] xx
    void $ pushSTArray st_nodes yy
  -- #Edges
  st_edges <- emptySTArray
  void $ forE 0 12 $ \i ->  do
    let xx = (edges !! i)
    let yy = fromMaybe [] xx
    void $ pushSTArray st_edges yy

  --------------------Updating cube using renderAnimation
  let updateCube = do
        st_nodes_rotated <- emptySTArray

        void $ forE 0 8 $ \i ->  do
          mvi <- peekSTArray st_nodes i
          let vi = fromMaybe [] mvi
          let mx = vi !! 0
          let my = vi !! 1
          let mz = vi !! 2
          let x = fromMaybe 0.0 mx
          let y = fromMaybe 0.0 my
          let z = fromMaybe 0.0 mz
          let yangle = 30.0 -- Angle
          let xangle = 30.0 -- Angle
          let v = rotateX (yangle*20.0) x y z
          let mvx = v !! 0
          let mvy = v !! 1
          let mvz = v !! 2
          let vx = fromMaybe 0.0 mvx
          let vy = fromMaybe 0.0 mvy
          let vz = fromMaybe 0.0 mvz
          let vv = rotateY (20.0*xangle) vx vy vz
          void $ pushSTArray st_nodes_rotated vv
          r <- emptySTArray
          void $ pushSTArray r 1

        drawCube st_nodes_rotated st_edges ctx
        requestAnimationFrame updateCube



  --  on "mousedown" (mouseDown canvas drag old_x old_y) canvas_jq
  --  on "mouseup" (mouseUp canvas drag) canvas_jq
  updateCube
