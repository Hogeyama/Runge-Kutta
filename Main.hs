{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Linear.V
import Linear.Vector
import Data.Vector   (Vector, toList, fromList)
import Data.List     (unfoldr)
import Data.Maybe    (fromMaybe)
import System.IO     (IOMode(..), hPutStrLn, withFile)
import Control.Monad (forM_)
import GHC.TypeLits  (KnownNat)

listToV :: KnownNat n => [a] -> V n a
listToV = fromMaybe (error "ListToV") . fromVector . fromList

data RKInstance = forall n. KnownNat n =>
    RK { f   :: Double -> V n Double -> V n Double
       , xs0 :: V n Double
       , t0  :: Double
       , t1  :: Double
       , dt  :: Double
       }

rk4 :: RKInstance -> [(Double, Vector Double)]
rk4 RK{..} = unfoldr step (t0,xs0)
  where
    step (t,xs) | t > t1    = Nothing
                | otherwise = Just ((t, toVector xs), next (t,xs))
    next (t,xs) = (t+dt, xs')
      where
        xs' = xs + dt/6 *^ (ks1 + 2 *^ ks2 + 2 *^ ks3 + ks4)
        ks1 = f t xs
        ks2 = f (t+dt/2) (xs + dt/2 *^ ks1)
        ks3 = f (t+dt/2) (xs + dt/2 *^ ks2)
        ks4 = f (t+dt  ) (xs + dt   *^ ks3)

wakusei :: RKInstance
wakusei = RK f xs0 t0 t1 dt
  where
    f :: Double -> V 4 Double -> V 4 Double
    f _ xs = listToV
           [ vx
           , vy
           , -c * x / r3
           , -c * y / r3
           ]
           where [x,y,vx,vy] = toList (toVector xs)
                 r3 = sqrt (x*x+y*y) ^ (3::Int)
    xs0 :: V 4 Double
    xs0 = listToV [1,0,0,1]
    t0 = 0
    t1 = 1
    dt = 0.01
    c  = 10 -- 中心星の質量のみに依存する定数

writeCSV :: FilePath -> [(Double, Vector Double)] -> IO ()
writeCSV f dat =
  withFile f WriteMode $ \h ->
    forM_ dat $ \(t,v) ->
      hPutStrLn h . tail . init . show $ t : toList v

main :: IO ()
main = writeCSV "out.csv" $ rk4 wakusei

