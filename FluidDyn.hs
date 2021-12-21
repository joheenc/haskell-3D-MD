import Linear.Metric
import Linear.V3
import Linear.Vector
import Control.Parallel(par)
import Control.Parallel.Strategies
import Vis
data Atom = Atom { i :: Int,
                   r :: V3 Float,
                   v :: V3 Float }

instance Show Atom where
  show (Atom i r v) = show i ++ ": r=" ++ show r ++ ", v=" ++ show v

s = 5 {- container dimension -}
rad = 0.15 {- atom radius -}
dt = 0.1 {- time step -}
chunkSize = 32 {- chunk size to use in parListChunk -}

{- position update in linear time -}
rstep :: Float -> Atom -> V3 Float -> Atom
rstep dt (Atom i r v) a = Atom i r' v
  where r' = r ^+^ (v ^* dt) ^+^ (0.5*dt**2 *^ a)

{- velocity update in linear time -}
vstep :: Float -> Atom -> V3 Float -> Atom
vstep dt atom a = Atom i r v'
  where (Atom i r v) = atom
        v' = (bound atom) * (v + (0.5 * dt) *^ a)
        bound (Atom _ (V3 x y z) _) = V3 xf yf zf
          where xf = if (abs x + rad > s/2) then (-1) else 1
                yf = if (abs y + rad > s/2) then (-1) else 1
                zf = if (abs z + rad > s/2) then (-1) else 1

{- force update in quadratic time -}
fstep :: [Atom] -> [V3 Float]
fstep atoms = fTot atoms atoms
  where fTot [a] bs = fOne a bs
        fTot (a:as) bs = fOne a bs ^+^ fTot as bs

{- helper function to calculate the total net force acting on a single atom -}
fOne :: Atom -> [Atom] -> [V3 Float]
fOne atom = fmap (f atom)
  where f a b = if (i a==i b) then 0 else lennardJones a b
        lennardJones a b = (1 / (norm d)^14 - 0.5 / (norm d)^8) *^ d
          where d = (r b) ^-^ (r a)

{- velocity Verlet algorithm -}
step :: Float -> [Atom] -> [Atom]
step dt atoms = zipWith (vstep dt) r' f'
  where f = fstep atoms `using` parListChunk chunkSize rdeepseq
        r' = zipWith (rstep dt) atoms f
        f' = f ^+^ (fstep r' `using` parListChunk chunkSize rdeepseq)

main :: IO ()
main = mainAnim

{- run the program with animation enabled -}
mainAnim = simulate options refreshRate initConfig draw update
  where options =
          ( defaultOpts
            { optWindowName = "FluidDyn",
              optBackgroundColor = Just white,
              optWindowSize = Just (1280, 720)
            }
          )
        refreshRate = 0.02
        initConfig = grid 4
        draw config = VisObjects $ [box] ++ (drawAtom <$> config `using` parListChunk chunkSize rseq)
          where box = Trans (V3 0 0 0) $ Box (s, s, s) Wireframe black
                drawAtom atom = Trans (r atom) $ Sphere rad Solid blue
        update _ config = step dt config

{- run the program with no animation -}
mainNoAnim = runSim 1000 (grid 8)
  where runSim :: Int -> [Atom] -> IO ()
        runSim 0 model' = do
          return ()
        runSim n model = do
          let model' = step (1.0/60.0) model
          runSim (n-1) model'

{- initialize a n x n x n cubical grid as the initial atom configuration -}
grid :: Int -> [Atom]
grid n = zipWith3 Atom [1..(n^3)] (cube n n) (replicate (n^3) (V3 0 0 0))
  where cube _ 0 = []
        cube d i = square d d z ++ cube d (i-1)
          where z = s/2 - (fromIntegral i * s/fromIntegral (d+1))

square :: Int -> Int -> Float -> [V3 Float]
square _ 0 _ = []
square d i z = row d d y ++ square d (i-1) z
  where y = s/2 - (fromIntegral i * s/fromIntegral (d+1))
        row _ 0 _ = []
        row d i y = V3 x y z : row d (i-1) y
          where x = s/2 - (fromIntegral i * s/fromIntegral (d+1))
