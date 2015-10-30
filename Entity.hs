module Entity (initEntity, Entity(..)) where

-- import all OpenGL libraries qualified
import qualified Linear as L
--

data Entity = Entity
  { lasttime    :: Double
  , position    :: L.V3 Float
  , orientation :: L.Quaternion Float
  , xAngle      :: Float
  , yAngle      :: Float
  , speed       :: Float
  , mouseSpeed  :: Float
  }
  deriving (Show)

initEntity :: Entity
initEntity = Entity 0 (L.V3 0 0 10) 0 0 0 3.0 0.1
