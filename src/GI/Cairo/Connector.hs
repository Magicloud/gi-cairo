module GI.Cairo.Connector where

class Connector a b where
  fromGI :: a -> IO b
  toGI :: b -> IO a
