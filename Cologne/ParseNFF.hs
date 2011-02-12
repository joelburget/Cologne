module Cologne.ParseNFF (parseNFF) where

import Numeric
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as PT
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Control.Applicative hiding ((<|>), many)
import Control.Monad (ap)

import Cologne.Vec
import Cologne.Primitives
import qualified Cologne.Primitives.Sphere as S

{- Parser for NFF (Neutral File Format). This is the first scene description
 - language Cologne supports. The language is not very expressive. It's like
 - training wheels, really.
 -
 - NOTE: We're using a modified version of nff, more suited to our ray tracer.
 - We may decide to implement the exact spec in the future.
 -
 - TODO: Expressions would be very helpful
 -       Comments
 -
 - More info on NFF:
 - http://tog.acm.org/resources/Sfloat/NFF.TXT
 -}

{- Let's make an instance of Applicative for GenParser like they do in Real
 - World Haskell:
 - http://book.realworldhaskell.org/read/using-parsec.html
 -}
instance Applicative (GenParser s a) where
  pure  = return
  (<*>) = ap

type ColorInfo = (ColorD, ColorD, ReflectionType)
type ObjParser a = GenParser Char ColorInfo a

{-
 - Use some predefined helpers from parsec for ints and floats. The only
 - interesting bit is that their float parser doesn't like negatives so we have
 - to be a bit more careful there.
 -}
lexer :: PT.TokenParser st
lexer  = PT.makeTokenParser emptyDef  

int = PT.integer lexer
uFloat = PT.float lexer
float = try (negate <$ char '-' <*> uFloat)
   <|>  try uFloat 
   <|>  fromIntegral <$> int

{- light
 - format l %g %g %g [%g %g %g]
 -          X  Y  Z   R  G  B
 -
 - e.g. l 3 7 8 0.9 0.8 0.7
 -      l 3 7 8
 -
 - This represents a point light source, which we just represent as a tiny
 - sphere with emission.
 -
 - First strip off the 'l' then build the light in an applicative manner. <$>
 - just lifts light to the functor level then we apply it to the parsed
 - doubles.
 -}
light :: ObjParser (Primitive ColorInfo)
light = 
  char 'l' *> spaces *>
  (light' <$> float <*> float <*> float <*> float <*> float <*> float) 
  <* spaces
  where light' x y z r g b = S.sphere (VecD x y z) 
                                      0.0000001 
                                      ((VecD 0 0 0), (VecD r g b), Diffuse)

{- sphere
 - format: s %g       %g       %g       %g
 -           center.x center.y center.z radius
 -
 - A difficulty here is that this format doesn't include the color information
 - with the primitives. They're supposed to take on whatever color most
 - recently preceded them, thus we must keep the previous color as state.
 -
 - One potentially tricky thing here is that 'spaces' reads in the end-of-line
 - characters of well. This is because it's defined using isSpace from
 - Data.Char, which returns true on ' ', '\t', '\n', and '\r', and possibly
 - others.
 -}

sphere :: ObjParser (Primitive ColorInfo)
sphere = 
  char 's' *> spaces *> 
    (sph <$> float <*> float <*> float <*> float <*> getState) 
    <* spaces
  where sph x y z r c = S.sphere (VecD x y z) r c

{- color
 - format f %g %g %g %g %g %g %s
 -          R  G  B  R  G  B  spec|diff|refr
 -
 - All of the floats should be between 0 and 1. The first triple represents the
 - color and the second represents the color of the emitted light, if any.
 -
 - I'm sticking to this format for now, since the way it's defined in the nff
 - spec doesn't suit us very well.
 -}
color :: ObjParser ()
color = do
  info <- (char 'f' *> spaces *> 
    (clr <$> float <*> float <*> float <*> float <*> float <*> float <*> reflT) 
    <* spaces)
  setState info
  where clr r1 g1 b1 r2 g2 b2 r = (VecD r1 g1 b1, VecD r2 g2 b2, r)
        reflT = spaces *> (Refractive <$ string "refr"
                       <|> Specular   <$ string "spec"
                       <|> Diffuse    <$ string "diff"
                       <?> "reflection type")

background = char 'b' *> (VecD <$> float <*> float <*> float) <* spaces
--background = char 'b' *> (VecD <$> count 3 float)
--background = char 'b' *> (VecD <$> (chainr1 float (<*>)))
--nToken 1 comb1 comb2 = comb1 <$> comb2
--nToken n comb1 comb2 = (nToken (n-1) comb1 comb2) <*> comb2

{- viewpoint
 -
 - Where the camera is located and where it points. We expect
 - it at the beginning of the file.
 -}
viewpoint :: ObjParser (VecD, VecD)
viewpoint = do
  char 'v' >> spaces
  from <- string "from " *> (VecD <$> float <*> float <*> float) <* spaces
  at <- string "at " *> (VecD <$> float <*> float <*> float) <* spaces
  return (from, norm at)
  -- If we were complying fully to the nff spec we would implement these but we
  -- have no need for them yet
  -- up <- string "up " *> (VecD <$> float <*> float <*> float) <* spaces
  -- angle <- string "angle " *> float <* spaces
  -- hither <- string "hither " *> float
  -- return (from, at, up, angle, hither)

file = do
  cam <- viewpoint
  objs <- many $ (skipMany color) *> objects
  eof
  return (cam, objs)
  where objects = sphere <|> light

parseNFF :: String -> String -> 
            Either ParseError ((VecD, VecD), [Primitive ColorInfo])
parseNFF input filename = runParser file noState filename input
  where noState = ((VecD 0 0 0), (VecD 0 0 0), (Diffuse))
