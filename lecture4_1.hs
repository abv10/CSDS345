--make type scale be an alias for Double
type Scale = Double

doubleScale:: Scale -> Scale
doubleScale x = 2 * x

{- To create a new type we use the key word "data"
    Coordinate is the name of the type, Coord2D and Coord#D are constructors for the type}
    "deriving (Show)" means we place Coordiante in the show class of types and use default behaviors
-}
--data Coordinate = Coord2D double double | Coord3D double double double deriving (Show, Eq)

data Coordinate = Zero | Coord1D t | Coord2D t t | Coord3D t t t deriving (Show)
{-- let x = Coord2D 4.0 5.0} -}
--define == on Coordiantes so two coordinates are equal if their distance is zero (get rid of deriving Eq first)


instance (Floating t, Eq t) => Eq(Coordinate t) where
    a == b = distance a b == 0

distance c1 c2 = sqrt(
    (getX c1 - getx c2)*(getX c1 - getx c2)
    +(getY c1 - getY c2)*(getY c1 - getY c2)
    +(getZ c1 - getZ c2)*(getZ c1 - getZ c2)
    )


--Create Helper "getter" functions Functions
getX (Coord3D x y z) = x
getX (Coord2D x y) = x
getX (Coord1D x) = x
getX Zero = 0

getY (Coord3D x y z) = y
getY (Coord2D x y) = y
getY (Coord1D x) = 0
getY Zero = 0

getZ (Coord3D x y z) = z
getZ (Coord2D x y) = 0
getZ (Coord1D x) = 0
getZ Zero = 0

--we can create infix operators (functions that are symbols)
(##) a b = distance a b
--now you can say Zero ## (Coord3D 1 2 3)

{- Your Task: Create a addition operator -|- that adds two coordiantes. The result the large of two inputs. Then override the + operator so it uses the coordinate addition that we wrote
 The + operator is in the Num class-}
 
 plus (Coord3D x y z) b = Coord3D (x + getX b) (y + getY b) (z + getZ b)
 plus b (Coord3D x y z) = Coord3D (x + getX b) (y + getY b) (z + getZ b)
 plus (Coord2D x y) b = Coord2D (x + getX b) (y + getY b)
 plus b (Coord2D x y) = Coord2D (x + getX b) (y + getY b)
 plus (Coord1D x) b = Coord1D (x + getX b)
 plus b (Coord1D x)  = Coord1D (x + getX b)
 plus Zero Zero = Zero

    
 (-|-) a b = plus a b

 instance (Floating t) => Num(Coordinate t) where
    a + b = a -|- b