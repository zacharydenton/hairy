module Hairy where
import Data.Maybe
import Data.String.Utils

type Vector = (Double, Double, Double)
type Point = Vector
type Ray = (Point, Vector)
type Frame = (Point, Vector, Vector, Vector)
type Color = Vector
type Image = (Integer, Integer, [Color])
data Shape = Sphere Frame Double
data Material = Lambert Color
type Object = (Shape, Material)
data Light = PointLight Frame Double
type Intersection = (Object, Frame)
type Camera = (Frame, Double, Double)
type Scene = [Object]

(<+>) ::  Vector -> Vector -> Vector
(x1, y1, z1) <+> (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

(<*>) ::  Vector -> Vector -> Vector
(x1, y1, z1) <*> (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

(<->) ::  Vector -> Vector -> Vector
(x1, y1, z1) <-> (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

(*>) ::  Vector -> Double -> Vector
(x, y, z) *> f = (x * f, y * f, z * f)

(/>) ::  Vector -> Double -> Vector
v /> f = v *> (1 / f)

infixl 6 <+>
infixl 6 <->
infixl 7 <*>
infixl 7 />
infixl 7 *>

dot ::  Vector -> Vector -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

cross ::  Vector -> Vector -> Vector
cross (x1, y1, z1) (x2, y2, z2) = (a, b, c)
    where a = y1 * z2 - z1 * y2
          b = z1 * x2 - x1 * z2
          c = x1 * y2 - y1 * x2

magnitude ::  Vector -> Double
magnitude v = sqrt $ dot v v

normalize ::  Vector -> Vector
normalize v | magnitude v < 10 ** (-9) = (0, 0, 0)
            | otherwise = v /> (magnitude v)

transformVector ::  Frame -> Vector -> Vector
transformVector (_, fx, fy, fz) (x, y, z) = fx *> x <+> fy *> y <+> fz *> z

transformPoint ::  Frame -> Point -> Point
transformPoint (fo, fx, fy, fz) (x, y, z) = fo <+> fx *> x <+> fy *> y <+> fz *> z

transformPointInverse ::  Frame -> Point -> Point
transformPointInverse (fo, fx, fy, fz) v = (vfo `dot` fx, vfo `dot` fy, vfo `dot` fz)
    where vfo = v <-> fo

transformRay ::  Frame -> Ray -> Ray
transformRay frame (start, direction) = (transformPoint frame start, transformVector frame direction)

evalRay ::  Ray -> Double -> Point
evalRay (start, direction) f = start <+> direction *> f

renderPPM ::  Image -> String
renderPPM (width, height, image) = join "\n" parts
    where parts = ["P3", show width, show height, show (round maxColor), join "\n" (map toTriplet image), ""]
          maxColor = 255
          toTriplet (r, g, b) = join " " (map (show . round . (* maxColor)) [r, g, b])

solveQuadratic ::  Double -> Double -> Double -> [Double]
solveQuadratic a b c | (d < 0) = []
                     | (d > 0) = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
                     | otherwise = [-b / (2 * a)]
    where d = b * b - 4 * a * c

materialBrdfCos

lightContribution ::  Frame -> Material -> Light -> Color
lightContribution frame material (PointLight point intensity) =
    let wi = 

intersectionDiffuse ::  Intersection -> Scene -> Color
intersectionDiffuse (((_, material), frame) (_, lights)) =
    sum $ map (lightContribution frame material) lights

intersectionColor ::  Intersection -> Scene -> Color
intersectionColor intersection = intersectionDiffuse intersection

sphereFrame ::  Point -> Double -> Point -> Frame
sphereFrame center radius point =
    let (dx, dy, dz) = (point <-> center) /> radius
        atan2pos x y | a >= 0 = a
                     | otherwise = 2 * pi + a
            where a = atan2 y x
        (ux, uy) = ((atan2pos dy dx) / (2 * pi), (acos dz) / pi)
        phi = 2 * pi * ux
        theta = pi * uy
        ct = cos theta
        st = sin theta
        cp = cos phi
        sp = sin phi
    in (point, (-sp, cp, 0), (ct * cp, ct * sp, -st), (st * cp, st * sp, ct))

intersect ::  Ray -> Object -> Maybe Intersection
intersect ray@(start, direction) sphere@(Sphere (origin, _, _, _) radius, _) = intersection
    where a = direction `dot` direction
          b = 2 * direction `dot` offset
          c = offset `dot` offset - radius * radius
          offset = start <-> origin
          intersectionPoints = solveQuadratic a b c
          intersection | null intersectionPoints = Nothing
                       | otherwise = Just (sphere, sphereFrame origin radius (evalRay ray (head intersectionPoints)))

intersectScene ::  Ray -> Scene -> Maybe Intersection
intersectScene ray scene = head $ map (intersect ray) scene

traceRay ::  Ray -> Scene -> Color
traceRay ray scene | isNothing intersection = (0, 0, 0)
                   | otherwise = intersectionColor $ fromJust intersection
    where intersection = intersectScene ray scene

fov ::  Double -> Double -> Double
fov lensSize focalLength = 2 * atan (0.5 * lensSize / focalLength)

cameraRay ::  Camera -> Double -> Double -> Ray
cameraRay (frame, size, focalLength) u v = transformRay frame (start, direction)
    where start = (0, 0, 0)
          cameraFov = fov size focalLength
          direction = ((2 * u - 1) * tan cameraFov, (2 * v - 1) * tan cameraFov, -1)

raytrace ::  Integer -> Integer -> Camera -> Scene -> Image 
raytrace width height camera scene = (width, height, pixels)
    where pixels = map trace uvs
          trace (u, v) = traceRay (cameraRay camera u v) scene
          coords = [(fromInteger x, fromInteger y) | y <- [1..height], x <- [1..width]]
          uv (x, y) = ((x - 0.5) / (fromInteger width), (y - 0.5) / (fromInteger height))
          uvs = map uv coords


main = putStr $ renderPPM $ raytrace width height camera scene
    where width = 200
          height = 200
          camera = (((278, 273, -800), (1, 0, 0), (0, 1, 0), (0, 0, -1)), 0.025, 0.035)
          scene = [sphere]
          sphere = (shape, material)
          shape = (Sphere ((108, 373, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)) 300)
          material = (Lambert (0.3, 0.81, 0.03))


