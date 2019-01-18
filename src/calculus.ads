with Ada.Numerics.Generic_Elementary_Functions; --use Ada.Numerics.Elementary_Functions;

package Calculus is

type My_Vector is record
   X : Float;
   Y : Float;
end record;

function VectorToPoint(V: My_Vector) return Point;
function calculateNormalAngle(pos_x: Integer) return Float;
function angleToDirection(angle: Float) return My_Vector;
procedure multVector(vec: in out My_Vector; factor: Float);
function calculateNorm(vec: My_Vector) return Float;
function vectorToAngle(vec: My_Vector) return Float;

end Calculus;
