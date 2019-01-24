package body Ball_Package is
   procedure Draw(This : Ball) is
   begin
      Display.Hidden_Buffer (1).Fill_Circle (Vector_To_Point(This.Pos), Integer(This.Radius));
   end Draw;

   procedure Update(This : in out Ball) is
      Old_Ball : Ball := This;
   begin
     This.Pos.x := This.Pos.x + This.Direction.x;
     This.Pos.y := This.Pos.y + This.Direction.y;

      while This.Bounce(Old_Ball) loop
        null;
      end loop;
   end Update;

   function Bounce(This : in out Ball; Old_Ball : in out Ball) return Boolean is
     normalAngle : Float;
     ballAngle : Float;
     newBallAngle : Float;
     norm : Float;
   begin
      if (This.Pos.x + This.Radius > LCD_Natural_Width_f) then
         This.Direction.X := - This.Direction.X;
         This.Pos.x := LCD_Natural_Width_f - (This.Pos.x + This.Radius - LCD_Natural_Width_f) - This.Radius;
         Old_Ball.Pos.x := LCD_Natural_Width_f * 2.0 - Old_Ball.Pos.x; -- mirror x of old pos;
         return True;
      end if;
      if (This.Pos.x < This.Radius) then
         This.Direction.X := - This.Direction.X;
         This.Pos.x := This.Radius + (This.Radius - This.Pos.X);
         Old_Ball.Pos.y := -Old_Ball.Pos.y;
         return True;
      end if;
      if (This.Pos.y + This.Radius > LCD_Natural_Height_f) then
         This.Direction.Y := - This.Direction.Y;
         This.Pos.y := LCD_Natural_Height_f - (This.Pos.y + This.Radius - LCD_Natural_Height_f) - This.Radius;
         return True;
      end if;
      if (This.Pos.y < This.Radius and then This.Direction.y < 0.0) then
         declare
           ratio_before_impact : Float;
           ratio_after_impact : Float;
           Impact_X : Float;
         begin
           ratio_before_impact := -(Old_Ball.Pos.y - This.Radius) / This.Direction.y;
           ratio_after_impact := 1.0 - ratio_before_impact;
           Impact_X := Old_Ball.Pos.x + ratio_before_impact * This.Direction.x;

           norm := calculus.Calculate_Norm(This.Direction);
           normalAngle := calculus.Calculate_Normal_Angle(Integer(Impact_X));
           ballAngle := calculus.Vector_To_Angle(This.Direction);
           newBallAngle := normalAngle + (normalAngle - ballAngle);
           --newBallAngle := -- TODO: constraint to ]-180°; 180°[
           This.Direction := calculus.Angle_To_Direction(newBallAngle);
           calculus.Mult_Vector(This.Direction, norm);
           This.Pos := (Impact_X + This.Direction.x * ratio_after_impact, This.Radius + This.Direction.y * ratio_after_impact);
         end;
        return True;
      end if;
      return False;
   end bounce;
end Ball_Package;
