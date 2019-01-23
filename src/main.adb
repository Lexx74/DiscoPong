------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
pragma Warnings (Off, "referenced");
with HAL.Touch_Panel;       use HAL.Touch_Panel;
with STM32.User_Button;     use STM32;
with BMP_Fonts;
with LCD_Std_Out;
with Calculus;                  use Calculus;

procedure Main
is
   LCD_Natural_Width_f : Float := Float(LCD_Natural_Width);
   LCD_Natural_Height_f : Float := Float(LCD_Natural_Height);

   BG : Bitmap_Color := (Alpha => 255, others => 0);
   Ball_Pos   : My_Vector := (Float(LCD_Natural_Width / 2), Float(15));
   Ball_Direction: My_Vector := (2.0, -2.0);
   radius : Float := 10.0;
   normalAngle : Float;
   ballAngle : Float;
   newBallAngle : Float;
   norm : Float;

   paddle_Length : Float := 40.0;
   paddle_x : Float := (LCD_Natural_Width_f - Paddle_Length) / 2.0;
   paddle_y : Float := 2.0;
begin

   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   User_Button.Initialize;

   LCD_Std_Out.Set_Font (BMP_Fonts.Font8x8);
   LCD_Std_Out.Current_Background_Color := BG;

   --  Clear LCD (set background)
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;

   LCD_Std_Out.Clear_Screen;
   Display.Update_Layer (1, Copy_Back => True);

   loop
      if User_Button.Has_Been_Pressed then
         BG := HAL.Bitmap.Dark_Orange;
      end if;

      Display.Hidden_Buffer (1).Set_Source (BG);
      Display.Hidden_Buffer (1).Fill;

      Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.Blue);
      
      Display.Hidden_Buffer (1).Fill_Circle (VectorToPoint(Ball_Pos), Integer(radius));
      Display.Hidden_Buffer (1).Draw_Line(Start => (Integer(paddle_x), Integer(paddle_y)),
                                          Stop => (Integer(paddle_x + Paddle_Length), Integer(paddle_y)),
                                          Thickness => 2);
      -- TODO: Read voltage from pin
      paddle_x := 50.0;

      Ball_Pos.x := (Ball_Pos.x + Ball_Direction.x);
      if (Ball_Pos.x + radius > LCD_Natural_Width_f) then
         Ball_Direction.X := - Ball_Direction.X;
         Ball_Pos.x := LCD_Natural_Width_f - (Ball_Pos.x + radius - LCD_Natural_Width_f) - radius;
      end if;
      if (Ball_Pos.x < radius) then
         Ball_Direction.X := - Ball_Direction.X;
         Ball_Pos.x := radius + (radius - Ball_Pos.X);
      end if;
      
      Ball_Pos.y := (Ball_Pos.y + Ball_Direction.Y);
      if (Ball_Pos.y + radius > LCD_Natural_Height_f) then
         Ball_Direction.Y := - Ball_Direction.Y;
         Ball_Pos.y := LCD_Natural_Height_f - (Ball_Pos.y + radius - LCD_Natural_Height_f) - radius;
      end if;
      if (Ball_Pos.y < radius and then Ball_Direction.y < 0.0) then
         --Ball_Direction.y := - Ball_Direction.y;
         --Ball_Pos.y := radius + (radius - Ball_Pos.y);
         norm := calculateNorm(Ball_Direction);
         normalAngle := calculateNormalAngle(Integer(Ball_Pos.X));
         ballAngle := vectorToAngle(Ball_Direction);
         newBallAngle := normalAngle + (normalAngle - ballAngle);
         Ball_Direction := angleToDirection(newBallAngle);
         multVector(Ball_Direction, norm);
      end if;
      
      declare
         State : constant TP_State := Touch_Panel.Get_All_Touch_Points;
      begin
         case State'Length is
            when 1 =>
               Ball_Pos := (Float(State (State'First).X), Float(State (State'First).Y));
            when others => null;
         end case;
      end;

      --  Update screen
      Display.Update_Layer (1, Copy_Back => True);

   end loop;
end Main;
