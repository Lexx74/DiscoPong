package body Game_Display is

   procedure Draw_Background (Color : Bitmap_Color) is
   begin
      Display.Hidden_Buffer(1).Set_Source(Color);
      Display.Hidden_Buffer(1).Fill;
   end Draw_Background;

   procedure Draw_Paddle(P : Paddle) is
      Start_X : Natural := P.Get_X - P.Get_Width / 2;
      End_X : Natural := Start_X + P.Get_Width;
      Y : Natural := Paddle_Default_Y;
      Thickness : Natural := P.Get_Thickness;
   begin
      Display.Hidden_Buffer(1).Draw_Line(Start => (Start_X, Y),
                                         Stop  => (End_X, Y),
                                         Thickness => Thickness);
   end Draw_Paddle;

   procedure Draw_Ball is
   begin
      -- FIXME
      null;
   end Draw_Ball;

end Game_Display;
