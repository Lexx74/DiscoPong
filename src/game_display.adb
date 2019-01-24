package body Game_Display is

   procedure Draw_Background (Color : Bitmap_Color) is
   begin
      Display.Hidden_Buffer(1).Set_Source(Color);
      Display.Hidden_Buffer(1).Fill;
   end Draw_Background;

   procedure Draw_Paddle(P : Game_Paddle) is
      Start_X : Natural := P.Get_X - Get_Width (P) / 2;
      End_X : Natural := Start_X + Get_Width (P);
      Y : Natural := Paddle_Default_Y;
   begin
      Display.Hidden_Buffer(1).Draw_Line(Start => (Start_X, Y),
                                         Stop  => (End_X, Y),
                                         Thickness => 6);
   end Draw_Paddle;

   procedure Draw_Ball is
   begin
      -- FIXME
      null;
   end Draw_Ball;

end Game_Display;
