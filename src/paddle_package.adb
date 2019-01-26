package body Paddle_Package is
   function Get_Min_X (This : Paddle) return Natural is
   begin
      return This.Width / 2 + Paddle_Default_Thickness / 2 + 1;
   end Get_Min_X;

   function Get_Max_X (This : Paddle) return Natural is
   begin
      return (LCD_Natural_Width - Paddle_Default_Thickness / 2 - 1) - This.Width / 2;
   end Get_Max_X;

   function Get_X (This : Paddle) return Natural is
   begin
      return This.X;
   end Get_X;

   function Get_Y (This : Paddle) return Natural is
   begin
      return This.Y;
   end Get_Y;

   function Get_Width (This : Paddle) return Natural is
   begin
      return This.Width;
   end Get_Width;

   function Get_Thickness (This : Paddle) return Natural is
   begin
      return This.Thickness;
   end Get_Thickness;

   procedure Set_X (This : in out Paddle; X : Natural) is
      Min_X : Natural := This.Get_Min_X;
      Max_X : Natural := This.Get_Max_X;
   begin
      if X < Min_X then
         This.X := Min_X;
      elsif X > Max_X then
         This.X := Max_X;
      else
         This.X := X;
      end if;
   end Set_X;

   procedure Update (This : in out Paddle) is
      Touches : constant TP_State := Touch_Panel.Get_All_Touch_Points;
   begin
      if Touches'Length = 1 then
         This.Set_X(Natural (Touches (Touches'First).X));
      end if;
   end Update;

   procedure Draw(This : Paddle) is
      Start_X : Natural := This.Get_X - This.Get_Width / 2;
      End_X : Natural := Start_X + This.Get_Width;
      Y : Natural := Paddle_Default_Y;
      Thickness : Natural := This.Get_Thickness;
   begin
      Display.Hidden_Buffer(1).Draw_Line(Start => (Start_X, Y),
                                         Stop  => (End_X, Y),
                                         Thickness => Thickness);
   end Draw;

end Paddle_Package;
