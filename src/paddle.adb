package body Paddle is
   function Get_Min_X (This : Game_Paddle) return Natural is
   begin
      return This.Width / 2 + 1;
   end Get_Min_X;

   function Get_Max_X (This : Game_Paddle) return Natural is
   begin
      return (LCD_Natural_Width - 1) - This.Width / 2;
   end Get_Max_X;

   function Get_X (This : Game_Paddle) return Natural is
   begin
      return This.X;
   end Get_X;

   function Get_Y (This : Game_Paddle) return Natural is
   begin
      return This.Y;
   end Get_Y;

   function Get_Width (This : Game_Paddle) return Natural is
   begin
      return This.Width;
   end Get_Width;

   procedure Set_X (This : in out Game_Paddle; X : Natural) is
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

   procedure Update_Paddle (This : in out Game_Paddle) is
      Touches : constant TP_State := Touch_Panel.Get_All_Touch_Points;
   begin
      if Touches'Length > 0 then
         This.Set_X(Natural (Touches (Touches'First).X));
      end if;
   end Update_Paddle;
end Paddle;
