package body Paddle is
   function Get_X (This : Game_Paddle) return Positive is
   begin
      return This.X;
   end Get_X;

   function Get_Y (This : Game_Paddle) return Positive is
   begin
      return This.Y;
   end Get_Y;

   function Get_Width (This : Game_Paddle) return Positive is
   begin
      return This.Width;
   end Get_Width;

   procedure Set_X (This : in out Game_Paddle; X : Positive) is
   begin
      This.X := X;
   end Set_X;

   procedure Set_Y (This : in out Game_Paddle; Y : Positive) is
   begin
      This.Y := Y;
   end Set_Y;

   procedure Set_Width (This : in out Game_Paddle; W : Positive) is
   begin
      This.Width := W;
   end Set_Width;

   procedure Update_Paddle (This : in out Game_Paddle) is
      Touches : constant TP_State := Touch_Panel.Get_All_Touch_Points;
   begin
      if Touches'Length > 0 then
         Set_X (This, Positive (Touches (Touches'First).X));
      end if;
   end Update_Paddle;
end Paddle;
