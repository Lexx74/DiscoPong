package body Paddle is
   function Get_X (This : Paddle) return Positive is
   begin
      return This.X;
   end Get_X;

   function Get_Y (This : Paddle) return Positive is
   begin
      return This.Y;
   end Get_Y;

   function Get_Width (This : Paddle) return Positive is
   begin
      return This.Width;
   end Get_Width;

   procedure Set_X (This : in out Paddle; X : Positive) is
   begin
      This.X := X;
   end Set_X;

   procedure Set_Y (This : in out Paddle; Y : Positive) is
   begin
      This.Y := Y;
   end Set_Y;

   procedure Set_Width (This : in out Paddle; W : Positive) is
   begin
      This.Width := W;
   end Set_Width;
end Paddle;
