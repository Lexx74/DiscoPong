package body Communication is
   procedure Initialize_Communication is
   begin
      Initialize (COM);
      Configure (COM, Baud_Rate => 115200);
   end Initialize_Communication;

   procedure Send_Message (M : String) is
      Buf : aliased Message (Physical_Size => 1024);
   begin
      Set (Buf, To => M);
      Put (COM, Buf'Unchecked_Access);
      Await_Transmission_Complete (Buf);
   end Send_Message;

   procedure Send_Debug (M : String) is
   begin
      Send_Message ("DEBUG: " & M);
   end Send_Debug;

   function Determine_Player_Number return Integer is
   begin
      return 0;
   end Determine_Player_Number;
end Communication;
