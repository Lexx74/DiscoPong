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
      Id : String (1 .. 12) := Unique_Id;
      -- End Of Message is the nul character
      EOM : String (1 .. 1) := (others => Character'Val(0));
      Foreign_Id : String (1 .. 12);
      In_Buf : aliased Message (Physical_Size => 1024);
      Limit : Integer := 100;
      Limit_Count : Integer := 0;
      Player_No : Integer := 1;

      Ack : Boolean := False;
   begin
      Set_Terminator (In_Buf, To => Character'Val(0));
      Get (COM, In_Buf'Unchecked_Access);
      while not In_Buf.Is_Reception_Complete and then Limit_Count < Limit loop
         Send_Message (Id);
         Limit_Count := Limit_Count + 1;
         delay 1.0;
      end loop;

      -- Raise exception if timeout limit
      if Limit <= Limit_Count then
         raise Connection_Timeout;
      end if;

      -- Copy the received id to variable
      for I in Foreign_Id'Range loop
         Foreign_Id (I) := In_Buf.Content_At(I);
      end loop;

      -- Compare Ids and determine players
      for I in 1 .. 12 loop
         if Id (I) < Foreign_Id (I) then
            Player_No := 1;
         else
            Player_No := 2;
         end if;
      end loop;

      -- Now wait for the other board's ACK
      while not Ack loop
         if not COM.Receiving then
            Get (COM, In_Buf'Unchecked_Access);
         end if;
         Send_Message (Id);
         if In_Buf.Is_Reception_Complete then
            Ack := In_Buf.Content_At (1) = 'A' and then In_Buf.Content_At (2) = 'C'
               and then In_Buf.Content_At (3) = 'K';
         end if;
      end loop;
      return Player_No;
   end Determine_Player_Number;
end Communication;
