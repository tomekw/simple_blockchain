with Ada.Text_IO; use Ada.Text_IO;

with Simple_Blockchain.Block;
with Simple_Blockchain.Blockchain;

use Simple_Blockchain;

function Simple_Blockchain_Demo return Integer is
   The_Blockchain : Blockchain.Object := Blockchain.Make (Difficulty => 6);
begin
   Put_Line ("Simple blockchain demo");
   New_Line;

   Put_Line ("Mining first block...");
   Blockchain.Mine_Block (The_Blockchain, Data => "First block");
   Put_Line ("Block mined.");
   New_Line;

   Put_Line ("Mining second block...");
   Blockchain.Mine_Block (The_Blockchain, Data => "Second block");
   Put_Line ("Block mined.");
   New_Line;

   Put_Line ("Mining third block...");
   Blockchain.Mine_Block (The_Blockchain, Data => "Third block");
   Put_Line ("Block mined.");
   New_Line;

   Put_Line ("Is blockchain valid? " & Blockchain.Is_Valid (The_Blockchain)'Image);
   New_Line;

   Put_Line ("Printing blockchain...");
   Put_Line (Blockchain.Image (The_Blockchain));

   return 0;
end Simple_Blockchain_Demo;
