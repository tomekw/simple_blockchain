with Ada.Calendar; use Ada.Calendar;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

generic
    Difficulty : Natural;
package Simple_Blockchain is

    -----------
    -- Block --
    -----------

    type Block is tagged private;

    function Make_Block (Previous_Hash : String; Data : String) return Block;

    ----------------
    -- Blockchain --
    ----------------

    function Is_Valid return Boolean;

    function Last_Block return Block;

    procedure Mine_Block (Data : String);

    procedure Print_Blockchain;

private
    type Block is tagged
        record
            Hash : String (1 .. 64);
            Previous_Hash : String (1 .. 64);
            Timestamp : Time;
            Nonce : Natural;
            Data : Unbounded_String;
        end record;

    function Image (This : Block) return String;

    procedure Recalculate_Hash (This : in out Block);

    function Calculate_Hash (Previous_Hash : String; Timestamp : Time; Nonce : Natural; Data : String) return String;

    package Block_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Block);
    use Block_Vectors;

    Blocks : Block_Vectors.Vector;

end Simple_Blockchain;
