with Ada.Containers.Vectors;

package shared is
    use Ada.Containers;

    type Directions is (Forward, Up, Down);

    type Command is record
        direction : Directions;
        distance  : Natural;
    end record;

    package CommandVectors is new Vectors (
        Index_Type => Natural,
        Element_Type => Command
    );

    subtype CommandVec is CommandVectors.Vector;

    function read_file(path : String) return CommandVec;

    procedure put_satanized_number_line(val : in Natural);
end shared;
