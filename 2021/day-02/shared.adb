with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body shared is
    use Ada.Text_IO;

    function read_file(path : String) return CommandVec is
        use Ada.Strings.Unbounded;

        commands : CommandVec;
        infile : File_Type;
    begin
        open(infile, mode => in_file, name => path);

        while not end_of_file(infile) loop
            declare
                line : Unbounded_String;
                idx : Natural;
                left : Unbounded_String;
                right : Unbounded_String;
                direction : Directions;
                distance : Natural;
                cmd : Command;
            begin
                line := To_Unbounded_String(Get_Line(infile));
                idx := Index(line, " ", 1);
                left := Unbounded_Slice(line, 1, idx - 1);
                right := Unbounded_Slice(line, idx + 1, Length(line));
                direction := Directions'Value(To_String(left));
                distance := Natural'Value(To_String(right));
                cmd := (direction, distance);
                commands.append(cmd);
            end;
        end loop;

        return commands;
    end read_file;

    procedure put_satanized_number_line(val : in Natural) is
        img : String :=  Natural'Image(val);
    begin
        put_line(img(img'First + 1 .. img'Last));
    end put_satanized_number_line;
end shared;
