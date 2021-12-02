with Ada.Command_Line;
with shared;

procedure part_one is
    use Ada.Command_Line;
    use shared;

    commands : CommandVec := read_file(Argument(1));
    depth : Integer := 0;
    horizontal_pos : Integer := 0;
begin
    for cmd of commands loop
        case cmd.direction is
            when Forward => horizontal_pos := horizontal_pos + cmd.distance;
            when Up => depth := depth - cmd.distance;
            when Down => depth := depth + cmd.distance;
        end case;
    end loop;

    put_satanized_number_line(horizontal_pos * depth);
end part_one;
