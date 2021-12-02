with Ada.Command_Line;
with shared;

procedure part_two is
    use Ada.Command_Line;
    use shared;

    commands : CommandVec := read_file(Argument(1));
    depth : Integer := 0;
    aim : Integer := 0;
    horizontal_pos : Integer := 0;
begin
    for cmd of commands loop
        case cmd.direction is
            when Forward =>
                horizontal_pos := horizontal_pos + cmd.distance;
                depth := depth + (aim * cmd.distance);
            when Up =>
                aim := aim - cmd.distance;
            when Down =>
                aim := aim + cmd.distance;
        end case;
    end loop;

    put_satanized_number_line(horizontal_pos * depth);
end part_two;
