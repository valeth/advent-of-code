mod common;

use std::env;
use std::thread;
use std::sync::atomic;
use common::{parse, Result, Tiles, Vec2};


type ArrayList<T> = Vec<T>;
type Rect = (Vec2, Vec2);


fn main() -> Result<()> {
    let infile = env::args().nth(1).expect("input file");
    let is_puzzle = infile.contains("puzzle");

    let start = std::time::Instant::now();
    let input = parse(infile)?;
    let parse_time = start.elapsed();

    let start = std::time::Instant::now();
    let solution = solve(input);
    let solve_time = start.elapsed();

    if is_puzzle {
        assert_eq!(solution, 1554370486);
    }

    println!("Part 2:");
    println!("  Time: {parse_time:.3?} (parse) + {solve_time:.3?} (solve) = {:.3?}", parse_time + solve_time);
    println!("  {solution}");

    Ok(())
}


fn solve(tiles: Tiles) -> u64 {
    let mut rects = ArrayList::new();

    for (idx1, tile1) in tiles.iter().enumerate() {
        for tile2 in tiles.iter().skip(idx1 + 1) {
            let dx = tile1.x.abs_diff(tile2.x) + 1;
            let dy = tile1.y.abs_diff(tile2.y) + 1;
            rects.push(((*tile1, *tile2), dx * dy));
        }
    }

    let max_area = atomic::AtomicU64::new(0);

    const THREADS: usize = 6;

    thread::scope(|scope| {
        for chunk in rects.chunks(rects.len() / THREADS) {
            let tiles = &tiles;
            let chunk = chunk.to_vec(); // Vec::into_chunks is not stable, so we just clone here
            scope.spawn(|| {
                for (rect, area) in chunk {
                    if max_area.load(atomic::Ordering::SeqCst) < area && rect_inside_area(rect, tiles) {
                        max_area.store(area, atomic::Ordering::SeqCst);
                    }
                }

            });
        }

    });

    max_area.into_inner()
}


// This works but is incredibly slow, probably doing a lot of redundant checks
fn rect_inside_area(rect: Rect, tiles: &Tiles) -> bool {
    let (point1, point2) = rect;

    let x_max = point1.x.max(point2.x);
    let x_min = point1.x.min(point2.x);
    let y_max = point1.y.max(point2.y);
    let y_min = point1.y.min(point2.y);

    for y in y_min..=y_max {
        if !point_inside(Vec2 { x: x_min, y }, &tiles) {
            return false;
        }

        if !point_inside(Vec2 { x: x_max, y }, &tiles) {
            return false;
        }
    }

    for x in x_min+1..x_max {
        if !point_inside(Vec2 { x, y: y_min }, &tiles) {
            return false;
        }

        if !point_inside(Vec2 { x, y: y_max }, &tiles) {
            return false;
        }
    }

    true
}


fn point_inside(Vec2 { x, y }: Vec2, tiles: &Tiles) -> bool {
    let mut intersections = 0;

    let mut t1 = tiles[0];
    for i in 1..=tiles.len() {
        let wrapping_idx = i % tiles.len();
        let t2 = tiles[wrapping_idx];

        let min_y = t1.y.min(t2.y);
        let max_y = t1.y.max(t2.y);
        let min_x = t1.x.min(t2.x);
        let max_x = t1.x.max(t2.x);

        let within_y_range = y > min_y && y <= max_y;
        let within_x_range = x >= min_x && x <= max_x;

        // Point lies on edge
        if (t1.y == t2.y && y == t1.y && within_x_range) || (t1.x == t2.x && x == t1.x && within_y_range) {
            return true;
        }

        // Would intersect with edge
        if t1.x == t2.x && within_y_range && x < max_x {
            intersections += 1;
        }

        t1 = t2;
    }

    intersections % 2 != 0
}
