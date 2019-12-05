use std::cmp::max;
use std::cmp::min;
use std::fs;
use std::io::BufRead;
use std::io::BufReader;

type Coord = i32;

type Point = (Coord, Coord);

#[derive(Debug, Clone, Copy)]
enum Direction {
    U,
    D,
    L,
    R,
}

#[derive(Debug, Clone, Copy)]
struct Move {
    dir: Direction,
    size: Coord,
}

type Segment = (Point, Move);

type Line = Vec<Segment>;

fn get_move(segment: Segment) -> Move {
    let (_, mv) = segment;
    return mv;
}

fn read_input() -> (String, String) {
    let file = fs::File::open("input.txt").expect("Couldn't read input");
    let mut reader = BufReader::new(file);
    let mut line1 = String::new();
    reader.read_line(&mut line1).expect("Failed to read line");
    let mut line2 = String::new();
    reader.read_line(&mut line2).expect("Failed to read line");
    return (line1, line2);
}

fn parse_move(desc: &str) -> Result<Move, String> {
    let dir = match desc.chars().nth(0) {
        Some('U') => Ok(Direction::U),
        Some('D') => Ok(Direction::D),
        Some('L') => Ok(Direction::L),
        Some('R') => Ok(Direction::R),
        _ => Err("Couldn't match"),
    }?;

    // TODO: This panics instead of propagating the Err(..)
    let size: Coord = desc[1..].trim().parse().unwrap();
    return Ok(Move {
        dir: dir,
        size: size,
    });
}

fn add_corners(moves: &Vec<Move>) -> Line {
    let mut x = 0;
    let mut y = 0;
    let mut vec = Vec::new();
    for &mv in moves {
        vec.push(((x, y), mv));
        match mv.dir {
            Direction::D => y = y - mv.size,
            Direction::U => y = y + mv.size,
            Direction::L => x = x - mv.size,
            Direction::R => x = x + mv.size,
        };
    }
    return vec;
}

fn advance(s: Segment) -> (Point, Point) {
    let ((x, y), mv) = s;
    let p = match mv.dir {
        Direction::D => (x, y - mv.size),
        Direction::U => (x, y + mv.size),
        Direction::L => (x - mv.size, y),
        Direction::R => (x + mv.size, y),
    };
    return ((x, y), p);
}

fn intersects(s1: Segment, s2: Segment) -> Option<(Point, Coord)> {
    let ((x1, y1), (x2, y2)) = advance(s1);
    let ((u1, v1), (u2, v2)) = advance(s2);

    if x1 == x2
        && v1 == v2
        && min(u1, u2) <= x1
        && x1 <= max(u1, u2)
        && min(y1, y2) <= v1
        && v1 <= max(y1, y2)
    {
        let p = (x1, v1);
        let steps = (x1 - u1).abs() + (v1 - y1).abs();

        Some((p, steps))
    } else if u1 == u2
        && y1 == y2
        && min(x1, x2) <= u1
        && u1 <= max(x1, x2)
        && min(v1, v2) <= y1
        && y1 <= max(v1, v2)
    {
        let p = (u1, y1);
        let steps = (u1 - x1).abs() + (v1 - y1).abs();

        Some((p, steps))
    } else {
        None
    }
}

fn find_intersections(line1: &Line, line2: &Line) -> Vec<(Coord, Point)> {
    let mut intersections = Vec::new();
    let mut steps1 = 0;
    for &s1 in line1 {
        let mut steps2 = 0;
        for &s2 in line2 {
            match intersects(s1, s2) {
                Some((point, steps)) => {
                    if point != (0, 0) {
                        intersections.push((steps1 + steps2 + steps, point))
                    }
                }
                None => (),
            }
            steps2 += get_move(s2).size;
        }
        steps1 += get_move(s1).size;
    }
    return intersections;
}

fn parse_line(line: &str) -> Vec<Move> {
    return line.split(",").map(|s| parse_move(s).unwrap()).collect();
}

fn min_manhattan_distance(points: &Vec<(Coord, Point)>) -> Option<Coord> {
    let mut m = None;
    for (_, (x, y)) in points {
        let d = x.abs() + y.abs();
        match m {
            None => m = Some(d),
            Some(m_) => {
                if d < m_ {
                    m = Some(d)
                }
            }
        }
    }
    return m;
}

fn min_steps(points: &Vec<(Coord, Point)>) -> Option<Coord> {
    let mut m = None;
    for &(s, _) in points {
        match m {
            None => m = Some(s),
            Some(m_) => {
                if s < m_ {
                    m = Some(s)
                }
            }
        }
    }
    return m;
}

fn closest_intersection(cs1: &str, cs2: &str) -> Option<Coord> {
    let line1 = add_corners(&parse_line(&cs1));
    let line2 = add_corners(&parse_line(&cs2));
    let intersections = find_intersections(&line1, &line2);
    return min_manhattan_distance(&intersections);
}

fn cheapest_intersection(cs1: &str, cs2: &str) -> Option<Coord> {
    let line1 = add_corners(&parse_line(&cs1));
    let line2 = add_corners(&parse_line(&cs2));
    let intersections = find_intersections(&line1, &line2);
    return min_steps(&intersections);
}

fn print_intersections(cs1: &str, cs2: &str) {
    let line1 = add_corners(&parse_line(&cs1));
    let line2 = add_corners(&parse_line(&cs2));
    println!("{:?}", line1);
    println!("{:?}", line2);
    let intersections = find_intersections(&line1, &line2);
    println!("{:?}", intersections);
}

fn compute_3a() -> Option<Coord> {
    let (cs1, cs2) = read_input();
    return closest_intersection(&cs1, &cs2);
}

fn compute_3b() -> Option<Coord> {
    let (cs1, cs2) = read_input();
    return cheapest_intersection(&cs1, &cs2);
}

fn main() {
    println!("Answer 3a: {:?}", compute_3a());
    println!("Answer 3b: {:?}", compute_3b());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_case_3a_1() {
        let actual = closest_intersection(
            "U62,R66,U55,R34,D71,R55,D58,R83",
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
        );
        assert_eq!(Some(159), actual);
    }

    #[test]
    fn test_case_3a_2() {
        let actual = closest_intersection(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        );
        assert_eq!(Some(135), actual);
    }

    #[test]
    fn test_case_3a_3() {
        let actual = closest_intersection("R8,U5,L5,D3", "U7,R6,D4,L4");
        assert_eq!(Some(6), actual);
    }

    #[test]
    fn test_case_3b_1() {
        let actual = cheapest_intersection(
            "U62,R66,U55,R34,D71,R55,D58,R83",
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
        );
        assert_eq!(Some(610), actual);
    }

    #[test]
    fn test_case_3b_2() {
        let actual = cheapest_intersection(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        );
        assert_eq!(Some(410), actual);
    }

    #[test]
    fn test_case_3b_3() {
        let actual = cheapest_intersection("R8,U5,L5,D3", "U7,R6,D4,L4");
        assert_eq!(Some(30), actual);
    }
}
