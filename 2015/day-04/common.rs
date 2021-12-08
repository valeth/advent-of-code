mod md5;
mod threadpool;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;
pub type ThreadPool = threadpool::ThreadPool<Vec<usize>>;

pub fn extract_result(pool: &ThreadPool) -> Option<usize> {
    let mut smallest = None;
    while let Some(result) = pool.next_result().unwrap() {
        let result = result.into_iter().min();
        if !result.is_none() && (smallest.is_none() || smallest > result) {
            smallest = result;
        }
    }
    smallest
}

pub fn find_suffix_with_zeroes(input: &str, amount: usize) -> usize {
    let pool = ThreadPool::with_threads(8);

    let mut start = 1;
    let mut tries = 8;

    let test_string = "0".repeat(amount);

    loop {
        if tries == 0 {
            match extract_result(&pool) {
                None => tries = 7,
                Some(v) => return v,
            }
        }

        let test_string = test_string.clone();
        let input = input.to_owned();
        pool.add_task(move || {
            (start..)
                .into_iter()
                .take(20_000)
                .filter_map(|i| {
                    let secret = format!("{}{}", input, i);
                    let hexdigest = md5::hexdigest(&secret);
                    if hexdigest.starts_with(&test_string) {
                        Some(i)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        });
        tries -= 1;
        start += 20_000;
    }
}
