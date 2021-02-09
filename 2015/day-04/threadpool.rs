use std::{
    thread,
    sync::{Arc, Mutex, mpsc},
};

pub trait Task<O>
    where O: Send + 'static
{
    fn process(&self) -> O;
}

impl<F, O> Task<O> for F
    where F: Fn() -> O,
          F: Send + 'static,
          O: Send + 'static
{
    fn process(&self) -> O {
        self()
    }
}

type BoxTask<O> = Box<dyn Task<O> + Send + 'static>;
type TaskReceiver<O> = Arc<Mutex<mpsc::Receiver<BoxTask<O>>>>;
type TaskSender<O> = mpsc::Sender<BoxTask<O>>;
type OutputSender<O> = Arc<Mutex<mpsc::Sender<O>>>;
type OutputReceiver<O> = mpsc::Receiver<O>;

#[derive(Clone)]
struct Shared<O>
    where O: Send + Clone + 'static
{
    incoming: TaskReceiver<O>,
    output: OutputSender<O>,
}

pub struct ThreadPool<O>
    where O: Send + Clone + 'static
{
    workers: Vec<thread::JoinHandle<()>>,
    shared: Shared<O>,
    tasks: TaskSender<O>,
    results: OutputReceiver<O>,
}

impl<O> ThreadPool<O>
    where O: Send + Clone + 'static
{
    pub fn with_threads(size: usize) -> Self {
        let (tasks, incoming) = mpsc::channel();
        let (output, results) = mpsc::channel();
        let shared = Shared {
            incoming: Arc::new(Mutex::new(incoming)),
            output: Arc::new(Mutex::new(output)),
        };
        let mut pool = Self {
            workers: Vec::new(),
            shared,
            tasks,
            results
        };

        for worker_id in 0..size {
            pool.spawn_worker(worker_id);
        }

        pool
    }

    pub fn add_task(&self, task: impl Task<O> + Send + 'static) {
        self.tasks.send(Box::new(task)).expect("Failed to enqueue task");
    }

    pub fn next_result(&self) -> Result<Option<O>, mpsc::TryRecvError> {
        match self.results.try_recv() {
            Ok(output) => Ok(Some(output)),
            Err(mpsc::TryRecvError::Empty) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn spawn_worker(&mut self, worker_id: usize) {
        let shared = self.shared.clone();
        let worker = thread::Builder::new()
            .name(format!("worker-{}", worker_id))
            .spawn(move || {
                loop {
                    let task = {
                        let incoming = shared.incoming.lock().expect("Lock was poisoned");
                        match incoming.recv() {
                            Ok(task) => task,
                            Err(_) => break,
                        }
                    };

                    let output = task.process();

                    let output_sender = shared.output.lock().expect("Lock was poisoned");
                    if let Err(_) = output_sender.send(output) {
                        break;
                    }
                }
            })
            .expect("Failed to spawn thread pool worker");

        self.workers.push(worker);
    }
}
