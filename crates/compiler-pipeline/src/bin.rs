use compiler_pipeline::Pipeline;

fn main() {
    simple_logger::init().unwrap();

    let pipeline = Pipeline::new("../../tests/fibonacci_recursive.crunch");

    pipeline.run();
}
